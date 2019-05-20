// TODO make input a slice
// TODO more tests for try_merge
// TODO support trailing text of a PoEntry after a merge
// TODO only output conflicting Entries in larger conflicts as conflict

extern crate regex;
#[macro_use]
extern crate lazy_static;

#[cfg(test)]
use pretty_assertions::assert_eq;

use regex::bytes::{Regex, Captures};
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::str::from_utf8;

#[derive(Debug)]
pub enum MyErr {
    Io(std::io::Error),
}

impl std::convert::From<std::io::Error> for MyErr {
    fn from(e: std::io::Error) -> MyErr {
        MyErr::Io(e)
    }
}

#[derive(Clone, Debug)]
enum RepeatType {
    Invalid,
    Msgctxt,
    Msgid,
    MsgidPlural,
    Msgstr,
    MsgstrPlural(u32),
}

#[derive(PartialEq, Debug)]
enum ParseResult {
    Ok,
    NextEntry,
}

fn _append_quoted_string(current_part: &[u8], complete_value: &mut Vec<u8>) {
    assert!(current_part.len() >= 2, "Invalid quoted string: too short");
    assert!(&current_part[0..1] == b"\"", "Invalid quoted string: no quote at the beginning");
    assert!(&current_part[current_part.len() - 1..] == b"\"", "Invalid quoted string: no quote at the end");

    complete_value.extend_from_slice(&current_part[1..current_part.len() - 1]);
}

#[test]
fn test_append_quoted_string() {
    let mut s = vec![];
    _append_quoted_string(b"\"foo\\n\"", &mut s);
    assert_eq!(&s, b"foo\\n");
    _append_quoted_string(b"\"asdf\"", &mut s);
    assert_eq!(&s, b"foo\\nasdf");
}

struct LinearTokenizer<'a> {
    haystack: &'a [u8],
    needle: &'a [u8],
    end: bool,
}
impl<'a> LinearTokenizer<'a> {
    fn new(haystack: &'a [u8], needle: &'a [u8]) -> LinearTokenizer<'a> {
        LinearTokenizer {
            haystack: haystack,
            needle: needle,
            end: haystack.is_empty(),
        }
    }
}
impl<'a> Iterator for LinearTokenizer<'a> {
    type Item = &'a [u8];
    fn next(&mut self) -> Option<&'a [u8]> {
        if self.end {
            return None
        }

        if self.haystack.len() > self.needle.len() {
            for end in 0..self.haystack.len() - self.needle.len() + 1 {
                if &self.haystack[end..end+self.needle.len()] == self.needle {
                    let res = &self.haystack[0..end];
                    self.haystack = &self.haystack[end+self.needle.len()..];
                    return Some(res);
                }
            }
        }
        self.end = true;
        Some(self.haystack)
    }
}

#[test]
fn test_split_by_newline() {
    let mut iter = LinearTokenizer::new(b"", b"\n");
    assert_eq!(iter.next(), None);

    let mut iter = LinearTokenizer::new(b"foo\nbar\n", b"\n");
    assert_eq!(iter.next().unwrap(), b"foo");
    assert_eq!(iter.next().unwrap(), b"bar");
    assert_eq!(iter.next().unwrap(), b"");
    assert_eq!(iter.next(), None);


    let mut iter = LinearTokenizer::new(b"asdf\\nfoo\\nbar\\n", b"\\n");
    assert_eq!(iter.next().unwrap(), b"asdf");
    assert_eq!(iter.next().unwrap(), b"foo");
    assert_eq!(iter.next().unwrap(), b"bar");
    assert_eq!(iter.next().unwrap(), b"");
    assert_eq!(iter.next(), None);

}

fn _format_as_quoted_string(title: &[u8], data: &[u8], add_tilde: bool, output_empty: bool, result: &mut Vec<u8>) {
    if data.is_empty() {
        if output_empty {
            if add_tilde {
                result.extend_from_slice(b"#~ ");
            }
            result.extend_from_slice(title);
            result.extend_from_slice(b" \"\"\n");
        }
        return;
    }

    let parts: Vec<&[u8]> = LinearTokenizer::new(data, b"\\n").collect();
    if add_tilde {
        result.extend_from_slice(b"#~ ");
    }
    result.extend_from_slice(title);
    result.push(b' ');
    for part in parts[0..parts.len() - 1].iter() {
        result.push(b'"');
        result.extend_from_slice(part);
        result.extend_from_slice(b"\\n\"\n");
        if add_tilde {
            result.extend_from_slice(b"#~ ");
        }
    }
    result.push(b'"');
    result.extend_from_slice(parts.last().expect("nonempty"));
    result.extend_from_slice(b"\"\n");
}

#[test]
fn test_format_as_quoted_string() {
    let mut result = vec![];
    _format_as_quoted_string(b"msgid", b"asdf", false, false, &mut result);
    assert_eq!(&result, b"msgid \"asdf\"\n");

    let mut result = vec![];
    _format_as_quoted_string(b"msgid", b"asdf", true, false, &mut result);
    assert_eq!(&result, b"#~ msgid \"asdf\"\n");

    let mut result = vec![];
    _format_as_quoted_string(b"msgid", b"asdf\\nfoo", false, false, &mut result);
    assert_eq!(&result, b"msgid \"asdf\\n\"\n\"foo\"\n");

    let mut result = vec![];
    _format_as_quoted_string(b"msgid", b"asdf\\nfoo", true, false, &mut result);
    assert_eq!(&result, b"#~ msgid \"asdf\\n\"\n#~ \"foo\"\n");

    let mut result = vec![];
    _format_as_quoted_string(b"msgstr", b"", false, true, &mut result);
    assert_eq!(&result, b"msgstr \"\"\n");

    let mut result = vec![];
    _format_as_quoted_string(b"msgstr", b"", false, false, &mut result);
    assert_eq!(&result, b"");
}

fn _append_prefixed_list(prefix: &[u8], lines: &Vec<&[u8]>, add_tilde: bool, result: &mut Vec<u8>) {
    for line in lines {
        if add_tilde {
            result.extend_from_slice(b"#~ ");
        }
        result.extend_from_slice(prefix);
        result.extend_from_slice(line);
        result.push(b'\n');
    }
}

#[test]
fn test_append_prefixed_list() {
    let mut result = vec![];
    let lines: Vec<&[u8]> = vec![
        b"foo",
        b"bar",
    ];
    _append_prefixed_list(b"#  ", &lines, false, &mut result);
    assert_eq!(&result, b"#  foo\n#  bar\n");

    result = vec![];
    _append_prefixed_list(b"#  ", &lines, true, &mut result);
    assert_eq!(&result, b"#~ #  foo\n#~ #  bar\n");
}

#[derive(Clone, Debug, PartialEq)]
enum TildeMode {
    Yes,
    No,
    Undefined,
}

#[derive(Clone, Debug)]
struct PoEntry<'l> {
    valid: bool,
    tilde: TildeMode,
    repeat_type: RepeatType,
    input: Vec<u8>,
    translator_comments: Vec<&'l [u8]>,
    extracted_comments: Vec<&'l [u8]>,
    references: Vec<&'l [u8]>,
    flags: Vec<&'l [u8]>,
    previous_untranslated_strings: Vec<&'l [u8]>,
    msgctxts: Vec<u8>,
    has_msgid: bool,
    msgids: Vec<u8>,
    msgid_plurals: Vec<u8>,
    has_msgstr: bool,
    msgstrs: Vec<u8>,
    has_msgstr_plurals: bool,
    msgstr_plurals: HashMap<u32, Vec<u8>>,
}

impl<'l> PoEntry<'l> {
    fn new() -> Self {
        PoEntry {
            valid: true,
            tilde: TildeMode::Undefined,
            repeat_type: RepeatType::Invalid,
            input: vec![],
            translator_comments: vec![],
            extracted_comments: vec![],
            references: vec![],
            flags: vec![],
            previous_untranslated_strings: vec![],
            msgctxts: vec![],
            has_msgid: false,
            msgids: vec![],
            msgid_plurals: vec![],
            has_msgstr: false,
            msgstrs: vec![],
            has_msgstr_plurals: false,
            msgstr_plurals: HashMap::new(),
        }
    }
    fn translator_comment(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');
        self.translator_comments.push(&line[3..]);
        self.repeat_type = RepeatType::Invalid;
    }
    fn extracted_comment(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');
        self.extracted_comments.push(&line[3..]);
        self.repeat_type = RepeatType::Invalid;
    }
    fn reference(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');
        self.references.push(&line[3..]);
        self.repeat_type = RepeatType::Invalid;
    }
    fn flag(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');
        self.flags.push(&line[3..]);
        self.repeat_type = RepeatType::Invalid;
    }
    fn previous_untranslated_string(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');
        self.previous_untranslated_strings.push(&line[3..]);
        self.repeat_type = RepeatType::Invalid;
    }
    fn msgctxt(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');
        if !self.msgctxts.is_empty() {
            println!("Warning: Repeated msgctxt in line {}", String::from_utf8_lossy(line));
            self.valid = false;
            return;
        }

        _append_quoted_string(&line[8..], &mut self.msgctxts);
        self.repeat_type = RepeatType::Msgctxt;
    }
    fn msgid(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');
        if self.has_msgid {
            println!("Warning: Repeated msgid in line {}", String::from_utf8_lossy(line));
            self.valid = false;
            return;
        }

        self.has_msgid = true;
        _append_quoted_string(&line[6..], &mut self.msgids);
        self.repeat_type = RepeatType::Msgid;
    }
    fn msgid_plural(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');
        if !self.msgid_plurals.is_empty() {
            println!("Warning: Repeated msgid_plural in line {}", String::from_utf8_lossy(line));
            self.valid = false;
            return;
        }

        _append_quoted_string(&line[13..], &mut self.msgid_plurals);
        self.repeat_type = RepeatType::MsgidPlural;
    }
    fn msgstr(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');
        if !self.msgstrs.is_empty() {
            println!("Warning: Repeated msgstr in line {}", String::from_utf8_lossy(line));
            self.valid = false;
            return;
        }

        _append_quoted_string(&line[7..], &mut self.msgstrs);
        self.has_msgstr = true;
        self.repeat_type = RepeatType::Msgstr;
    }
    fn _add_plural(&mut self, n: u32, value: &'l [u8]) {
        let plurals = match self.msgstr_plurals.get_mut(&n) {
            Some(v) => v,
            None => {
                self.msgstr_plurals.insert(n, vec![]);
                self.msgstr_plurals.get_mut(&n).unwrap()
            }
        };
        _append_quoted_string(value, plurals);
    }
    fn msgstr_plural(&mut self, line: &'l [u8]) {
        lazy_static! {
            static ref PLURAL_REGEX: Regex = Regex::new(r"^msgstr\[(\d+)\] (.*)$").expect("Valid PLURAL_REGEX");
        }

        self.input.extend_from_slice(line);
        self.input.push(b'\n');

        let captures: Captures<'_> = PLURAL_REGEX.captures(&line).expect("Valid msgstr_plural line");
        let n: u32 = captures.get(1).and_then(|m| from_utf8(m.as_bytes()).ok()).unwrap_or("").parse().expect("Valid PLURAL_REGEX Group 1");
        let value: &[u8] = captures.get(2).expect("Valid PLURAL_REGEX Group 2").as_bytes();

        if self.msgstr_plurals.contains_key(&n) {
            println!("Warning: Repeated msgstr[{}] in line {}", n, String::from_utf8_lossy(line));
            self.valid = false;
            return;
        }
        self._add_plural(n, value);
        self.has_msgstr_plurals = true;
        self.repeat_type = RepeatType::MsgstrPlural(n);
    }
    fn invalid(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');
        self.valid = false;
        self.repeat_type = RepeatType::Invalid;
    }
    fn repeat(&mut self, line: &'l [u8]) {
        self.input.extend_from_slice(line);
        self.input.push(b'\n');

        match self.repeat_type {
            RepeatType::Msgid => _append_quoted_string(line, &mut self.msgids),
            RepeatType::Msgstr => _append_quoted_string(line, &mut self.msgstrs),
            RepeatType::Msgctxt => _append_quoted_string(line, &mut self.msgctxts),
            RepeatType::MsgidPlural => _append_quoted_string(line, &mut self.msgid_plurals),
            RepeatType::MsgstrPlural(n) => self._add_plural(n, &line),
            RepeatType::Invalid => {
                println!("Warning: Unexpected repeated line {}", String::from_utf8_lossy(line));
                self.valid = false
            }
        }
    }
    pub fn parse_line(&mut self, mut line: &'l [u8]) -> ParseResult {
        lazy_static! {
            static ref WHITESPACE: Regex = Regex::new(r"^\s*$").expect("Valid WHITESPACE Regex");
        }

        if WHITESPACE.is_match(&line) {
            return ParseResult::NextEntry;
        }

        if line.len() >= 3 && &line[0..3] == b"#~ " {
            assert!(self.tilde != TildeMode::No, "Found a tilde line in a non-tilde entry!");
            self.tilde = TildeMode::Yes;
            line = &line[3..];
        } else {
            assert!(self.tilde != TildeMode::Yes, "Found a non-tilde line in a tilde entry!");
            self.tilde = TildeMode::No;
        }

        if !line.is_empty() && &line[0..1] == b"\"" {
            self.repeat(line);
            return ParseResult::Ok;
        }

        if line.len() < 3 {
            self.invalid(line);
            return ParseResult::Ok;
        }

        match &line[0..3] {
            b"#  " => self.translator_comment(line),
            b"#. " => self.extracted_comment(line),
            b"#: " => self.reference(line),
            b"#, " => self.flag(line),
            b"#| " => self.previous_untranslated_string(line),
            _ => {
                if line.len() >= 6 && &line[0..6] == b"msgid " {
                    self.msgid(line);
                    return ParseResult::Ok;
                }
                if line.len() >= 7 && &line[0..7] == b"msgstr " {
                    self.msgstr(line);
                    return ParseResult::Ok;
                }
                if line.len() >= 8 && &line[0..8] == b"msgctxt " {
                    self.msgctxt(line);
                    return ParseResult::Ok;
                }
                if line.len() >= 13 && &line[0..13] == b"msgid_plural " {
                    self.msgid_plural(line);
                    return ParseResult::Ok;
                }
                if line.len() >= 7 && &line[0..7] == b"msgstr[" {
                    self.msgstr_plural(line);
                    return ParseResult::Ok;
                }

                self.invalid(line);
                return ParseResult::Ok;
            }
        }

        ParseResult::Ok
    }
    pub fn commit(self, known_msgids: & mut HashMap<Vec<u8>, PoEntry<'l>>, result: &mut Vec<u8>) -> bool {
        if self.has_msgid {
            if let Some(other) = known_msgids.get(&self.msgids) {
                if self.try_merge(&other).is_some() && (self.tilde == TildeMode::Yes || self.tilde == other.tilde) {
                    return false;
                }
            }
        }
        known_msgids.insert(self.msgids.clone(), self.clone());

        if !self.valid {
            result.extend(self.input);
            return true;
        }
        _append_prefixed_list(b"#  ", &self.translator_comments, self.tilde == TildeMode::Yes, result);
        _append_prefixed_list(b"#. ", &self.extracted_comments, self.tilde == TildeMode::Yes, result);
        _append_prefixed_list(b"#: ", &self.references, self.tilde == TildeMode::Yes, result);
        _append_prefixed_list(b"#, ", &self.flags, self.tilde == TildeMode::Yes, result);
        _append_prefixed_list(b"#| ", &self.previous_untranslated_strings, self.tilde == TildeMode::Yes, result);

        _format_as_quoted_string(b"msgctxt", &self.msgctxts, self.tilde == TildeMode::Yes, false, result);
        _format_as_quoted_string(b"msgid", &self.msgids, self.tilde == TildeMode::Yes, self.has_msgid, result);
        _format_as_quoted_string(b"msgid_plural", &self.msgid_plurals, self.tilde == TildeMode::Yes, false, result);
        _format_as_quoted_string(b"msgstr", &self.msgstrs, self.tilde == TildeMode::Yes, self.has_msgstr, result);

        let mut keys: Vec<&u32> = (&self.msgstr_plurals).keys().collect();
        keys.sort();
        for n in keys {
            let lines = &self.msgstr_plurals[n];
            _format_as_quoted_string(format!("msgstr[{}]", n).as_bytes(), &lines, self.tilde == TildeMode::Yes, self.has_msgstr_plurals, result);
        }

        true
    }
    pub fn has_content(&self) -> bool {
        !self.translator_comments.is_empty()
            || !self.extracted_comments.is_empty()
            || !self.references.is_empty()
            || !self.flags.is_empty()
            || !self.previous_untranslated_strings.is_empty()
            || !self.msgids.is_empty()
            || !self.msgstrs.is_empty()
    }
    pub fn try_merge(&self, other: &PoEntry<'l>) -> Option<PoEntry<'l>> {
        if self.has_msgid && other.has_msgid && self.msgids == b"" && other.msgids == b"" {
            return Some(self.clone());
        }

        if !self.valid || !other.valid {
            if !self.valid && !other.valid && self.input == other.input {
                return Some(self.clone());
            }
            return None;
        }

        if self.translator_comments.len() != other.translator_comments.len()
            || self.translator_comments.iter().zip(&other.translator_comments).any(|(a, b)| a != b)
        {
            return None;
        }
        if self.extracted_comments.len() != other.extracted_comments.len() || self.extracted_comments.iter().zip(&other.extracted_comments).any(|(a, b)| a != b)
        {
            return None;
        }
        if self.flags.len() != other.flags.len() || self.flags.iter().zip(&other.flags).any(|(a, b)| a != b) {
            return None;
        }
        if self.previous_untranslated_strings.len() != other.previous_untranslated_strings.len()
            || self
                .previous_untranslated_strings
                .iter()
                .zip(&other.previous_untranslated_strings)
                .any(|(a, b)| a != b)
        {
            return None;
        }
        if self.has_msgid && other.has_msgid && self.msgids != other.msgids {
            return None;
        }
        if self.msgstrs != other.msgstrs {
            return None;
        }
        if self.msgctxts != other.msgctxts {
            return None;
        }
        if self.msgid_plurals != other.msgid_plurals {
            return None;
        }
        if self.msgstr_plurals.len() != other.msgstr_plurals.len()
            || self
                .msgstr_plurals
                .keys()
                .map(|n| other.msgstr_plurals.get(n).filter(|other| *other == &self.msgstr_plurals[n]).is_some())
                .any(|b| b)
        {
            return None;
        }

        if self.tilde == TildeMode::Yes && other.tilde == TildeMode::No {
            Some(other.clone())
        } else {
            Some(self.clone())
        }
    }
}

#[test]
fn test_poentry_try_merge() {
    let test_cases: Vec<(Vec<&[u8]>, Vec<&[u8]>, _, _, Option<&[u8]>)> = vec![
        (vec![b"invalid"], vec![b"msgid \"asdf\""], false, true, None),
        (vec![&b"msgid \"asdf\""[..]], vec![&b"invalid"[..]], true, false, None),
        (vec![&b"msgid \"asdf\""[..]], vec![&b"msgid \"something else\""[..]], true, true, None),
        (vec![&b"msgid \"asdf\""[..]], vec![&b"msgid \"asdf\""[..]], true, true, Some(&b"msgid \"asdf\"\n"[..])),
        (
            vec![&b"msgid \"asdf\""[..], &b"msgstr \"asdf\""[..]],
            vec![&b"msgid \"asdf\""[..], &b"msgstr \"asdf\""[..]],
            true,
            true,
            Some(&b"msgid \"asdf\"\nmsgstr \"asdf\"\n"[..]),
        ),
    ];

    for (inpa, inpb, valida, validb, expected_result) in test_cases {
        let mut a = PoEntry::new();
        let mut b = PoEntry::new();
        for line in inpa {
            a.parse_line(line);
        }
        for line in inpb {
            b.parse_line(line);
        }

        assert_eq!(a.valid, valida);
        assert_eq!(b.valid, validb);
        if let Some(expected_str) = expected_result {
            let mut res : Vec<u8> = vec![];
            assert!(a.try_merge(&b).unwrap().commit(&mut HashMap::new(), &mut res));
            assert_eq!(expected_str, &res[..]);
        } else {
            assert!(a.try_merge(&b).is_none());
        }
    }
}

#[derive(Debug, PartialEq)]
enum ConflictPosition {
    Left,
    Right,
}

#[derive(Debug)]
struct Conflict<'l> {
    input: Vec<u8>,
    position: ConflictPosition,
    left: Vec<PoEntry<'l>>,
    right: Vec<PoEntry<'l>>,
}

impl<'l> Conflict<'l> {
    fn new(initial_entry: PoEntry<'l>, initial_line: &[u8]) -> Self {
        let mut input = initial_entry.input.clone();
        input.extend_from_slice(initial_line);
        input.push(b'\n');
        Conflict {
            input: input,
            position: ConflictPosition::Left,
            left: vec![initial_entry.clone()],
            right: vec![initial_entry],
        }
    }

    fn parse_left(&mut self, line: &'l [u8]) -> ParseResult {
        if line.len() >= 7 && &line[0..7] == b"=======" {
            return ParseResult::NextEntry;
        }
        if self.left.last_mut().unwrap().parse_line(line) == ParseResult::NextEntry {
            self.left.push(PoEntry::new());
        }
        ParseResult::Ok
    }

    fn parse_right(&mut self, line: &'l [u8]) -> ParseResult {
        if line.len() >= 8 && &line[0..8] == b">>>>>>> " {
            return ParseResult::NextEntry;
        }
        if self.right.last_mut().unwrap().parse_line(line) == ParseResult::NextEntry {
            self.right.push(PoEntry::new());
        }
        ParseResult::Ok
    }

    fn parse_line(&mut self, line: &'l [u8]) -> ParseResult {
        self.input.extend(line);
        self.input.push(b'\n');

        if self.position == ConflictPosition::Left {
            if self.parse_left(line) == ParseResult::NextEntry {
                self.position = ConflictPosition::Right;
            }
            return ParseResult::Ok;
        }
        self.parse_right(line)
    }

    fn try_merge(&self) -> Option<Vec<PoEntry<'l>>> {
        let mut right_lookup: HashMap<Vec<u8>, &PoEntry<'l>> = HashMap::new();
        for rs in self.right.iter() {
            right_lookup.insert(rs.msgids.clone(), &rs);
        }

        let mut res = vec![];
        for left in self.left.iter() {
            if let Some(right) = right_lookup.remove(&left.msgids) {
                if let Some(merged) = left.try_merge(right) {
                    res.push(merged);
                } else {
                    return None;
                }
            } else {
                res.push(left.clone());
            }
        }

        let old_last = res.pop().expect("Conflict created with an initial PoEntry, should never be empty");
        for (_, right) in right_lookup.drain() {
            res.push(right.clone());
        }
        res.push(old_last);
        Some(res)
    }

    fn commit(self, known_msgids: &mut HashMap<Vec<u8>, PoEntry<'l>>, result: &mut Vec<u8>) -> PoEntry<'l> {
        if let Some(mut entries) = self.try_merge() {
            let last_entry = entries.pop().unwrap_or_else(PoEntry::new);
            for entry in entries {
                if entry.commit(known_msgids, result) {
                    result.push(b'\n');
                }
            }
            return last_entry;
        }
        result.extend(&self.input);
        PoEntry::new()
    }
}

#[test]
fn test_conflict_marker_parsing() {
    let initial = PoEntry::new();
    let mut c = Conflict::new(initial, b"<<<<<<<");
    assert!(c.parse_left(b"=======") == ParseResult::NextEntry);
    assert_eq!(c.parse_right(b">>>>>>> "), ParseResult::NextEntry)
}

pub fn parse_po_lines(file_content: &Vec<u8>) -> Vec<u8> {
    let mut result: Vec<u8> = vec![];
    let mut current_entry = PoEntry::new();
    let mut current_conflict: Option<Conflict> = None;
    let mut known_msgids : HashMap<Vec<u8>, PoEntry> = HashMap::new();

    for line in file_content.split(|&c| c == b'\n') {
        if let Some(mut conflict) = current_conflict {
            if conflict.parse_line(line) == ParseResult::Ok {
                current_conflict = Some(conflict);
            } else {
                current_entry = conflict.commit(&mut known_msgids, &mut result);
                current_conflict = None;
            }
        } else {
            if line.len() > 7 && &line[0..7] == b"<<<<<<<" {
                current_conflict = Some(Conflict::new(current_entry.clone(), line));
                continue;
            }
            if current_entry.parse_line(line) == ParseResult::NextEntry {
                if current_entry.commit(&mut known_msgids, &mut result) {
                    result.push(b'\n');
                    result.extend_from_slice(line);
                }
                current_entry = PoEntry::new();
            }
        }
    }

    if current_entry.has_content() && current_entry.commit(&mut known_msgids, &mut result) {
        result.push(b'\n');
    }

    if !result.is_empty() {
        result.pop();
    }

    result
}

fn usage() {
    println!("Usage: {} <file>..", std::env::args().next().unwrap_or(String::from("pomerge")));
    println!("Resolves git-style merge conflicts in PO files.");
    std::process::exit(1);
}

fn main() -> Result<(), MyErr> {
    let argv: Vec<String> = std::env::args().collect();
    if argv.len() < 2 {
        usage();
    }

    for fname in &argv[1..] {
        let mut file_content : Vec<u8> = vec![];

        {
            let mut file = match File::open(fname) {
                Err(e) => { eprintln!("Could not open {}: {:?}", fname, e); continue; }
                Ok(file) => {
                    file
                }
            };

            if let Err(e) = file.read_to_end(&mut file_content) {
                eprintln!("Read from {} failed: {:?}", fname, e);
                continue;
            }
        }

        let result = parse_po_lines(&file_content);

        {
            let mut file = match File::create(fname) {
                Err(e) => { eprintln!("Could not open {}: {:?}", fname, e); continue; }
                Ok(file) => { file }
            };
            if let Err(e) = file.write_all(&result) {
                eprintln!("Write to {} failed: {:?}", fname, e);
                continue;
            }
        }

    }

    Ok(())
}

#[test]
fn simple_parser_test() {
    let src: Vec<&[u8]> = vec![b"msgid \"foo\"", b"msgstr \"bar\""];

    let mut po_entry = PoEntry::new();
    for line in src {
        assert_eq!(po_entry.parse_line(line), ParseResult::Ok)
    }
    assert!(po_entry.valid);
    assert_eq!(po_entry.tilde, TildeMode::No);
    assert_eq!(po_entry.parse_line(b""), ParseResult::NextEntry);

    let mut output = vec![];
    assert!(po_entry.commit(&mut HashMap::new(), &mut output));
    assert_eq!(output, b"msgid \"foo\"\nmsgstr \"bar\"\n");
}

#[test]
fn tile_parser_test() {
    let src : Vec<&[u8]> = vec![b"#~ msgid \"foo\"", b"#~ msgstr \"bar\""];

    let mut po_entry = PoEntry::new();
    for line in src {
        assert_eq!(po_entry.parse_line(line), ParseResult::Ok)
    }
    assert!(po_entry.valid);
    assert_eq!(po_entry.tilde, TildeMode::Yes);
    assert_eq!(po_entry.parse_line(b""), ParseResult::NextEntry);

    let mut output = vec![];
    assert!(po_entry.commit(&mut HashMap::new(), &mut output));
    assert_eq!(output, b"#~ msgid \"foo\"\n#~ msgstr \"bar\"\n");
}

#[test]
fn full_parser_test() {
    let src: Vec<&[u8]> = vec![
        b"#  translator-comment",
        b"#  some_other_comment",
        b"#. code comment",
        b"#: file.rs:1337",
        b"#, fuzzy, c-format",
        b"#| \"blork\"",
        b"msgid \"No thing found\"",
        b"msgid_plural \"%d things found\"",
        b"msgstr[0] \"Nothing found\"",
        b"msgstr[1] \"%dthing found\"",
    ];

    let mut po_entry = PoEntry::new();

    for line in src {
        assert_eq!(po_entry.parse_line(line), ParseResult::Ok)
    }
    assert!(po_entry.valid);
    assert_eq!(po_entry.parse_line(b""), ParseResult::NextEntry);

    let mut output = vec![];
    assert!(po_entry.commit(&mut HashMap::new(), &mut output));
    let res: Vec<&[u8]> = output.split(|&c| c == b'\n').collect();
    assert_eq!(res[0], b"#  translator-comment");
    assert_eq!(res[1], b"#  some_other_comment");
    assert_eq!(res[2], b"#. code comment");
    assert_eq!(res[3], b"#: file.rs:1337");
    assert_eq!(res[4], b"#, fuzzy, c-format");
    assert_eq!(res[5], b"#| \"blork\"");
    assert_eq!(res[6], b"msgid \"No thing found\"");
    assert_eq!(res[7], b"msgid_plural \"%d things found\"");
    assert_eq!(res[8], b"msgstr[0] \"Nothing found\"");
    assert_eq!(res[9], b"msgstr[1] \"%dthing found\"");
}

#[test]
fn invalid_test() {
    let src: Vec<&[u8]> = vec![b"msgid \"foo\"", b"somethingelse \"bar\"", b"msgstr \"bar\""];

    let mut po_entry = PoEntry::new();
    for line in src {
        assert_eq!(po_entry.parse_line(line), ParseResult::Ok)
    }
    assert!(!po_entry.valid);

    let mut output = vec![];
    assert!(po_entry.commit(&mut HashMap::new(), &mut output));
    let res: Vec<&[u8]> = output.split(|&c| c == b'\n').collect();
    assert_eq!(res[0], b"msgid \"foo\"");
    assert_eq!(res[1], b"somethingelse \"bar\"");
    assert_eq!(res[2], b"msgstr \"bar\"");
}

#[test]
fn complete_file_with_valid_content() {
    let mut input: Vec<u8> = vec![];
    File::open("corpus/clean.po").unwrap().read_to_end(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    File::open("corpus/clean.po.res").unwrap().read_to_end(&mut expected).unwrap();
    assert_eq!(parse_po_lines(&input), expected);
}

#[test]
fn complete_file_with_path_conflict() {
    let mut input: Vec<u8> = vec![];
    File::open("corpus/paths.po").unwrap().read_to_end(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    File::open("corpus/paths.po.res").unwrap().read_to_end(&mut expected).unwrap();
    assert_eq!(parse_po_lines(&input), expected);
}

#[test]
fn complete_file_with_header_conflict() {
    let mut input: Vec<u8> = vec![];
    File::open("corpus/header.po").unwrap().read_to_end(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    File::open("corpus/header.po.res").unwrap().read_to_end(&mut expected).unwrap();
    assert_eq!(parse_po_lines(&input), expected);
}

#[test]
fn complete_file_with_reordered_conflict() {
    let mut input: Vec<u8> = vec![];
    File::open("corpus/reorder.po").unwrap().read_to_end(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    File::open("corpus/reorder.po.res").unwrap().read_to_end(&mut expected).unwrap();
    assert_eq!(parse_po_lines(&input), expected);
}

#[test]
fn complete_file_with_reordered_conflict_with_elements_only_on_one_side() {
    let mut input: Vec<u8> = vec![];
    File::open("corpus/elements_only_on_one_side.po").unwrap().read_to_end(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    File::open("corpus/elements_only_on_one_side.po.res").unwrap().read_to_end(&mut expected).unwrap();
    assert_eq!(parse_po_lines(&input), expected);
}

#[test]
fn complete_file_with_conflict_with_prefix() {
    let mut input: Vec<u8> = vec![];
    File::open("corpus/conflict_with_prefix.po").unwrap().read_to_end(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    File::open("corpus/conflict_with_prefix.po.res").unwrap().read_to_end(&mut expected).unwrap();
    let is = parse_po_lines(&input);
    let should = String::from_utf8(expected).unwrap();
    assert_eq!(String::from_utf8(is).unwrap(), should);
}

#[test]
fn complete_file_with_bug1() {
    let mut input: Vec<u8> = vec![];
    File::open("corpus/bug1.po").unwrap().read_to_end(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    File::open("corpus/bug1.po.res").unwrap().read_to_end(&mut expected).unwrap();
    let is = parse_po_lines(&input);
    let should = String::from_utf8(expected).unwrap();
    assert_eq!(String::from_utf8(is).unwrap(), should);
}

#[test]
fn poentry_skip_duplicates() {
    let mut input: Vec<u8> = vec![];
    File::open("corpus/duplicates.po").unwrap().read_to_end(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    File::open("corpus/duplicates.po.res").unwrap().read_to_end(&mut expected).unwrap();
    assert_eq!(parse_po_lines(&input), expected);
}

#[test]
fn poentry_skip_duplicates_conflict() {
    let mut input: Vec<u8> = vec![];
    File::open("corpus/duplicates_conflict.po").unwrap().read_to_end(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    File::open("corpus/duplicates_conflict.po.res").unwrap().read_to_end(&mut expected).unwrap();
    let result = parse_po_lines(&input);
    assert_eq!(String::from_utf8(result).unwrap(), String::from_utf8(expected).unwrap());
}

#[test]
fn poentry_repeat_duplicate_without_tilde() {
    let mut input: Vec<u8> = vec![];
    File::open("corpus/duplicates_without_tilde.po").unwrap().read_to_end(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    File::open("corpus/duplicates_without_tilde.po.res").unwrap().read_to_end(&mut expected).unwrap();
    let result = parse_po_lines(&input);
    assert_eq!(String::from_utf8(result).unwrap(), String::from_utf8(expected).unwrap());
}
