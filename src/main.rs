// TODO remove duplicates
// #~ lines
// TODO make input a slice
// TODO bytes instead of strings

extern crate regex;
#[macro_use] extern crate lazy_static;

#[cfg(test)]
use pretty_assertions::assert_eq;

use std::collections::HashMap;
use std::fmt::Write;
use std::fs::File;
use std::io::Read;
use regex::Regex;


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

trait Parser<'l> {
    fn parse_line(&mut self, line: &'l str) -> ParseResult;
}

fn _append_quoted_string(full_string: &str, current_value: &mut String) {
    assert!(full_string.len() >= 2, "Invalid quoted string: too short");
    assert!(&full_string[0..1] == "\"", "Invalid quoted string: no quote at the beginning");
    assert!(&full_string[full_string.len()-1..] == "\"", "Invalid quoted string: no quote at the end");

    current_value.push_str(&full_string[1..full_string.len()-1]);
}

#[test]
fn test_append_quoted_string() {
    let mut s = String::new();
    _append_quoted_string("\"foo\\n\"", &mut s);
    assert_eq!(&s, "foo\\n");
    _append_quoted_string("\"asdf\"", &mut s);
    assert_eq!(&s, "foo\\nasdf");
}

fn _format_as_quoted_string(title: &str, data: &str, result: &mut String) {
    if data.len() == 0 {
        return;
    }

    let parts: Vec<&str> = data.split("\\n").collect();
    write!(result, "{} ", title).expect("write! works on Strings");
    for part in parts[0..parts.len()-1].iter() {
        writeln!(result, "\"{}\\n\"", part).expect("write! works on Strings");
    }
    writeln!(result, "\"{}\"", parts.last().unwrap()).expect("write! works on Strings");
}

#[derive(Clone, Debug)]
struct PoEntry<'l> {
    valid: bool,
    repeat_type: RepeatType,
    input: String,
    translator_comments: Vec<&'l str>,
    extracted_comments: Vec<&'l str>,
    references: Vec<&'l str>,
    flags: Vec<&'l str>,
    previous_untranslated_strings: Vec<&'l str>,
    msgctxts: String,
    has_msgid: bool,
    msgids: String,
    msgid_plurals: String,
    msgstrs: String,
    msgstr_plurals: HashMap<u32, String>,
}

#[test]
fn test_format_as_quoted_string() {
    let mut result = String::new();
    _format_as_quoted_string("msgid", "asdf", &mut result);
    assert_eq!(&result, "msgid \"asdf\"\n");

    let mut result = String::new();
    _format_as_quoted_string("msgid", "asdf\\nfoo", &mut result);
    assert_eq!(&result, "msgid \"asdf\\n\"\n\"foo\"\n");
}

impl<'l> PoEntry<'l> {
    fn new() -> Self {
        PoEntry{
            valid: true,
            repeat_type: RepeatType::Invalid,
            input: String::new(),
            translator_comments: vec!(),
            extracted_comments: vec!(),
            references: vec!(),
            flags: vec!(),
            previous_untranslated_strings: vec!(),
            msgctxts: String::new(),
            has_msgid: false,
            msgids: String::new(),
            msgid_plurals: String::new(),
            msgstrs: String::new(),
            msgstr_plurals: HashMap::new(),
        }
    }
    fn translator_comment(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        self.translator_comments.push(&line[3..]);
        self.repeat_type = RepeatType::Invalid;
    }
    fn extracted_comment(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        self.extracted_comments.push(&line[3..]);
        self.repeat_type = RepeatType::Invalid;
    }
    fn reference(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        self.references.push(&line[3..]);
        self.repeat_type = RepeatType::Invalid;
    }
    fn flag(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        self.flags.push(&line[3..]);
        self.repeat_type = RepeatType::Invalid;
    }
    fn previous_untranslated_string(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        self.previous_untranslated_strings.push(&line[3..]);
        self.repeat_type = RepeatType::Invalid;
    }
    fn msgctxt(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        if self.msgctxts.len() != 0 {
            println!("Warning: Repeated msgctxt in line {}", line);
            self.valid = false;
            return;
        }

        _append_quoted_string(&line[8..], &mut self.msgctxts);
        self.repeat_type = RepeatType::Msgctxt;
    }
    fn msgid(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        if self.has_msgid {
            println!("Warning: Repeated msgid in line {}", line);
            self.valid = false;
            return;
        }

        self.has_msgid = true;
        _append_quoted_string(&line[6..], &mut self.msgids);
        self.repeat_type = RepeatType::Msgid;
    }
    fn msgid_plural(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        if self.msgid_plurals.len() != 0 {
            println!("Warning: Repeated msgid_plural in line {}", line);
            self.valid = false;
            return;
        }

        _append_quoted_string(&line[13..], &mut self.msgid_plurals);
        self.repeat_type = RepeatType::MsgidPlural;
    }
    fn msgstr(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        if self.msgstrs.len() != 0 {
            println!("Warning: Repeated msgstr in line {}", line);
            self.valid = false;
            return;
        }

        _append_quoted_string(&line[7..], &mut self.msgstrs);
        self.repeat_type = RepeatType::Msgstr;
    }
    fn _add_plural(&mut self, n: u32, value: &'l str) {
        let plurals = match self.msgstr_plurals.get_mut(&n) {
            Some(v) => v,
            None => {
                self.msgstr_plurals.insert(n, String::new());
                self.msgstr_plurals.get_mut(&n).unwrap()
            }
        };
        _append_quoted_string(value, plurals);
    }
    fn msgstr_plural(&mut self, line: &'l str) {
        lazy_static! {
            static ref PLURAL_REGEX : Regex = Regex::new(r"^msgstr\[(\d+)\] (.*)$").expect("Valid PLURAL_REGEX");
        }

        self.input.push_str(line);
        self.input.push('\n');

        let captures : regex::Captures<'_> = PLURAL_REGEX.captures(&line).expect("Valid msgstr_plural line");
        let n : u32 = captures.get(1).map_or("", |m| m.as_str()).parse().expect("Valid PLURAL_REGEX Group 1");
        let value : &str = captures.get(2).expect("Valid PLURAL_REGEX Group 2").as_str();

        if self.msgstr_plurals.contains_key(&n) {
            println!("Warning: Repeated msgstr[{}] in line {}", n, line);
            self.valid = false;
            return;
        }
        self._add_plural(n, value);
        self.repeat_type = RepeatType::MsgstrPlural(n);
    }
    fn invalid(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        self.valid = false;
        self.repeat_type = RepeatType::Invalid;
    }
    fn repeat(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');

        match self.repeat_type {
            RepeatType::Msgid => _append_quoted_string(line, &mut self.msgids),
            RepeatType::Msgstr => _append_quoted_string(line, &mut self.msgstrs),
            RepeatType::Msgctxt => _append_quoted_string(line, &mut self.msgctxts),
            RepeatType::MsgidPlural => _append_quoted_string(line, &mut self.msgid_plurals),
            RepeatType::MsgstrPlural(n) => self._add_plural(n, &line),
            RepeatType::Invalid => {
                println!("Warning: Unexpected repeated line {}", line);
                self.valid = false
            },
        }
    }
    pub fn parse_line(&mut self, line: &'l str) -> ParseResult {
        lazy_static! {
            static ref WHITESPACE: Regex = Regex::new(r"^\s*$").expect("Valid WHITESPACE Regex");
        }

        if WHITESPACE.is_match(&line) {
            return ParseResult::NextEntry
        }

        if line.len() > 0 && &line[0..1] == "\"" {
            self.repeat(line);
            return ParseResult::Ok;
        }

        if line.len() < 3 {
            self.invalid(line);
            return ParseResult::Ok;
        }

        match &line[0..3] {
            "#  " => self.translator_comment(line),
            "#. " => self.extracted_comment(line),
            "#: " => self.reference(line),
            "#, " => self.flag(line),
            "#| " => self.previous_untranslated_string(line),
            _ => {
                if line.len() >= 6 && &line[0..6] == "msgid " {
                    self.msgid(line);
                    return ParseResult::Ok;
                }
                if line.len() >= 7 && &line[0..7] == "msgstr " {
                    self.msgstr(line);
                    return ParseResult::Ok;
                }
                if line.len() >= 8 && &line[0..8] == "msgctxt " {
                    self.msgctxt(line);
                    return ParseResult::Ok;
                }
                if line.len() >= 13 && &line[0..13] == "msgid_plural " {
                    self.msgid_plural(line);
                    return ParseResult::Ok;
                }
                if line.len() >= 7 && &line[0..7] == "msgstr[" {
                    self.msgstr_plural(line);
                    return ParseResult::Ok;
                }

                self.invalid(line);
                return ParseResult::Ok;
            },
        }

        ParseResult::Ok
    }
    pub fn commit(self) -> String {
        if !self.valid {
            return self.input;
        }
        let mut result = String::new();
        for translator_comment in self.translator_comments {
            result.push_str(&format!("#  {}\n", translator_comment));
        }
        for extracted_comment in self.extracted_comments {
            result.push_str(&format!("#. {}\n", extracted_comment));
        }
        for reference in self.references {
            result.push_str(&format!("#: {}\n", reference));
        }
        for flag in self.flags {
            result.push_str(&format!("#, {}\n", flag));
        }
        for previous_untranslated_string in self.previous_untranslated_strings {
            result.push_str(&format!("#| {}\n", previous_untranslated_string));
        }

        _format_as_quoted_string("msgctxt", &self.msgctxts, &mut result);
        _format_as_quoted_string("msgid", &self.msgids, &mut result);
        _format_as_quoted_string("msgid_plural", &self.msgid_plurals, &mut result);
        _format_as_quoted_string("msgstr", &self.msgstrs, &mut result);

        let mut keys: Vec<&u32> = (&self.msgstr_plurals).keys().collect();
        keys.sort();
        for n in keys {
            let lines = &self.msgstr_plurals[n];
            _format_as_quoted_string(&format!("msgstr[{}]", n), &lines, &mut result);
        }

        return result;
    }
    pub fn has_content(&self) -> bool {
        return self.translator_comments.len() > 0 || self.extracted_comments.len() > 0 || self.references.len() > 0 || self.flags.len() > 0 || self.previous_untranslated_strings.len() > 0 || self.msgids.len() > 0 || self.msgstrs.len() > 0
    }
    pub fn try_merge(&self, other: &PoEntry<'l>) -> Option<PoEntry<'l>> {
        if self.has_msgid && other.has_msgid && self.msgids == "" && other.msgids == "" {
            return Some(self.clone())
        }

        if !self.valid || !other.valid {
            if self.valid && other.valid && self.input == other.input {
                return Some(self.clone())
            }
            return None
        }

        if self.translator_comments.len() != other.translator_comments.len() || self.translator_comments.iter().zip(&other.translator_comments).any(|(a, b)| a != b) {
            return None
        }
        if self.extracted_comments.len() != other.extracted_comments.len() || self.extracted_comments.iter().zip(&other.extracted_comments).any(|(a, b)| a != b) {
            return None
        }
        if self.flags.len() != other.flags.len() || self.flags.iter().zip(&other.flags).any(|(a, b)| a != b) {
            return None
        }
        if self.previous_untranslated_strings.len() != other.previous_untranslated_strings.len() || self.previous_untranslated_strings.iter().zip(&other.previous_untranslated_strings).any(|(a, b)| a != b) {
            return None
        }
        if self.has_msgid && other.has_msgid && self.msgids != other.msgids {
            return None
        }
        if self.msgstrs != other.msgstrs {
            return None
        }
        if self.msgctxts != other.msgctxts {
            return None
        }
        if self.msgid_plurals != other.msgid_plurals {
            return None
        }
        if self.msgstr_plurals.len() != other.msgstr_plurals.len() || self.msgstr_plurals.keys().map(|n| other.msgstr_plurals.get(n).filter(|other| *other == &self.msgstr_plurals[n]).is_some()).any(|b| b) {
            return None
        }

        return Some(self.clone())
    }
}

#[test]
fn test_poentry_try_merge() {
    let test_cases : Vec<(_, _, _, _, Option<&str>)> = vec![
        (vec!["invalid"], vec!["msgid \"asdf\""], false, true, None),
        (vec!["msgid \"asdf\""], vec!["invalid"], true, false, None),
        (vec!["msgid \"asdf\""], vec!["msgid \"something else\""], true, true, None),
        (vec!["msgid \"asdf\""], vec!["msgid \"asdf\""], true, true, Some("msgid \"asdf\"\n")),
        (vec!["msgid \"asdf\"", "msgstr \"asdf\""], vec!["msgid \"asdf\"", "msgstr \"asdf\""], true, true, Some("msgid \"asdf\"\nmsgstr \"asdf\"\n")),
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
            assert_eq!(expected_str, a.try_merge(&b).unwrap().commit());
        } else {
            assert!(a.try_merge(&b).is_none());
        }
    }
}

#[derive(Debug, PartialEq)]
enum ConflictPosition {
    Left,
    Right
}

#[derive(Debug)]
struct Conflict<'l> {
    input: String,
    position: ConflictPosition,
    left: Vec<PoEntry<'l>>,
    right: Vec<PoEntry<'l>>,
}

impl<'l> Conflict<'l> {
    fn new(initial_entry: PoEntry<'l>, initial_line: &str) -> Self {
        Conflict {
            input: format!("{}\n", initial_line),
            position: ConflictPosition::Left,
            left: vec![initial_entry.clone()],
            right: vec![initial_entry],
        }
    }

    fn parse_left(&mut self, line: &'l str) -> ParseResult {
        if line.len() >= 7 && &line[0..7] == "=======" {
            return ParseResult::NextEntry;
        }
        if self.left.last_mut().unwrap().parse_line(line) == ParseResult::NextEntry {
            self.left.push(PoEntry::new());
        }
        return ParseResult::Ok;
    }

    fn parse_right(&mut self, line: &'l str) -> ParseResult {
        if line.len() >= 8 && &line[0..8] == ">>>>>>> " {
            return ParseResult::NextEntry;
        }
        if self.right.last_mut().unwrap().parse_line(line) == ParseResult::NextEntry {
            self.right.push(PoEntry::new());
        }
        return ParseResult::Ok;
    }

    fn parse_line(&mut self, line: &'l str) -> ParseResult {
        self.input.push_str(line);
        self.input.push('\n');

        if self.position == ConflictPosition::Left && self.parse_left(line) == ParseResult::NextEntry {
            self.position = ConflictPosition::Right;
            return ParseResult::Ok;
        }
        return self.parse_right(line);
    }

    fn try_merge(&self) -> Option<Vec<PoEntry<'l>>> {
        let mut right_lookup: HashMap<String, &PoEntry<'l>> = HashMap::new();
        for rs in self.right.iter() {
            right_lookup.insert(rs.msgids.clone(), &rs);
        }

        let mut res = vec!();
        for left in self.left.iter() {
            if let Some(right) = right_lookup.remove(&left.msgids) {
                if let Some(merged) = left.try_merge(right) {
                    res.push(merged);
                } else {
                    return None
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
        return Some(res);
    }

    fn commit(self, result: &mut  String) -> PoEntry<'l> {
        if let Some(mut entries) = self.try_merge() {
            let last_entry = entries.pop().unwrap_or_else(|| PoEntry::new());
            for entry in entries {
                result.push_str(&entry.commit());
                result.push('\n');
            }
            return last_entry;
        }
        result.push_str(&self.input);
        return PoEntry::new();
    }
}

#[test]
fn test_conflict_marker_parsing() {
    let initial = PoEntry::new();
    let mut c = Conflict::new(initial, "<<<<<<<");
    assert!(c.parse_left("=======") == ParseResult::NextEntry);
    assert_eq!(c.parse_right(">>>>>>> "), ParseResult::NextEntry)
}

pub fn parse_po_lines(lines: &str) -> Result<String, MyErr> {
    let mut result = String::new();
    let mut current_entry = PoEntry::new();
    let mut current_conflict : Option<Conflict> = None;

    for line in lines.lines() {
        if let Some(mut conflict) = current_conflict {
            if conflict.parse_line(line) == ParseResult::Ok {
                current_conflict = Some(conflict);
            } else {
                current_entry = conflict.commit(&mut result);
                current_conflict = None;
            }
        } else {
            if line.len() > 7 && &line[0..7] == "<<<<<<<" {
                current_conflict = Some(Conflict::new(current_entry.clone(), line));
                continue;
            }
            if current_entry.parse_line(line) == ParseResult::NextEntry {
                result.push_str(&current_entry.commit());
                result.push_str(line);
                result.push('\n');
                current_entry = PoEntry::new();
            }
        }
    };

    if current_entry.has_content() {
        result.push_str(&current_entry.commit());
    }

    Ok(result)
}

fn main() -> Result<(), MyErr> {
    let argv : Vec<String> = std::env::args().collect();
    if argv.len() < 2 {
        panic!("Missing Argument Filename");
    }

    let mut file_content = String::new();
    File::open(&argv[1])?.read_to_string(&mut file_content)?;
    print!("{}", parse_po_lines(&file_content)?);

    return Ok(())
}

#[test]
fn simple_parser_test() {
    let src: Vec<&str> = vec![
        "msgid \"foo\"",
        "msgstr \"bar\"",
    ];

    let mut po_entry = PoEntry::new();
    for line in src {
        assert_eq!(po_entry.parse_line(line), ParseResult::Ok)
    }
    assert!(po_entry.valid);
    assert_eq!(po_entry.parse_line(""), ParseResult::NextEntry);

    let output = po_entry.commit();
    let res: Vec<&str> = output.lines().collect();
    assert_eq!(res[0], "msgid \"foo\"");
    assert_eq!(res[1], "msgstr \"bar\"");
}

#[test]
fn full_parser_test() {
    let src: Vec<&str> = vec![
        "#  translator-comment",
        "#  some_other_comment",
        "#. code comment",
        "#: file.rs:1337",
        "#, fuzzy, c-format",
        "#| \"blork\"",
        "msgid \"No thing found\"",
        "msgid_plural \"%d things found\"",
        "msgstr[0] \"Nothing found\"",
        "msgstr[1] \"%dthing found\"",
    ];

    let mut po_entry = PoEntry::new();

    for line in src {
        assert_eq!(po_entry.parse_line(line), ParseResult::Ok)
    }
    assert!(po_entry.valid);
    assert_eq!(po_entry.parse_line(""), ParseResult::NextEntry);

    let output = po_entry.commit();
    let res: Vec<&str> = output.lines().collect();
    assert_eq!(res[0], "#  translator-comment");
    assert_eq!(res[1], "#  some_other_comment");
    assert_eq!(res[2], "#. code comment");
    assert_eq!(res[3], "#: file.rs:1337");
    assert_eq!(res[4], "#, fuzzy, c-format");
    assert_eq!(res[5], "#| \"blork\"");
    assert_eq!(res[6], "msgid \"No thing found\"");
    assert_eq!(res[7], "msgid_plural \"%d things found\"");
    assert_eq!(res[8], "msgstr[0] \"Nothing found\"");
    assert_eq!(res[9], "msgstr[1] \"%dthing found\"");
}

#[test]
fn invalid_test() {
    let src: Vec<&str> = vec![
        "msgid \"foo\"",
        "somethingelse \"bar\"",
        "msgstr \"bar\"",
    ];

    let mut po_entry = PoEntry::new();
    for line in src {
        assert_eq!(po_entry.parse_line(line), ParseResult::Ok)
    }
    assert!(!po_entry.valid);

    let output = po_entry.commit();
    let res: Vec<&str> = output.lines().collect();
    assert_eq!(res[0], "msgid \"foo\"");
    assert_eq!(res[1], "somethingelse \"bar\"");
    assert_eq!(res[2], "msgstr \"bar\"");
}

 #[test]
 fn complete_file_with_valid_content() {
     let mut input = String::new();
     File::open("corpus/clean.po").unwrap().read_to_string(&mut input).unwrap();
     let mut expected = String::new();
     File::open("corpus/clean.po.res").unwrap().read_to_string(&mut expected).unwrap();
     assert_eq!(parse_po_lines(&input).unwrap(), expected);
 }

 #[test]
 fn complete_file_with_path_conflict() {
     let mut input = String::new();
     File::open("corpus/paths.po").unwrap().read_to_string(&mut input).unwrap();
     let mut expected = String::new();
     File::open("corpus/paths.po.res").unwrap().read_to_string(&mut expected).unwrap();
     assert_eq!(parse_po_lines(&input).unwrap(), expected);
 }

 #[test]
 fn complete_file_with_header_conflict() {
     let mut input = String::new();
     File::open("corpus/header.po").unwrap().read_to_string(&mut input).unwrap();
     let mut expected = String::new();
     File::open("corpus/header.po.res").unwrap().read_to_string(&mut expected).unwrap();
     assert_eq!(parse_po_lines(&input).unwrap(), expected);
 }

 #[test]
 fn complete_file_with_reordered_conflict() {
     let mut input = String::new();
     File::open("corpus/reorder.po").unwrap().read_to_string(&mut input).unwrap();
     let mut expected = String::new();
     File::open("corpus/reorder.po.res").unwrap().read_to_string(&mut expected).unwrap();
     assert_eq!(parse_po_lines(&input).unwrap(), expected);
 }

 #[test]
 fn complete_file_with_reordered_conflict_with_hangover() {
     let mut input = String::new();
     File::open("corpus/hangover.po").unwrap().read_to_string(&mut input).unwrap();
     let mut expected = String::new();
     File::open("corpus/hangover.po.res").unwrap().read_to_string(&mut expected).unwrap();
     assert_eq!(parse_po_lines(&input).unwrap(), expected);
 }
