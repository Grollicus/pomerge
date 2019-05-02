// TODO merge paths
// TODO merge nochange
// TODO merge Header
// TODO merge multiple
// TODO support plural
// TODO merge strings so that changes in the string splitting can be resolved automatically

extern crate regex;
#[macro_use] extern crate lazy_static;

#[cfg(test)]
use pretty_assertions::assert_eq;

use std::collections::HashMap;
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

#[derive(Clone)]
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

#[derive(Clone)]
struct PoEntry<'l> {
    valid: bool,
    repeat_type: RepeatType,
    input: String,
    translator_comments: Vec<&'l str>,
    extracted_comments: Vec<&'l str>,
    references: Vec<&'l str>,
    flags: Vec<&'l str>,
    previous_untranslated_strings: Vec<&'l str>,
    msgctxts: Vec<&'l str>,
    msgids: Vec<&'l str>,
    msgid_plurals: Vec<&'l str>,
    msgstrs: Vec<&'l str>,
    msgstr_plurals: HashMap<u32, Vec<&'l str>>,
}

fn _print_with_title<'l>(title: &str, lines: &Vec<&'l str>, result: &mut String) {
    if lines.len() == 0 {
        return
    }

    let mut first = true;
    for line in lines {
        if first {
            result.push_str(&format!("{} {}\n", title, *line));
            first = false;
        } else {
            result.push_str(*line);
            result.push('\n');
        }
    }
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
            msgctxts: vec!(),
            msgids: vec!(),
            msgid_plurals: vec!(),
            msgstrs: vec!(),
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

        self.msgctxts.push(&line[8..]);
        self.repeat_type = RepeatType::Msgctxt;
    }
    fn msgid(&mut self, line: &'l str) {
        self.input.push_str(line);
        self.input.push('\n');
        if self.msgids.len() != 0 {
            println!("Warning: Repeated msgid in line {}", line);
            self.valid = false;
            return;
        }

        self.msgids.push(&line[6..]);
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

        self.msgid_plurals.push(&line[13..]);
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

        self.msgstrs.push(&line[7..]);
        self.repeat_type = RepeatType::Msgstr;
    }
    fn _add_plural(&mut self, n: u32, value: &'l str) {
        let plurals = match self.msgstr_plurals.get_mut(&n) {
            Some(v) => v,
            None => {
                self.msgstr_plurals.insert(n, vec!());
                self.msgstr_plurals.get_mut(&n).unwrap()
            }
        };
        plurals.push(value);
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

        match &self.repeat_type {
            RepeatType::Msgid => self.msgids.push(line),
            RepeatType::Msgstr => self.msgstrs.push(line),
            RepeatType::Msgctxt => self.msgctxts.push(line),
            RepeatType::MsgidPlural => self.msgid_plurals.push(line),
            RepeatType::MsgstrPlural(n) => self._add_plural(*n, &line),
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
            // println!("Error: Invalid line {:?}", line);

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
    pub fn commit(self, result: &mut String) {
        if !self.is_valid() {
            result.push_str(&self.input);
            return;
        }
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
        _print_with_title("msgctxt", &self.msgctxts, result);
        _print_with_title("msgid", &self.msgids, result);
        _print_with_title("msgid_plural", &self.msgid_plurals, result);
        _print_with_title("msgstr", &self.msgstrs, result);

        let mut keys: Vec<&u32> = (&self.msgstr_plurals).keys().collect();
        keys.sort();
        for n in keys {
            let lines = &self.msgstr_plurals[n];
            _print_with_title(&format!("msgstr[{}]", n), &lines, result);
        }
    }
    pub fn has_content(&self) -> bool {
        return self.translator_comments.len() > 0 || self.extracted_comments.len() > 0 || self.references.len() > 0 || self.flags.len() > 0 || self.previous_untranslated_strings.len() > 0 || self.msgids.len() > 0 || self.msgstrs.len() > 0
    }
    fn is_valid(&self) -> bool {
        return self.valid && (self.msgids.len() > 0 || self.msgid_plurals.len() > 0) && (self.msgstrs.len() > 0 || self.msgstr_plurals.len() > 0)
    }
}

pub fn parse_po_lines(lines: &str) -> Result<String, MyErr> {
    let mut current_entry = PoEntry::new();
    let mut result = String::new();
    // let mut first = true;  // TODO handle the first newline in the header handling and remove `first` here.

    for line in lines.lines() {
        print!("Parsing {:?}.. ", line);
        match current_entry.parse_line(line) {
            ParseResult::Ok => {println!("Got ok")},
            ParseResult::NextEntry => {
                println!("Got NextEntry");
                current_entry.commit(&mut result);
                result.push_str(line);
                result.push('\n');
                current_entry = PoEntry::new();
            }
        }
    };

    if current_entry.has_content() {
        current_entry.commit(&mut result);
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

    let mut output = String::new();
    po_entry.commit(&mut output);

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

    let mut output = String::new();
    po_entry.commit(&mut output);

    println!("Output: {}", output);
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

    let mut output = String::new();
    po_entry.commit(&mut output);

    let res: Vec<&str> = output.lines().collect();
    assert_eq!(res[0], "msgid \"foo\"");
    assert_eq!(res[1], "somethingelse \"bar\"");
    assert_eq!(res[2], "msgstr \"bar\"");
}

 #[test]
 fn completet_file_with_valid_content() {
     let mut s = String::new();
     File::open("corpus/clean.po").unwrap().read_to_string(&mut s).unwrap();
     assert_eq!(parse_po_lines(&s).unwrap(), s);
 }

 #[test]
 fn completet_file_with_invalid_content() {
     let mut s = String::new();
     File::open("corpus/paths.po").unwrap().read_to_string(&mut s).unwrap();
     assert_eq!(parse_po_lines(&s).unwrap(), s);
 }

 #[test]
 fn completet_file_with_invalid_content2() {
     let mut s = String::new();
     File::open("corpus/nochange.po").unwrap().read_to_string(&mut s).unwrap();
     assert_eq!(parse_po_lines(&s).unwrap(), s);
 }
