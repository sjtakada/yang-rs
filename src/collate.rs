//
// YANG - Collate
//  Copyright (C) 2021 Toshiaki Takada
//

use std::collections::HashMap;
use super::error::*;

/// String helper.
fn is_integer_value(s: &str) -> bool {
    let mut chars = s.chars();
    let mut c = chars.next();

    match c {
        None => return false,
        Some('-') => c = chars.next(),
        _ => {},
    }

    match c {
        None => return false,
        Some(c) if c == '0' => {
            match chars.next() {
                None => return true,
                _ => return false,
            }
        }
        Some(c) if !c.is_digit(10) => return false,
        Some(_) => {},
    }

    for c in chars {
        if !c.is_digit(10) {
            return false;
        }
    }

    true
}

fn is_decimal_value(s: &str) -> bool {
    if let Some(p) = s.find('.') {
        let is = &s[..p];
        let fs = &s[p + 1..];

        if is_integer_value(is) && fs.len() > 0 {
            return fs.chars().all(|c: char| c.is_digit(10));
        }
    }

    false
}

fn is_non_zero_digit(s: &str) -> bool {
    s.chars().all(|c: char| c >= '1' && c <= '9')
}

fn is_identifier(s: &str) -> bool {
    if let Some(c) = s.chars().next() {
        if !c.is_alphabetic() && c != '_' {
            return false;
        }

        let s = &s[1..];
        if s.len() == 0 {
            return true;
        }

        if let None = s.find(|c: char| !c.is_alphabetic() && !c.is_digit(10) && c != '_' && c != '-' && c != '.') {
            return true;
        }
    }

    false
}

/* TODO
  unknown-statement

  prefix:identifier name {
    .....
  }

  prefix:identifier name;

  prefix:identifner name {
  } prefix:identifier name {
  } ...

*/

/// Core rules and other primitives
// non-zero-digit: only used in positive-integer-value
// SQUOTE: only used in quoted-strin

// ALPHA:  char::is_alphabetic
// CR: only used in line-break and CRLF

//fn is_crlf(s: &str) -> bool {
//    s == "\r\n"
//}

//fn is_char_digit(s: &str) -> bool {
//    s.chars().all(|c: char| c.is_digit(10))
//}

// DIGIT: |c: char| c.is_digit(10)
// DQUOTE: only used in quoted-string
// HTAB = %x09: only used in WSP
// LF = %x0A: only used in CRLF and line-break
// SP = %x20: only used in WSP

// WSP = SP / HTAB
fn is_char_wsp(c: char) -> bool {
    c == ' ' || c == '\t'
}


pub struct CoreRule {
    rules: HashMap<String, Box<dyn Fn(&str) -> bool>>,
}

impl CoreRule {
    pub fn new() -> CoreRule {
        let mut cr = CoreRule {
            rules: HashMap::new(),
        };

        cr.rules.insert("integer-value".to_string(), Box::new(|s: &str| {
            is_integer_value(s)
        }));

        cr.rules.insert("sep".to_string(), Box::new(|s: &str| {
            s.len() > 0 && s.chars().all(|c: char| c == ' ' || c == '\t' || c == '\r' || c == '\n')
        }));

        cr.rules.insert("optsep".to_string(), Box::new(|s: &str| {
            s.chars().all(|c: char| c == ' ' || c == '\t' || c == '\r' || c == '\n')
        }));

// How to get stmtsep token.

//        cr.rules.insert("stmtsep".to_string(), Box::new(|s: &str| {
//        }));

        cr.rules.insert("decimal-value".to_string(), Box::new(|s: &str| {
            is_decimal_value(s)
        }));

//        cr.rules.insert("".to_string(), Box::new(|s: &str| {
//        }));

        cr
    }

    pub fn collate(&self, rulename: &str, input: &str) -> Result<bool, YangError> {
        match self.rules.get(rulename) {
            Some(f) => Ok(f(input)),
            None => Err(YangError::NoSuchRulename(rulename.to_string())),
        }
    }
}

///
/// Unit tests.
///
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_integer_value() {
        let cr = CoreRule::new();

        match cr.collate("integer-value", "0") {
            Ok(true) => {},
            _ => assert!(false),
        }

        match cr.collate("integer-value", "-") {
            Ok(false) => {},
            _ => assert!(false),
        }

        match cr.collate("integer-value", "-0") {
            Ok(true) => {},
            _ => assert!(false),
        }

        match cr.collate("integer-value", "-01") {
            Ok(false) => {},
            _ => assert!(false),
        }

        match cr.collate("integer-value", "01") {
            Ok(false) => {},
            _ => assert!(false),
        }

        match cr.collate("integer-value", "1") {
            Ok(true) => {},
            _ => assert!(false),
        }

        match cr.collate("integer-value", "1234567890") {
            Ok(true) => {},
            _ => assert!(false),
        }

        match cr.collate("integer-value", "a") {
            Ok(false) => {},
            _ => assert!(false),
        }

        match cr.collate("integer-value", "1a") {
            Ok(false) => {},
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test_decimal_value() {
        let cr = CoreRule::new();

        match cr.collate("decimal-value", "0") {
            Ok(false) => {},
            _ => assert!(false),
        }

        match cr.collate("decimal-value", "0.0") {
            Ok(true) => {},
            _ => assert!(false),
        }

        match cr.collate("decimal-value", "-0.0") {
            Ok(true) => {},
            _ => assert!(false),
        }

        match cr.collate("decimal-value", "1.0") {
            Ok(true) => {},
            _ => assert!(false),
        }

        match cr.collate("decimal-value", "-1.0") {
            Ok(true) => {},
            _ => assert!(false),
        }

        match cr.collate("decimal-value", "123.456789") {
            Ok(true) => {},
            _ => assert!(false),
        }

        match cr.collate("decimal-value", "123.456789.123") {
            Ok(false) => {},
            _ => assert!(false),
        }

        match cr.collate("decimal-value", "-.123") {
            Ok(false) => {},
            _ => assert!(false),
        }
    }
}

