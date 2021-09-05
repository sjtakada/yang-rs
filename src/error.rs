//
// YANG - error.
//  Copyright (C) 2021 Toshiaki Takada
//

use quick_error::*;

quick_error! {
    #[derive(Debug)]
    pub enum YangError {
        IoError(s: String) {
            display("io::Error: {}", s)
        }
        InvalidComment {
            display("Invalid comment")
        }
        InvalidString(s: String) {
            display("Invalid string '{}'", s)
        }
//        InvalidIdentifier {
//            display("Invalid identifier")
//        }
        UnexpectedEof {
            display("Unexpected end of file")
        }
        UnexpectedToken(s: String) {
            display("Unexpected token {}", s)
        }
        UnexpectedStatement(s: String) {
            display("Unexpected statement: {}", s)
        }
        MissingStatement(s: &'static str) {
            display("Missing statement {}", s)
        }
        TooFewStatement(s: String) {
            display("Too few statement {}", s)
        }
        TooManyStatements(s: String) {
            display("Too many statements {}", s)
        }
        ArgumentParseError(s: &'static str) {
            display("Argument parse error {}", s)
        }
        PlaceHolder {
            display("placeholder")
        }
    }
}

use std::fmt;

#[derive(Debug)]
pub struct ArgError {
    pub str: &'static str,
}

impl fmt::Display for ArgError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Arg error: {}", self.str)
    }
}

impl ArgError {
    pub fn new(str: &'static str) -> ArgError {
        ArgError { str }
    }
}
