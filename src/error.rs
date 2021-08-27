//
// YANG - error.
//  Copyright (C) 2021 Toshiaki Takada
//

use quick_error::*;

quick_error! {
    #[derive(Debug)]
    pub enum YangError {
        InvalidComment {
            display("Invalid comment")
        }
        InvalidString {
            display("Invalid string")
        }
        InvalidIdentifier {
            display("Invalid identifier")
        }
        UnexpectedEof {
            display("Unexpected end of file")
        }
        UnexpectedToken(s: String) {
            display("Unexpected token {}", s)
        }
        UnexpectedStatement(line: usize) {
            display("Unexpected statement at line {}", line)
        }
        MissingStatement(s: &'static str) {
            display("Missing statement {}", s)
        }
        TooFewStatement(line: usize, s: String) {
            display("Too few statement {} at line {}", s, line)
        }
        TooManyStatements(line: usize, s: String) {
            display("Too many statements {} at line {}", s, line)
        }
        ArgumentParseError(s: &'static str, line: usize) {
            display("Argument parse error: {} at line {}", s, line)
        }
        PlaceHolder {
            display("placeholder")
        }
    }
}

use std::fmt;

#[derive(Debug)]
pub struct ArgError {
    pub str: &'static str
}

impl fmt::Display for ArgError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Arg error: {}", self.str)
    }
}

impl ArgError {
    pub fn new(str: &'static str) -> ArgError {
        ArgError {
            str
        }
    }
}
