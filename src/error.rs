//
// YANG - error.
//  Copyright (C) 2021 Toshiaki Takada
//

use quick_error::*;

quick_error! {
    #[derive(Debug)]
    pub enum YangError {
        TokenParseError {
            display("Token parse error")
        }
        InvalidComment {
            display("Invalid comment")
        }
        InvalidString {
            display("Invalid string")
        }
        InvalidIdentifier {
            display("Invalid identifier")
        }
        NoSuchRulename(s: String) {
            display("No such rulename {}", s)
        }
        UnexpectedEof {
            display("Unexpected end of file")
        }
        UnexpectedToken(line: usize) {
            display("Unexpected token at line {}", line)
        }
        UnexpectedStatement(line: usize) {
            display("Unexpected statement at line {}", line)
        }
        StatementMismatch(s: &'static str) {
            display("Number of statements mismatch {}", s)
        }
        MissingStatement(s: &'static str) {
            display("Missing statement {}", s)
        }
        MethodNotImplemented {
            display("Method not implemented")
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

pub struct ArgError {
    pub str: &'static str
}

impl fmt::Display for ArgError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "An Error Occurred, Please Try Again!")
    }
}

impl fmt::Debug for ArgError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{ file: {}, line: {} }}", file!(), line!())
    }
}

impl ArgError {
    pub fn new(str: &'static str) -> ArgError {
        ArgError {
            str
        }
    }
}
