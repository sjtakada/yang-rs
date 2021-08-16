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
            from(err: ArgError) -> (err.str, 0)
            display("Argument parse error: {} at line {}", s, line)
        }
//        ArgumentParseError(s: &'static str) {
//            display("Argument parse error: {} at line ", s)
//        }
        PlaceHolder {
            display("placeholder")
        }
    }
}

pub struct ArgError {
    pub str: &'static str
}

impl ArgError {
    pub fn new(str: &'static str) -> ArgError {
        ArgError {
            str
        }
    }
}

//impl ToString for ArgError {
//    fn to_string(&self) -> String {
//        format!("{}", "hoge")
//    }
//}
