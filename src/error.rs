//
// YANG - error.
//  Copyright (C) 2021 Toshiaki Takada
//

use super::parser::*;
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
        MissingStatement {
            display("Missing statement")
        }
        MethodNotImplemented {
            display("Method not implemented")
        }
        PlaceHolder {
            display("placeholder")
        }
    }
}
