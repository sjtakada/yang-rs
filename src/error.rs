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
        PlaceHolder {
            display("placeholder")
        }
    }
}
