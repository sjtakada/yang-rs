//
// YANG.rs
//   Copyright (C) 2021 Toshiaki Takada
//

use std::error::Error;
use std::fmt::{self, Display};

///
/// YANG Parse Error.
///
#[derive(Debug, PartialEq)]
pub enum YangParseError {
    /// Invalid comment.
    InvalidComment(String),
    /// Invalid string literal.
    InvalidStringLiteral(String),
    /// Invalid string concatenation.
    InvalidStringConcatenation,
    /// Quoted string not closed.
    QuotedStringNotClosed,
}

impl Display for YangParseError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let msg = match *self {
            YangParseError::InvalidComment(ref s) => format!("Invalid comment: {}", s),
            YangParseError::InvalidStringLiteral(ref s) => format!("Invalid string literal: {}", s),
            YangParseError::InvalidStringConcatenation => format!("Invalid string concatenation"),
            YangParseError::QuotedStringNotClosed => format!("Quoted string not closed"),
        };
        write!(fmt, "{}", msg)
    }
}

impl Error for YangParseError {
    fn description(&self) -> &str {
        match *self {
            YangParseError::InvalidComment(_) => "Invalid comment",
            YangParseError::InvalidStringLiteral(_) => "Invalid string literal",
            YangParseError::InvalidStringConcatenation => "Invalid string concatenation",
            YangParseError::QuotedStringNotClosed => "Quoted string not closed",
        }
    }
}
