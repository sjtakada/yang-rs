//
// YANG - YANG statement args.
//  Copyright (C) 2021 Toshiaki Takada
//

use url::Url;

use super::error::*;
use super::parser::*;

// TBD
//
//   yang-string         = *yang-char
//
//   ;; any Unicode or ISO/IEC 10646 character, including tab, carriage
//   ;; return, and line feed but excluding the other C0 control
//   ;; characters, the surrogate blocks, and the noncharacters
//   yang-char = %x09 / %x0A / %x0D / %x20-D7FF /
//                               ; exclude surrogate blocks %xD800-DFFF
//              %xE000-FDCF /    ; exclude noncharacters %xFDD0-FDEF
//              %xFDF0-FFFD /    ; exclude noncharacters %xFFFE-FFFF
//              %x10000-1FFFD /  ; exclude noncharacters %x1FFFE-1FFFF
//              %x20000-2FFFD /  ; exclude noncharacters %x2FFFE-2FFFF
//              %x30000-3FFFD /  ; exclude noncharacters %x3FFFE-3FFFF
//              %x40000-4FFFD /  ; exclude noncharacters %x4FFFE-4FFFF
//              %x50000-5FFFD /  ; exclude noncharacters %x5FFFE-5FFFF
//              %x60000-6FFFD /  ; exclude noncharacters %x6FFFE-6FFFF
//              %x70000-7FFFD /  ; exclude noncharacters %x7FFFE-7FFFF
//              %x80000-8FFFD /  ; exclude noncharacters %x8FFFE-8FFFF
//              %x90000-9FFFD /  ; exclude noncharacters %x9FFFE-9FFFF
//              %xA0000-AFFFD /  ; exclude noncharacters %xAFFFE-AFFFF
//              %xB0000-BFFFD /  ; exclude noncharacters %xBFFFE-BFFFF
//              %xC0000-CFFFD /  ; exclude noncharacters %xCFFFE-CFFFF
//              %xD0000-DFFFD /  ; exclude noncharacters %xDFFFE-DFFFF
//              %xE0000-EFFFD /  ; exclude noncharacters %xEFFFE-EFFFF
//              %xF0000-FFFFD /  ; exclude noncharacters %xFFFFE-FFFFF
//              %x100000-10FFFD  ; exclude noncharacters %x10FFFE-10FFFF
// 
// YANG string, quoted or unquoted.
fn parse_string(parser: &mut Parser) -> Result<String, YangError> {
    let token = parser.get_token()?;
    match token {
        // Statement argument.
        Token::Identifier(s) |
        Token::QuotedString(s) => Ok(s),
        // 
        Token::EndOfInput => Err(YangError::UnexpectedEof),
        // Unexpected Token.
        _ => Err(YangError::UnexpectedToken(parser.line())),
    }
}


//
// Trait for statement arg.
//
pub trait StmtArg {
    /// Parse token and return StmtArg if it is valid.
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> where Self: Sized;

    /// Get argment into string.
    fn get_arg(&self) -> String;
}

// Yang Identifier.
#[derive(Debug, Clone)]
pub struct Identifier {
    str: String,
}

impl StmtArg for Identifier {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        if !str.starts_with(|c: char| c.is_alphabetic() || c == '_') {
            return Err(YangError::InvalidIdentifier);
        }

        if str.len() > 1 {
            if let Some(_) = &str[1..].find(|c: char| !c.is_alphabetic() && !c.is_digit(10) && c != '_' && c != '-' && c != '.') {
                return Err(YangError::InvalidIdentifier);
            }
        }
        Ok(Identifier { str })
    }

    fn get_arg(&self) -> String {
        self.str.clone()
    }
}

// Yang String.
impl StmtArg for String {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        Ok(parse_string(parser)?)
    }

    fn get_arg(&self) -> String {
        self.clone()
    }
}

// URL string.
impl StmtArg for Url {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let s = parse_string(parser)?;

        match Url::parse(&s) {
            Ok(url) => Ok(url),
            Err(err) => Err(YangError::ArgumentParseError(err.to_string())),
        }
    }

    fn get_arg(&self) -> String {
        self.to_string()
    }
}

// Yang Version String.
#[derive(Debug, Clone)]
pub struct YangVersionArg {
    str: String,
}

impl StmtArg for YangVersionArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        if str == "1.1" {
            Ok(YangVersionArg { str })
        } else {
            Err(YangError::ArgumentParseError(format!("Invalid Yang Version {}", str)))
        }
    }

    fn get_arg(&self) -> String {
        self.str.clone()
    }
}

