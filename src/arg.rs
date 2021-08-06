//
// YANG - YANG statement args.
//  Copyright (C) 2021 Toshiaki Takada
//

use std::fmt;
use url::Url;

use super::core::*;
use super::error::*;
use super::parser::*;

// Aliases.
pub type Prefix = Identifier;
pub type NodeIdentifier = IdentifierRef;

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
    /// Arg value type.
    type Value;

    /// Parse token and return StmtArg if it is valid.
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> where Self: Sized;

    /// Get argment into string.
    fn get_arg(&self) -> Self::Value;
}

// Yang Identifier.
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    str: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.str)
    }
}

impl Identifier {
    pub fn from(str: &str) -> Identifier {
        Identifier {
            str: String::from(str),
        }
    }

    pub fn validate(str: &str) -> bool {
        if !str.starts_with(|c: char| c.is_alphabetic() || c == '_') {
            false
        } else if str.len() > 1 {
            if let Some(_) = &str[1..].find(|c: char| !c.is_alphabetic() && !c.is_ascii_digit() && c != '_' && c != '-' && c != '.') {
                false
            } else {
                true
            }
        } else {
            true
        }
    }
}

impl StmtArg for Identifier {
    type Value = String;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        if !Identifier::validate(&str) {
            Err(YangError::InvalidIdentifier)
        } else {
            Ok(Identifier { str })
        }
    }

    fn get_arg(&self) -> String {
        self.str.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierRef {
    prefix: Option<Prefix>,
    identifier: Identifier,
}

impl StmtArg for IdentifierRef {
    type Value = String;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        match str.find(":") {
            Some(p) => {
                let prefix_str = &str[..p];
                let identifier_str = &str[p+1..];

                if Identifier::validate(&prefix_str) && Identifier::validate(&identifier_str) {
                    Ok(IdentifierRef {
                        prefix: Some(Identifier::from(prefix_str)),
                        identifier: Identifier::from(identifier_str) })
                } else {
                    Err(YangError::InvalidIdentifier)
                }
            }
            None => {
                if Identifier::validate(&str) {
                    Ok(IdentifierRef { prefix: None, identifier: Identifier::from(&str) })
                } else {
                    Err(YangError::InvalidIdentifier)
                }
            }
        }
    }

    fn get_arg(&self) -> String {
        match &self.prefix {
            Some(prefix) => format!("{}:{}", prefix, self.identifier),
            None => self.identifier.to_string(),
        }
    }
}

// Yang String.
impl StmtArg for String {
    type Value = String;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        Ok(parse_string(parser)?)
    }

    fn get_arg(&self) -> String {
        self.clone()
    }
}

// URL string.
impl StmtArg for Url {
    type Value = Url;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let s = parse_string(parser)?;

        match Url::parse(&s) {
            Ok(url) => Ok(url),
            Err(err) => Err(YangError::ArgumentParseError(err.to_string())),
        }
    }

    fn get_arg(&self) -> Url {
        self.clone()
    }
}

// Yang Version String.
#[derive(Debug, Clone)]
pub struct YangVersionArg {
    str: String,
}

impl StmtArg for YangVersionArg {
    type Value = String;

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

// Date arg. 
#[derive(Debug, Clone)]
pub struct DateArg {
    str: String,
}

impl StmtArg for DateArg {
    type Value = String;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        if str.chars().count() == 10 {
            if let Some(_) = &str[0..4].find(|c: char| !c.is_ascii_digit()) {
                Err(YangError::ArgumentParseError("date-arg".to_string()))
            } else if str.chars().nth(4).unwrap() != '-' {
                Err(YangError::ArgumentParseError("date-arg".to_string()))
            } else if let Some(_) = &str[5..7].find(|c: char| !c.is_ascii_digit()) {
                Err(YangError::ArgumentParseError("date-arg".to_string()))
            } else if str.chars().nth(7).unwrap() != '-' {
                Err(YangError::ArgumentParseError("date-arg".to_string()))
            } else if let Some(_) = &str[8..10].find(|c: char| !c.is_ascii_digit()) {
                Err(YangError::ArgumentParseError("date-arg".to_string()))
            } else {
                Ok(DateArg { str })
            }
        } else {
            Err(YangError::ArgumentParseError("date-arg".to_string()))
        }
    }

    fn get_arg(&self) -> String {
        self.str.clone()
    }
}

// Yin Element arg. 
#[derive(Debug, Clone)]
pub struct YinElementArg {
    arg: bool,
}

impl StmtArg for YinElementArg {
    type Value = bool;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str == "true" {
            Ok(YinElementArg { arg: true })
        } else if str == "false" {
            Ok(YinElementArg { arg: false })
        } else {
            Err(YangError::ArgumentParseError("yin-element-arg".to_string()))
        }
    }

    fn get_arg(&self) -> bool {
        self.arg
    }
}

// Fraction Digits arg.
#[derive(Debug, Clone)]
pub struct FractionDigitsArg {
    digits: u8,
}

impl StmtArg for FractionDigitsArg {
    type Value = u8;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        match str.parse::<u8>() {
            Ok(num) if num >= 1 && num <= 18 => Ok(FractionDigitsArg { digits: num }),
            _ => Err(YangError::ArgumentParseError("fraction-digits-arg".to_string()))
        }
    }

    fn get_arg(&self) -> u8 {
        self.digits
    }
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Status {
    Current,
    Obsolete,
    Deprecated,
}

// Status arg.
#[derive(Debug, Clone)]
pub struct StatusArg {
    arg: Status,
}

impl StmtArg for StatusArg {
    type Value = Status;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str == "current" {
            Ok(StatusArg { arg: Status::Current })
        } else if str == "obsolete" {
            Ok(StatusArg { arg: Status::Obsolete })
        } else if str == "deprecated" {
            Ok(StatusArg { arg: Status::Deprecated })
        } else {
            Err(YangError::ArgumentParseError("status-arg".to_string()))
        }
    }

    fn get_arg(&self) -> Status {
        self.arg
    }
}

// Config Arg.
#[derive(Debug, Clone)]
pub struct ConfigArg {
    arg: bool,
}

impl StmtArg for ConfigArg {
    type Value = bool;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str == "true" {
            Ok(ConfigArg { arg: true })
        } else if str == "false" {
            Ok(ConfigArg { arg: false })
        } else {
            Err(YangError::ArgumentParseError("config-arg".to_string()))
        }
    }

    fn get_arg(&self) -> bool {
        self.arg
    }
}

// Mandatory Arg.
#[derive(Debug, Clone)]
pub struct MandatoryArg {
    arg: bool,
}

impl StmtArg for MandatoryArg {
    type Value = bool;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str == "true" {
            Ok(MandatoryArg { arg: true })
        } else if str == "false" {
            Ok(MandatoryArg { arg: false })
        } else {
            Err(YangError::ArgumentParseError("mandatory-arg".to_string()))
        }
    }

    fn get_arg(&self) -> bool {
        self.arg
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OrderedBy {
    User,
    System,
}

// Ordered-By arg.
#[derive(Debug, Clone)]
pub struct OrderedByArg {
    arg: OrderedBy,
}

impl StmtArg for OrderedByArg {
    type Value = OrderedBy;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str == "user" {
            Ok(OrderedByArg { arg: OrderedBy::User })
        } else if str == "system" {
            Ok(OrderedByArg { arg: OrderedBy::System })
        } else {
            Err(YangError::ArgumentParseError("ordered-by-arg".to_string()))
        }
    }

    fn get_arg(&self) -> OrderedBy {
        self.arg
    }
}

// Min Value arg.
#[derive(Debug, Clone)]
pub struct MinValueArg {
    val: String,
}

impl StmtArg for MinValueArg {
    type Value = String;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if is_non_negative_integer_value(&str) {
            Ok(MinValueArg { val: str })
        } else {
            Err(YangError::ArgumentParseError("min-value-arg".to_string()))
        }
    }

    fn get_arg(&self) -> String {
        self.val.clone()
    }
}

// Max Value arg.
#[derive(Debug, Clone)]
pub struct MaxValueArg {
    unbounded: bool,

    val: String,
}

impl StmtArg for MaxValueArg {
    type Value = String;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        if str == "unbounded" {
            Ok(MaxValueArg { unbounded: true, val: String::new() })
        } else if is_positive_integer_value(&str) {
            Ok(MaxValueArg { unbounded: false, val: str })
        } else {
            Err(YangError::ArgumentParseError("max-value-arg".to_string()))
        }
    }

    fn get_arg(&self) -> String {
        if self.unbounded {
            "unbounded".to_string()
        } else {
            self.val.clone()
        }
    }
}

// Integer Value str.
#[derive(Debug, Clone)]
pub struct IntegerValue {
    val: String,
}

impl StmtArg for IntegerValue {
    type Value = String;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if is_integer_value(&str) {
            Ok(IntegerValue { val: str })
        } else {
            Err(YangError::ArgumentParseError("integer-value".to_string()))
        }
    }

    fn get_arg(&self) -> String {
        self.val.clone()
    }
}

// Ranges.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RangeBoundary {
    Min,
    Max,
    Integer(i64),
    Decimal(f64),
}

pub type Range = (RangeBoundary, Option<RangeBoundary>);

pub struct RangeArg {
    ranges: Vec<Range>,
}

impl StmtArg for RangeArg {
    type Value = Vec<Range>;

    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        let rp: Vec<_> = str.split('|').collect();
        let mut v = Vec::new();

        for r in rp {
            let srb;
            let erb;

            let rb: Vec<_> = str.split("..").collect();

            if rb.len() == 1 {
                srb = parse_range_boundary(rb[0])?;
                erb = None;
            } else if rb.len() == 2 {
                srb = parse_range_boundary(rb[0])?;
                erb = Some(parse_range_boundary(rb[1])?);
            } else {
                return Err(YangError::ArgumentParseError("range-arg".to_string()));
            }

            v.push((srb, erb));
        }

        Ok(RangeArg { ranges: v })
    }

    fn get_arg(&self) -> Vec<Range> {
        self.ranges.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_arg_identifier() {
        let s = " hello-world ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, Identifier { str: String::from("hello-world") }),
            Err(_) => assert!(false),
        }

        let s = " _123.IdEnT.456-789_ ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, Identifier { str: String::from("_123.IdEnT.456-789_") }),
            Err(_) => assert!(false),
        }

        let s = " 123 ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Invalid identifier"),
        }

        let s = " 123$ ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Invalid identifier"),
        }
    }

    #[test]
    pub fn test_arg_identifier_ref() {
        let s = " prefix:hello-world ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.get_arg(), "prefix:hello-world"),
            Err(_) => assert!(false),
        }

        let s = " _prefix_:_123.IdEnT.456-789_ ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.get_arg(), "_prefix_:_123.IdEnT.456-789_"),
            Err(_) => assert!(false),
        }

        let s = " 123:456 ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Invalid identifier"),
        }

        let s = " _123:_456 ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.get_arg(), "_123:_456"),
            Err(_) => assert!(false),
        }

        let s = " _123: ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Invalid identifier"),
        }
    }

    #[test]
    pub fn test_arg_date_arg() {
        let s = " 2021-08-01 ";
        let mut parser = Parser::new(s.to_string());

        match DateArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.get_arg(), "2021-08-01"),
            Err(_) => assert!(false),
        }

        let s = " 2021-8-1 ";
        let mut parser = Parser::new(s.to_string());

        match DateArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: date-arg"),
        }

        let s = " 08-01-2021 ";
        let mut parser = Parser::new(s.to_string());

        match DateArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: date-arg"),
        }

        let s = " 2021-08-0x ";
        let mut parser = Parser::new(s.to_string());

        match DateArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: date-arg"),
        }
    }

    #[test]
    pub fn test_arg_fraction_digits() {
        let s = "18";
        let mut parser = Parser::new(s.to_string());

        match FractionDigitsArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.get_arg(), 18),
            Err(_) => assert!(false),
        }

        let s = "0";
        let mut parser = Parser::new(s.to_string());

        match FractionDigitsArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: fraction-digits-arg"),
        }

        let s = "19";
        let mut parser = Parser::new(s.to_string());

        match FractionDigitsArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: fraction-digits-arg"),
        }
    }
}

