//
// YANG - YANG statement args.
//  Copyright (C) 2021 Toshiaki Takada
//

use std::fmt;
use std::str::FromStr;
use std::string::ToString;
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


///
/// Trait for statement arg.
///
pub trait StmtArg {
    /// Parse token and return StmtArg if it is valid.
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> where Self: Sized;
}

///
/// Yang Identifier.
///
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    str: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.str)
    }
}

impl FromStr for Identifier {
    type Err = YangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with(|c: char| c.is_alphabetic() || c == '_') {
            Err(YangError::ArgumentParseError("identifier".to_string()))
        } else if s.len() > 1 {
            if let Some(_) = &s[1..].find(|c: char| !c.is_alphabetic() && !c.is_ascii_digit() && c != '_' && c != '-' && c != '.') {
                Err(YangError::ArgumentParseError("identifier".to_string()))
            } else {
                Ok(Identifier { str: s.to_string() })
            }
        } else {
            Ok(Identifier { str: s.to_string() })
        }
    }
}

impl StmtArg for Identifier {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        Identifier::from_str(&str)
    }
}

///
/// IdentifierRef.
///
#[derive(Clone, PartialEq)]
pub struct IdentifierRef {
    prefix: Option<Prefix>,
    identifier: Identifier,
}

impl fmt::Debug for IdentifierRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.prefix {
            Some(prefix) => write!(f, "{}:{}", prefix, self.identifier),
            None => write!(f, "{}", self.identifier),
        }
    }
}

impl ToString for IdentifierRef {
    fn to_string(&self) -> String {
        match &self.prefix {
            Some(prefix) => format!("{}:{}", prefix, self.identifier),
            None => format!("{}", self.identifier),
        }
    }
}

impl StmtArg for IdentifierRef {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        match str.find(":") {
            Some(p) => {
                let prefix = Identifier::from_str(&str[..p])?;
                let identifier = Identifier::from_str(&str[p + 1..])?;

                Ok(IdentifierRef { prefix: Some(prefix), identifier})
            }
            None => {
                let identifier = Identifier::from_str(&str)?;
                Ok(IdentifierRef { prefix: None, identifier })
            }
        }
    }
}

// Yang String.
impl StmtArg for String {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        Ok(parse_string(parser)?)
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
}

///
/// The "yang-version-arg".
///
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
}

///
/// Date arg. 
///
#[derive(Debug, Clone)]
pub struct DateArg {
    str: String,
}

impl ToString for DateArg {
    fn to_string(&self) -> String {
        self.str.clone()
    }
}

impl StmtArg for DateArg {
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
}

///
/// Yin Element arg. 
///
#[derive(Debug, Clone)]
pub struct YinElementArg {
    arg: bool,
}

impl StmtArg for YinElementArg {
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
}

///
/// Fraction Digits arg.
///
#[derive(Debug, Clone)]
pub struct FractionDigitsArg {
    digits: u8,
}

impl FractionDigitsArg {
    /// Return fraction digits in unsigned integer.
    fn digits(&self) -> u8 {
        self.digits
    }
}

impl ToString for FractionDigitsArg {
    fn to_string(&self) -> String {
        format!("{}", self.digits)
    }
}

impl StmtArg for FractionDigitsArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        match str.parse::<u8>() {
            Ok(num) if num >= 1 && num <= 18 => Ok(FractionDigitsArg { digits: num }),
            _ => Err(YangError::ArgumentParseError("fraction-digits-arg".to_string()))
        }
    }
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Status {
    Current,
    Obsolete,
    Deprecated,
}

///
/// Status arg.
///
#[derive(Debug, Clone)]
pub struct StatusArg {
    arg: Status,
}

impl StmtArg for StatusArg {
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
}

///
/// Config Arg.
///
#[derive(Debug, Clone)]
pub struct ConfigArg {
    arg: bool,
}

impl StmtArg for ConfigArg {
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
}

///
/// Mandatory Arg.
///
#[derive(Debug, Clone)]
pub struct MandatoryArg {
    arg: bool,
}

impl StmtArg for MandatoryArg {
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
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OrderedBy {
    User,
    System,
}

///
/// Ordered-By arg.
///
#[derive(Debug, Clone)]
pub struct OrderedByArg {
    arg: OrderedBy,
}

impl StmtArg for OrderedByArg {
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
}

///
/// Min Value arg.
///
#[derive(Debug, Clone)]
pub struct MinValueArg {
    val: u64,
}

impl StmtArg for MinValueArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if is_non_negative_integer_value(&str) {
            match str.parse::<u64>() {
                Ok(num) => Ok(MinValueArg { val: num }),
                Err(_) => Err(YangError::ArgumentParseError("min-value-arg".to_string()))
            }
        } else {
            Err(YangError::ArgumentParseError("min-value-arg".to_string()))
        }
    }
}

#[derive(Clone)]
pub enum MaxValue {
    Unbounded,
    Value(u64),
}

impl fmt::Debug for MaxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            MaxValue::Unbounded => write!(f, "unbounded"),
            MaxValue::Value(num) => write!(f, "{}", num),
        }
    }
}

///
/// Max Value arg.
///
#[derive(Debug, Clone)]
pub struct MaxValueArg {
    val: MaxValue,
}

impl StmtArg for MaxValueArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        if str == "unbounded" {
            Ok(MaxValueArg { val: MaxValue::Unbounded })
        } else if is_positive_integer_value(&str) {
            match str.parse::<u64>() {
                Ok(num) => Ok(MaxValueArg { val: MaxValue::Value(num) }),
                Err(_) => Err(YangError::ArgumentParseError("max-value-arg".to_string()))
            }
        } else {
            Err(YangError::ArgumentParseError("max-value-arg".to_string()))
        }
    }
}

///
/// Integer Value str.
///
#[derive(Debug, Clone)]
pub struct IntegerValue {
    val: String,
}

impl StmtArg for IntegerValue {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if is_integer_value(&str) {
            Ok(IntegerValue { val: str })
        } else {
            Err(YangError::ArgumentParseError("integer-value".to_string()))
        }
    }
}

///
/// Ranges.
///
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RangeBoundary {
    Min,
    Max,
    Integer(i64),
    Decimal(f64),
}

impl FromStr for RangeBoundary {
    type Err = YangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rb = s.trim();

        if rb == "min" {
            Ok(RangeBoundary::Min)
        } else if rb == "max" {
            Ok(RangeBoundary::Max)
        } else if is_decimal_value(rb) {
            match s.parse::<f64>() {
                Ok(num) => Ok(RangeBoundary::Decimal(num)),
                Err(_) => Err(YangError::ArgumentParseError("range-arg".to_string())),
            }
        } else if is_integer_value(rb) {
            match s.parse::<i64>() {
                Ok(num) => Ok(RangeBoundary::Integer(num)),
                Err(_) => Err(YangError::ArgumentParseError("range-arg".to_string())),
            }
        } else {
            Err(YangError::ArgumentParseError("range-arg".to_string()))
        }
    }
}

pub type Range = (RangeBoundary, Option<RangeBoundary>);

pub struct RangeArg {
    ranges: Vec<Range>,
}

impl StmtArg for RangeArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        let rp: Vec<_> = str.split('|').collect();
        let mut v = Vec::new();

        for r in rp {
            let srb;
            let erb;

            let rb: Vec<_> = r.split("..").collect();

            if rb.len() == 1 {
                srb = RangeBoundary::from_str(rb[0])?;
                erb = None;
            } else if rb.len() == 2 {
                srb = RangeBoundary::from_str(rb[0])?;
                erb = Some(RangeBoundary::from_str(rb[1])?);
            } else {
                return Err(YangError::ArgumentParseError("range-arg".to_string()));
            }

            v.push((srb, erb));
        }

        Ok(RangeArg { ranges: v })
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
            Ok(arg) => assert_eq!(arg.to_string(), "prefix:hello-world"),
            Err(_) => assert!(false),
        }

        let s = " _prefix_:_123.IdEnT.456-789_ ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.to_string(), "_prefix_:_123.IdEnT.456-789_"),
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
            Ok(arg) => assert_eq!(arg.to_string(), "_123:_456"),
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
            Ok(arg) => assert_eq!(arg.to_string(), "2021-08-01"),
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
            Ok(arg) => assert_eq!(arg.digits(), 18),
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

