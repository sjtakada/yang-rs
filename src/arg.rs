//
// YANG - YANG statement args.
//  Copyright (C) 2021 Toshiaki Takada
//

use std::fmt;
use std::iter::Iterator;
use std::str::FromStr;
use std::string::ToString;
use url::Url;

use derive_getters::Getters;

use super::core::*;
use super::error::*;
use super::parser::*;

// Aliases.
pub type Prefix = Identifier;

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
        Token::Identifier(s) | Token::QuotedString(s) => Ok(s),
        // End of Input.
        Token::EndOfInput => Err(YangError::UnexpectedEof),
        // Unexpected Token.
        _ => Err(YangError::UnexpectedToken(token.to_string())),
    }
}

///
/// Trait for statement arg.
///
pub trait StmtArg {
    /// Parse token and return StmtArg if it is valid.
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError>
    where
        Self: Sized;
}

///
/// No Arg (for "input-stmt", "output-stmt").
///
#[derive(Debug, Clone, PartialEq)]
pub struct NoArg;

impl StmtArg for NoArg {
    fn parse_arg(_parser: &mut Parser) -> Result<Self, YangError> {
        Ok(NoArg)
    }
}

///
/// Yang Identifier.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct Identifier {
    str: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.str)
    }
}

impl FromStr for Identifier {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if is_identifier(s) {
            Ok(Identifier { str: s.to_string() })
        } else {
            Err(ArgError::new("identifier"))
        }
    }
}

impl StmtArg for Identifier {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        match Identifier::from_str(&str) {
            Ok(arg) => Ok(arg),
            Err(e) => Err(YangError::ArgumentParseError(e.str)),
        }
    }
}

///
/// "identity-ref".
///
#[derive(Clone, PartialEq, Getters)]
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

impl FromStr for IdentifierRef {
    type Err = ArgError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        match str.find(":") {
            Some(p) => {
                let prefix = Identifier::from_str(&str[..p])?;
                let identifier = Identifier::from_str(&str[p + 1..])?;

                Ok(IdentifierRef {
                    prefix: Some(prefix),
                    identifier,
                })
            }
            None => {
                let identifier = Identifier::from_str(&str)?;
                Ok(IdentifierRef {
                    prefix: None,
                    identifier,
                })
            }
        }
    }
}

impl StmtArg for IdentifierRef {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        IdentifierRef::from_str(&str).map_err(|e| YangError::ArgumentParseError(e.str))
    }
}

///
/// "node-identifier".
///
#[derive(Clone, PartialEq, Getters)]
pub struct NodeIdentifier {
    prefix: Option<Prefix>,
    identifier: Identifier,
}

impl fmt::Debug for NodeIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.prefix {
            Some(prefix) => write!(f, "{}:{}", prefix, self.identifier),
            None => write!(f, "{}", self.identifier),
        }
    }
}

impl FromStr for NodeIdentifier {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.find(":") {
            Some(p) => {
                let prefix = Identifier::from_str(&s[..p])?;
                let identifier = Identifier::from_str(&s[p + 1..])?;

                Ok(NodeIdentifier {
                    prefix: Some(prefix),
                    identifier,
                })
            }
            None => {
                let identifier = Identifier::from_str(&s)?;
                Ok(NodeIdentifier {
                    prefix: None,
                    identifier,
                })
            }
        }
    }
}

impl ToString for NodeIdentifier {
    fn to_string(&self) -> String {
        match &self.prefix {
            Some(prefix) => format!("{}:{}", prefix, self.identifier),
            None => format!("{}", self.identifier),
        }
    }
}

impl StmtArg for NodeIdentifier {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        NodeIdentifier::from_str(&str).map_err(|e| YangError::ArgumentParseError(e.str))
    }
}

///
/// "unknown-stmt" keyword.
///
#[derive(Clone, PartialEq, Getters)]
pub struct UnknownStmtKeyword {
    prefix: Prefix,
    identifier: Identifier,
}

impl fmt::Debug for UnknownStmtKeyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.prefix, self.identifier)
    }
}

impl ToString for UnknownStmtKeyword {
    fn to_string(&self) -> String {
        format!("{}:{}", self.prefix, self.identifier)
    }
}

impl FromStr for UnknownStmtKeyword {
    type Err = ArgError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        match str.find(":") {
            Some(p) => {
                let prefix = Identifier::from_str(&str[..p])?;
                let identifier = Identifier::from_str(&str[p + 1..])?;

                Ok(UnknownStmtKeyword { prefix, identifier })
            }
            None => Err(ArgError::new("unknown-stmt keyword")),
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
            Err(_) => Err(YangError::ArgumentParseError("url")),
        }
    }
}

///
/// "yang-version-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct YangVersionArg {
    str: String,
}

impl StmtArg for YangVersionArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        // According to RFC7950, the version should be "1.1", but we relax it.

        match parser.config().yang_version() {
            Some(yang_version) => {
                if str == yang_version {
                    Ok(YangVersionArg { str })
                } else {
                    Err(YangError::ArgumentParseError("yang-version"))
                }
            }
            None => Ok(YangVersionArg { str }),
        }
    }
}

///
/// "date-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct DateArg {
    str: String,
}

impl ToString for DateArg {
    fn to_string(&self) -> String {
        self.str.clone()
    }
}

impl FromStr for DateArg {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(DateArg { str: s.to_string() })
    }
}

impl StmtArg for DateArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        if str.chars().count() == 10 {
            if let Some(_) = &str[0..4].find(|c: char| !c.is_ascii_digit()) {
                Err(YangError::ArgumentParseError("date-arg"))
            } else if str.chars().nth(4).unwrap() != '-' {
                Err(YangError::ArgumentParseError("date-arg"))
            } else if let Some(_) = &str[5..7].find(|c: char| !c.is_ascii_digit()) {
                Err(YangError::ArgumentParseError("date-arg"))
            } else if str.chars().nth(7).unwrap() != '-' {
                Err(YangError::ArgumentParseError("date-arg"))
            } else if let Some(_) = &str[8..10].find(|c: char| !c.is_ascii_digit()) {
                Err(YangError::ArgumentParseError("date-arg"))
            } else {
                Ok(DateArg { str })
            }
        } else {
            Err(YangError::ArgumentParseError("date-arg"))
        }
    }
}

///
/// "yin-element-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct YinElementArg {
    arg: bool,
}

impl FromStr for YinElementArg {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "true" {
            Ok(YinElementArg { arg: true })
        } else if s == "false" {
            Ok(YinElementArg { arg: false })
        } else {
            Err(ArgError::new("yin-element-arg"))
        }
    }
}

impl StmtArg for YinElementArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        YinElementArg::from_str(&str).map_err(|_| YangError::ArgumentParseError("yin-element-arg"))
    }
}

///
/// "fraction-digits-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct FractionDigitsArg {
    digits: u8,
}

impl ToString for FractionDigitsArg {
    fn to_string(&self) -> String {
        format!("{}", self.digits())
    }
}

impl StmtArg for FractionDigitsArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        match str.parse::<u8>() {
            Ok(num) if num >= 1 && num <= 18 => Ok(FractionDigitsArg { digits: num }),
            _ => Err(YangError::ArgumentParseError("fraction-digits-arg")),
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
/// "status-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct StatusArg {
    arg: Status,
}

impl StmtArg for StatusArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str == "current" {
            Ok(StatusArg {
                arg: Status::Current,
            })
        } else if str == "obsolete" {
            Ok(StatusArg {
                arg: Status::Obsolete,
            })
        } else if str == "deprecated" {
            Ok(StatusArg {
                arg: Status::Deprecated,
            })
        } else {
            Err(YangError::ArgumentParseError("status-arg"))
        }
    }
}

///
/// "config-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
            Err(YangError::ArgumentParseError("config-arg"))
        }
    }
}

///
/// "mandatory-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
            Err(YangError::ArgumentParseError("mandatory-arg"))
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OrderedBy {
    User,
    System,
}

///
/// "order-by-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct OrderedByArg {
    arg: OrderedBy,
}

impl StmtArg for OrderedByArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str == "user" {
            Ok(OrderedByArg {
                arg: OrderedBy::User,
            })
        } else if str == "system" {
            Ok(OrderedByArg {
                arg: OrderedBy::System,
            })
        } else {
            Err(YangError::ArgumentParseError("ordered-by-arg"))
        }
    }
}

///
/// "min-value-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct MinValueArg {
    val: u64,
}

impl StmtArg for MinValueArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if is_non_negative_integer_value(&str) {
            match str.parse::<u64>() {
                Ok(num) => Ok(MinValueArg { val: num }),
                Err(_) => Err(YangError::ArgumentParseError("min-value-arg")),
            }
        } else {
            Err(YangError::ArgumentParseError("min-value-arg"))
        }
    }
}

#[derive(Clone, PartialEq)]
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
/// "max-value-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct MaxValueArg {
    val: MaxValue,
}

impl StmtArg for MaxValueArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        if str == "unbounded" {
            Ok(MaxValueArg {
                val: MaxValue::Unbounded,
            })
        } else if is_positive_integer_value(&str) {
            match str.parse::<u64>() {
                Ok(num) => Ok(MaxValueArg {
                    val: MaxValue::Value(num),
                }),
                Err(_) => Err(YangError::ArgumentParseError("max-value-arg")),
            }
        } else {
            Err(YangError::ArgumentParseError("max-value-arg"))
        }
    }
}

///
/// "integer-value".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct IntegerValue {
    val: i64,
}

impl StmtArg for IntegerValue {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if is_integer_value(&str) {
            match str.parse::<i64>() {
                Ok(num) => Ok(IntegerValue { val: num }),
                Err(_) => Err(YangError::ArgumentParseError("integer-value")),
            }
        } else {
            Err(YangError::ArgumentParseError("integer-value"))
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RangeBoundary {
    Min,
    Max,
    Integer(i64),
    Decimal(f64),
}

impl FromStr for RangeBoundary {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rb = s.trim();

        if rb == "min" {
            Ok(RangeBoundary::Min)
        } else if rb == "max" {
            Ok(RangeBoundary::Max)
        } else if is_decimal_value(rb) {
            match rb.parse::<f64>() {
                Ok(num) => Ok(RangeBoundary::Decimal(num)),
                Err(_) => Err(ArgError::new("range-arg")),
            }
        } else if is_integer_value(rb) {
            match rb.parse::<i64>() {
                Ok(num) => Ok(RangeBoundary::Integer(num)),
                Err(_) => Err(ArgError::new("range-arg")),
            }
        } else {
            Err(ArgError::new("range-arg"))
        }
    }
}

pub type RangePart = (RangeBoundary, Option<RangeBoundary>);

///
/// "range-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct RangeArg {
    parts: Vec<RangePart>,
}

impl StmtArg for RangeArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        let parts: Vec<_> = str.split('|').collect();
        let mut v = Vec::new();

        for p in parts {
            let lower;
            let upper;

            let bounds: Vec<_> = p.split("..").collect();
            if bounds.len() == 1 {
                if bounds[0] == "" {
                    return Err(YangError::ArgumentParseError("range-arg"));
                }

                lower = RangeBoundary::from_str(bounds[0])
                    .map_err(|e| YangError::ArgumentParseError(e.str))?;
                upper = None;
            } else if bounds.len() == 2 {
                if bounds[0] == "" || bounds[1] == "" {
                    return Err(YangError::ArgumentParseError("range-arg"));
                }
                lower = RangeBoundary::from_str(bounds[0])
                    .map_err(|e| YangError::ArgumentParseError(e.str))?;
                upper = Some(
                    RangeBoundary::from_str(bounds[1])
                        .map_err(|e| YangError::ArgumentParseError(e.str))?,
                );
            } else {
                return Err(YangError::ArgumentParseError("range-arg"));
            }

            v.push((lower, upper));
        }

        Ok(RangeArg { parts: v })
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LengthBoundary {
    Min,
    Max,
    Integer(u64),
}

impl FromStr for LengthBoundary {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lb = s.trim();

        if lb == "min" {
            Ok(LengthBoundary::Min)
        } else if lb == "max" {
            Ok(LengthBoundary::Max)
        } else if is_integer_value(lb) {
            match lb.parse::<u64>() {
                Ok(num) => Ok(LengthBoundary::Integer(num)),
                Err(_) => Err(ArgError::new("length-arg")),
            }
        } else {
            Err(ArgError::new("length-arg"))
        }
    }
}

pub type LengthPart = (LengthBoundary, Option<LengthBoundary>);

///
/// "length-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct LengthArg {
    parts: Vec<LengthPart>,
}

impl StmtArg for LengthArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        let parts: Vec<_> = str.split('|').collect();
        let mut v = Vec::new();

        for p in parts {
            let lower;
            let upper;

            let bounds: Vec<_> = p.split("..").collect();
            if bounds.len() == 1 {
                if bounds[0] == "" {
                    return Err(YangError::ArgumentParseError("length-arg"));
                }

                lower = LengthBoundary::from_str(bounds[0])
                    .map_err(|e| YangError::ArgumentParseError(e.str))?;
                upper = None;
            } else if bounds.len() == 2 {
                if bounds[0] == "" || bounds[1] == "" {
                    return Err(YangError::ArgumentParseError("length-arg"));
                }
                lower = LengthBoundary::from_str(bounds[0])
                    .map_err(|e| YangError::ArgumentParseError(e.str))?;
                upper = Some(
                    LengthBoundary::from_str(bounds[1])
                        .map_err(|e| YangError::ArgumentParseError(e.str))?,
                );
            } else {
                return Err(YangError::ArgumentParseError("length-arg"));
            }

            v.push((lower, upper));
        }

        Ok(LengthArg { parts: v })
    }
}

///
/// "modifier-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ModifierArg {}

impl StmtArg for ModifierArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str == "invert-match" {
            Ok(ModifierArg {})
        } else {
            Err(YangError::ArgumentParseError("modifier-arg"))
        }
    }
}

///
/// "position-value-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct PositionValueArg {
    val: u64,
}

impl StmtArg for PositionValueArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if is_non_negative_integer_value(&str) {
            match str.parse::<u64>() {
                Ok(num) => Ok(PositionValueArg { val: num }),
                Err(_) => Err(YangError::ArgumentParseError("position-value-arg")),
            }
        } else {
            Err(YangError::ArgumentParseError("position-value-arg"))
        }
    }
}

///
/// "path-arg".
///
#[derive(Debug, Clone, PartialEq)]
pub enum PathArg {
    AbsolutePath(AbsolutePath),
    RelativePath(RelativePath),
    #[cfg(feature = "cisco-nso-extensions")]
    DerefPath(DerefPath),
}

impl StmtArg for PathArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        Self::from_str(&str).map_err(|e| YangError::ArgumentParseError(e.str))
    }
}

impl FromStr for PathArg {
    type Err = ArgError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('/') {
            Ok(PathArg::AbsolutePath(AbsolutePath::from_str(&s)?))
        } else if s.starts_with("..") {
            Ok(PathArg::RelativePath(RelativePath::from_str(&s)?))
        } else if s.starts_with("deref") {
            #[cfg(feature = "cisco-nso-extensions")]
            return Ok(PathArg::DerefPath(DerefPath::from_str(s)?));
            #[cfg(not(feature = "cisco-nso-extensions"))]
            return Err(ArgError {
                str: "path-arg with deref is not RFC7950 compatible, see cisco-nso-extensions feature",
            });
        } else {
            Err(ArgError {
                str: "path-arg start",
            })
        }
    }
}

/// "absolute-path".
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct AbsolutePath {
    nodes: Vec<PathNode>,
}

impl FromStr for AbsolutePath {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = &s[..];
        let mut nodes = Vec::new();

        while s.len() > 0 {
            if !s.starts_with('/') {
                return Err(ArgError::new("absolute-path"));
            }
            s = &s[1..];

            let node_identifier;
            let mut path_predicate = Vec::new();

            match s.find(|c: char| c == '[' || c == '/') {
                Some(pos) => {
                    node_identifier = NodeIdentifier::from_str(&s[..pos])?;
                    s = &s[pos..];

                    if s.starts_with('[') {
                        // do while.
                        while {
                            let pos = match s.find(']') {
                                Some(p) => Ok(p + 1),
                                None => Err(ArgError::new("absolute-path")),
                            }?;

                            path_predicate.push(PathPredicate::from_str(&s[..pos])?);
                            s = &s[pos..];

                            s.len() > 0 && s.starts_with('[')
                        } {}
                    }
                }
                None => {
                    node_identifier = NodeIdentifier::from_str(&s)?;
                    nodes.push(PathNode {
                        node_identifier,
                        path_predicate,
                    });
                    break;
                }
            }

            nodes.push(PathNode {
                node_identifier,
                path_predicate,
            });
        }

        Ok(AbsolutePath { nodes })
    }
}

#[derive(Debug, Clone, PartialEq, Getters)]
pub struct PathNode {
    node_identifier: NodeIdentifier,
    path_predicate: Vec<PathPredicate>,
}

/// "relative-path".
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct RelativePath {
    up: u32,
    descendant_path: DescendantPath,
}

impl FromStr for RelativePath {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = &s[..];
        let mut up = 0;

        if !s.starts_with("../") {
            return Err(ArgError::new("relative-path"));
        }

        while {
            up += 1;
            s = &s[3..];
            s.len() > 0 && s.starts_with("../")
        } {}

        let descendant_path = DescendantPath::from_str(s)?;
        Ok(RelativePath {
            up,
            descendant_path,
        })
    }
}

///  "deref-path"
#[cfg(feature = "cisco-nso-extensions")]
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct DerefPath {
    deref_path: Box<PathArg>, // Needs to be boxed to prevent recursion
    relative_path: RelativePath,
}

#[cfg(feature = "cisco-nso-extensions")]
impl FromStr for DerefPath {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with("deref(") {
            return Err(ArgError::new("deref-path prefix"));
        }
        let mut paren_count = 0;
        let mut deref_arg_end = 0;
        for (i, c) in s.chars().enumerate().skip("deref".len()) {
            match c {
                '(' => paren_count += 1,
                ')' => {
                    paren_count -= 1;
                    if paren_count == 0 {
                        deref_arg_end = i;
                        break;
                    }
                }
                _ => {}
            };
        }

        if deref_arg_end == 0 {
            println!("Paren mismatch in '{}': {}", s, paren_count);
            return Err(ArgError::new("deref-path parens"));
        }

        let path_remainder_start = deref_arg_end + 2; // skip )/
        if &s[deref_arg_end..path_remainder_start] != ")/" {
            return Err(ArgError::new("deref-path follow"));
        }

        let deref_path = PathArg::from_str(&s[6..deref_arg_end])?;
        let relative_path = RelativePath::from_str(&s[path_remainder_start..])?;

        Ok(DerefPath {
            deref_path: Box::new(deref_path),
            relative_path,
        })
    }
}

/// "descendant-path".
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct DescendantPath {
    node_identifier: NodeIdentifier,
    path_predicate: Vec<PathPredicate>,
    absolute_path: Option<AbsolutePath>,
}

impl FromStr for DescendantPath {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = &s[..];
        let node_identifier;
        let mut path_predicate = Vec::new();
        let mut absolute_path = None;

        if s.len() == 0 {
            return Err(ArgError::new("descendant-path"));
        }

        match s.find(|c: char| c == '[' || c == '/') {
            Some(pos) => {
                node_identifier = NodeIdentifier::from_str(&s[..pos])?;
                s = &s[pos..];

                if s.starts_with('[') {
                    // do while.
                    while {
                        let pos = match s.find(']') {
                            Some(p) => Ok(p + 1),
                            None => Err(ArgError::new("descendant-path")),
                        }?;

                        path_predicate.push(PathPredicate::from_str(&s[..pos])?);
                        s = &s[pos..];

                        s.len() > 0 && s.starts_with('[')
                    } {}
                }

                if s.len() > 0 {
                    absolute_path = Some(AbsolutePath::from_str(s)?);
                }
            }
            None => {
                node_identifier = NodeIdentifier::from_str(s)?;
            }
        }

        Ok(DescendantPath {
            node_identifier,
            path_predicate,
            absolute_path,
        })
    }
}

/// "path-predicate".
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct PathPredicate {
    path_equality_expr: PathEqualityExpr,
}

impl FromStr for PathPredicate {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with('[') || !s.ends_with(']') {
            Err(ArgError::new("path-predicate"))
        } else {
            Ok(PathPredicate {
                path_equality_expr: PathEqualityExpr::from_str(&s[1..s.len() - 1])?,
            })
        }
    }
}

/// "path-equality-expr".
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct PathEqualityExpr {
    node_identifier: NodeIdentifier,
    path_key_expr: PathKeyExpr,
}

impl FromStr for PathEqualityExpr {
    type Err = ArgError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.find('=') {
            Some(p) => Ok(PathEqualityExpr {
                node_identifier: NodeIdentifier::from_str(&s[0..p].trim())?,
                path_key_expr: PathKeyExpr::from_str(&s[p + 1..].trim())?,
            }),
            None => Err(ArgError::new("path-equality-expr")),
        }
    }
}

/// "path-key-expr".
///
/// path-key-expr       = current-function-invocation *WSP "/" *WSP
///                       rel-path-keyexpr
///
/// rel-path-keyexpr    = 1*(".." *WSP "/" *WSP)
///                       *(node-identifier *WSP "/" *WSP)
///                       node-identifier
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct PathKeyExpr {
    rel_path_keyexpr: String,
}

impl FromStr for PathKeyExpr {
    type Err = ArgError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        // TBD: Validation only.
        let paths: Vec<_> = str.split("/").map(|s| s.trim()).collect();
        // Minimum of "current() / .. / node-identifier".
        if paths.len() < 3 {
            return Err(ArgError::new("path-key-expr"));
        // Invalid current function invocation.
        } else if !is_current_function_invocation(&paths[0]) {
            return Err(ArgError::new("path-key-expr"));
        // Validate rel-path-keyexpr.
        } else {
            let mut i = 1;

            if paths[i] != ".." {
                Err(ArgError::new("path-key-expr"))
            } else {
                i += 1;
                while i < paths.len() && paths[i] == ".." {
                    i += 1;
                }
                if i >= paths.len() {
                    return Err(ArgError::new("path-key-expr"));
                }

                if !is_node_identifier(paths[i]) {
                    return Err(ArgError::new("path-key-expr"));
                }

                while i < paths.len() {
                    if !is_node_identifier(paths[i]) {
                        return Err(ArgError::new("path-key-expr"));
                    }
                    i += 1;
                }

                Ok(PathKeyExpr {
                    rel_path_keyexpr: str.to_string(),
                })
            }
        }
    }
}

///
/// Tokenizer for "if-feature".
///
pub enum IfFeatureToken {
    Init,
    ParenBegin,
    ParenEnd,
    Not,
    And,
    Or,
    IdentifierRef(String),
    EndOfLine,
}

pub struct Tokenizer {
    str: String,
    pos: usize,
}

impl Tokenizer {
    pub fn new(s: String) -> Tokenizer {
        Tokenizer { str: s, pos: 0 }
    }

    pub fn line(&mut self) -> &str {
        &self.str[self.pos..]
    }

    /// Helper to check if the current line starts with a keyword
    /// while checking the keyword is followed by a word boundary (whitespace, line end)
    /// to distinguish between keywords ('or')
    /// and identifiers starting the same ('origin')
    fn line_starts_with_keyword(&mut self, key: &str) -> bool {
        let line = self.line();
        line.starts_with(key)
            && (line.len() == key.len()
                || line.find(|c: char| c.is_whitespace()) == Some(key.len()))
    }

    pub fn get_token(&mut self) -> IfFeatureToken {
        if let Some(p) = self.line().find(|c: char| !c.is_whitespace()) {
            self.pos += p;
        }

        if self.line().len() == 0 {
            IfFeatureToken::EndOfLine
        } else if self.line().starts_with('(') {
            self.pos += 1;
            IfFeatureToken::ParenBegin
        } else if self.line().starts_with(')') {
            self.pos += 1;
            IfFeatureToken::ParenEnd
        } else if self.line_starts_with_keyword("not") {
            self.pos += 3;
            IfFeatureToken::Not
        } else if self.line_starts_with_keyword("and") {
            self.pos += 3;
            IfFeatureToken::And
        } else if self.line_starts_with_keyword("or") {
            self.pos += 2;
            IfFeatureToken::Or
        } else {
            let p = match self.line().find(|c: char| {
                !c.is_alphanumeric() && c != '-' && c != '_' && c != '.' && c != ':'
            }) {
                Some(p) => p,
                None => self.str.len() - self.pos,
            };
            let token = &self.str[self.pos..self.pos + p];

            self.pos += p;
            IfFeatureToken::IdentifierRef(token.to_string())
        }
    }
}

/// "if-feature-expr".
#[derive(Clone, PartialEq, Getters)]
pub struct IfFeatureExpr {
    terms: Vec<IfFeatureTerm>,
}

impl fmt::Debug for IfFeatureExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl ToString for IfFeatureExpr {
    fn to_string(&self) -> String {
        let vec: Vec<_> = self.terms.iter().map(|t| t.to_string()).collect();
        format!("{}", vec.join(" or "))
    }
}

impl IfFeatureExpr {
    /// Recursively parse "if-feature" arg string.
    pub fn parse(tokenizer: &mut Tokenizer) -> Result<Self, ArgError> {
        let mut terms: Vec<IfFeatureTerm> = Vec::new();
        let mut factors: Vec<IfFeatureFactor> = Vec::new();
        let mut not: Option<bool> = None;
        let mut prev = IfFeatureToken::Init;

        loop {
            let mut token = tokenizer.get_token();
            match token {
                IfFeatureToken::Init => {}
                IfFeatureToken::ParenBegin => {
                    match prev {
                        IfFeatureToken::ParenBegin
                        | IfFeatureToken::ParenEnd
                        | IfFeatureToken::IdentifierRef(_) => {
                            return Err(ArgError::new("if-feature-expr: invalid begin paren"))
                        }
                        _ => {}
                    }

                    let expr = Box::new(IfFeatureExpr::parse(tokenizer)?);
                    factors.push(IfFeatureFactor::IfFeatureExpr((not.take(), expr)));

                    token = IfFeatureToken::ParenEnd;
                }
                IfFeatureToken::ParenEnd => {
                    match prev {
                        IfFeatureToken::ParenBegin
                        | IfFeatureToken::Not
                        | IfFeatureToken::And
                        | IfFeatureToken::Or => {
                            return Err(ArgError::new("if-feature-expr: invalid end paren"))
                        }
                        _ => {}
                    }

                    break;
                }
                IfFeatureToken::Or => {
                    match prev {
                        IfFeatureToken::Init
                        | IfFeatureToken::ParenBegin
                        | IfFeatureToken::Not
                        | IfFeatureToken::And
                        | IfFeatureToken::Or => {
                            return Err(ArgError::new("if-feature-expr: invalid 'or'"))
                        }
                        _ => {}
                    }

                    terms.push(IfFeatureTerm {
                        factors: factors.drain(..).collect(),
                    });
                    factors = Vec::new();
                }
                IfFeatureToken::And => match prev {
                    IfFeatureToken::Init
                    | IfFeatureToken::ParenBegin
                    | IfFeatureToken::Not
                    | IfFeatureToken::And
                    | IfFeatureToken::Or => {
                        return Err(ArgError::new("if-feature-expr: invalid 'and'"))
                    }
                    _ => {}
                },
                IfFeatureToken::Not => {
                    match prev {
                        IfFeatureToken::ParenEnd
                        | IfFeatureToken::Not
                        | IfFeatureToken::IdentifierRef(_) => {
                            return Err(ArgError::new("if-feature-expr: invalid 'not'"))
                        }
                        _ => {}
                    }

                    not.replace(true);
                }
                IfFeatureToken::IdentifierRef(ref str) => {
                    match prev {
                        IfFeatureToken::ParenEnd | IfFeatureToken::IdentifierRef(_) => {
                            return Err(ArgError::new("if-feature-expr: invalid identifier-ref"))
                        }
                        _ => {}
                    }

                    let identifier_ref = IdentifierRef::from_str(&str)?;
                    factors.push(IfFeatureFactor::IdentifierRef((not.take(), identifier_ref)));
                }
                IfFeatureToken::EndOfLine => break,
            }

            prev = token;
        }

        terms.push(IfFeatureTerm {
            factors: factors.drain(..).collect(),
        });
        Ok(IfFeatureExpr { terms })
    }
}

impl StmtArg for IfFeatureExpr {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        let mut tokenizer = Tokenizer::new(str);

        IfFeatureExpr::parse(&mut tokenizer).map_err(|e| YangError::ArgumentParseError(e.str))
    }
}

/// "if-feature-term".
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct IfFeatureTerm {
    factors: Vec<IfFeatureFactor>,
}

impl ToString for IfFeatureTerm {
    fn to_string(&self) -> String {
        let vec: Vec<_> = self.factors.iter().map(|f| f.to_string()).collect();
        format!("{}", vec.join(" and "))
    }
}

/// "if-feature-factor".
#[derive(Debug, Clone, PartialEq)]
pub enum IfFeatureFactor {
    IfFeatureExpr((Option<bool>, Box<IfFeatureExpr>)),
    IdentifierRef((Option<bool>, IdentifierRef)),
}

impl ToString for IfFeatureFactor {
    fn to_string(&self) -> String {
        match self {
            IfFeatureFactor::IfFeatureExpr((not, expr)) => {
                if let Some(_) = not {
                    format!("not ({:?})", expr)
                } else {
                    format!("({:?})", expr)
                }
            }
            IfFeatureFactor::IdentifierRef((not, identifier_ref)) => {
                if let Some(_) = not {
                    format!("not {:?}", identifier_ref)
                } else {
                    format!("{:?}", identifier_ref)
                }
            }
        }
    }
}

///
/// "require-instance-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct RequireInstanceArg {
    arg: bool,
}

impl StmtArg for RequireInstanceArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str == "true" {
            Ok(RequireInstanceArg { arg: true })
        } else if str == "false" {
            Ok(RequireInstanceArg { arg: false })
        } else {
            Err(YangError::ArgumentParseError("require-instance-arg"))
        }
    }
}

///
/// "key-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct KeyArg {
    keys: Vec<NodeIdentifier>,
}

impl StmtArg for KeyArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        let mut keys = Vec::new();
        let mut s = &str[..];

        while {
            let pos = match s.find(char::is_whitespace) {
                Some(p) => p,
                None => s.len(),
            };

            let node_identifier = NodeIdentifier::from_str(&s[..pos])
                .map_err(|e| YangError::ArgumentParseError(e.str))?;
            keys.push(node_identifier);

            s = &s[pos..].trim();
            s.len() > 0
        } {}

        Ok(KeyArg { keys })
    }
}

///
/// "schema-nodeid".
///
#[derive(Debug, Clone, PartialEq)]
pub enum SchemaNodeid {
    Absolute(AbsoluteSchemaNodeid),
    Descendant(DescendantSchemaNodeid),
}

impl StmtArg for SchemaNodeid {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str.starts_with('/') {
            Ok(SchemaNodeid::Absolute(
                AbsoluteSchemaNodeid::from_str(&str)
                    .map_err(|e| YangError::ArgumentParseError(e.str))?,
            ))
        } else {
            Ok(SchemaNodeid::Descendant(
                DescendantSchemaNodeid::from_str(&str)
                    .map_err(|e| YangError::ArgumentParseError(e.str))?,
            ))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Getters)]
pub struct AbsoluteSchemaNodeid {
    nodes: Vec<NodeIdentifier>,
}

#[derive(Debug, Clone, PartialEq, Getters)]
pub struct DescendantSchemaNodeid {
    nodes: Vec<NodeIdentifier>,
}

impl FromStr for AbsoluteSchemaNodeid {
    type Err = ArgError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        if let Some(_) = str.find(char::is_whitespace) {
            Err(ArgError::new("absolute-schema-nodeid"))
        } else if let Some(_) = str.find("//") {
            Err(ArgError::new("absolute-schema-nodeid"))
        } else if str.starts_with('/') {
            let mut nodes: Vec<NodeIdentifier> = Vec::new();
            for n in (&str[1..]).split('/') {
                nodes.push(NodeIdentifier::from_str(n)?);
            }
            Ok(AbsoluteSchemaNodeid { nodes })
        } else {
            Err(ArgError::new("absolute-schema-nodeid"))
        }
    }
}

impl FromStr for DescendantSchemaNodeid {
    type Err = ArgError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        if let Some(_) = str.find(char::is_whitespace) {
            Err(ArgError::new("descendant-schema-nodeid"))
        } else if let Some(_) = str.find("//") {
            Err(ArgError::new("descendant-schema-nodeid"))
        } else if !str.starts_with('/') {
            let mut nodes: Vec<NodeIdentifier> = Vec::new();
            for n in str.split('/') {
                nodes.push(NodeIdentifier::from_str(n)?);
            }
            Ok(DescendantSchemaNodeid { nodes })
        } else {
            Err(ArgError::new("descendant-schema-nodeid"))
        }
    }
}

impl StmtArg for AbsoluteSchemaNodeid {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        AbsoluteSchemaNodeid::from_str(&str).map_err(|e| YangError::ArgumentParseError(e.str))
    }
}

impl StmtArg for DescendantSchemaNodeid {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        DescendantSchemaNodeid::from_str(&str).map_err(|e| YangError::ArgumentParseError(e.str))
    }
}

impl ToString for AbsoluteSchemaNodeid {
    fn to_string(&self) -> String {
        format!(
            "/{}",
            self.nodes
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<String>>()
                .join("/")
        )
    }
}

impl ToString for DescendantSchemaNodeid {
    fn to_string(&self) -> String {
        self.nodes
            .iter()
            .map(|n| n.to_string())
            .collect::<Vec<String>>()
            .join("/")
    }
}

///
/// "unique-arg".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct UniqueArg {
    nodeids: Vec<DescendantSchemaNodeid>,
}

impl StmtArg for UniqueArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        UniqueArg::from_str(&str).map_err(|e| YangError::ArgumentParseError(e.str))
    }
}

impl FromStr for UniqueArg {
    type Err = ArgError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        let mut nodeids = Vec::new();

        for n in str.split_whitespace() {
            nodeids.push(DescendantSchemaNodeid::from_str(n)?);
        }

        Ok(UniqueArg { nodeids })
    }
}

/// "refine-arg".
pub type RefineArg = DescendantSchemaNodeid;

/// "uses-augment-arg".
pub type UsesAugmentArg = DescendantSchemaNodeid;

/// "augment-arg".
pub type AugmentArg = AbsoluteSchemaNodeid;

/// "deviation-arg".
pub type DeviationArg = AbsoluteSchemaNodeid;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_identifier() {
        let s = " hello-world ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(
                arg,
                Identifier {
                    str: String::from("hello-world")
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = " _123.IdEnT.456-789_ ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(
                arg,
                Identifier {
                    str: String::from("_123.IdEnT.456-789_")
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = " 123 ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error identifier"),
        }

        let s = " 123$ ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error identifier"),
        }
    }

    #[test]
    pub fn test_identifier_ref() {
        let s = " prefix:hello-world ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.to_string(), "prefix:hello-world"),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = " _prefix_:_123.IdEnT.456-789_ ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.to_string(), "_prefix_:_123.IdEnT.456-789_"),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = " 123:456 ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error identifier"),
        }

        let s = " _123:_456 ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.to_string(), "_123:_456"),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = " _123: ";
        let mut parser = Parser::new(s.to_string());

        match IdentifierRef::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error identifier"),
        }
    }

    #[test]
    pub fn test_date_arg() {
        let s = " 2021-08-01 ";
        let mut parser = Parser::new(s.to_string());

        match DateArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.to_string(), "2021-08-01"),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = " 2021-8-1 ";
        let mut parser = Parser::new(s.to_string());

        match DateArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error date-arg"),
        }

        let s = " 08-01-2021 ";
        let mut parser = Parser::new(s.to_string());

        match DateArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error date-arg"),
        }

        let s = " 2021-08-0x ";
        let mut parser = Parser::new(s.to_string());

        match DateArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error date-arg"),
        }
    }

    #[test]
    pub fn test_fraction_digits_arg() {
        let s = "18";
        let mut parser = Parser::new(s.to_string());

        match FractionDigitsArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.digits(), &18),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = "0";
        let mut parser = Parser::new(s.to_string());

        match FractionDigitsArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error fraction-digits-arg"),
        }

        let s = "19";
        let mut parser = Parser::new(s.to_string());

        match FractionDigitsArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error fraction-digits-arg"),
        }
    }

    #[test]
    pub fn test_range_arg() {
        let s = r#""1..10""#;
        let mut parser = Parser::new(s.to_string());

        match RangeArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(
                arg,
                RangeArg {
                    parts: vec![(RangeBoundary::Integer(1), Some(RangeBoundary::Integer(10)))]
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""1 .. 10 | 21..30""#;
        let mut parser = Parser::new(s.to_string());

        match RangeArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(
                arg,
                RangeArg {
                    parts: vec![
                        (RangeBoundary::Integer(1), Some(RangeBoundary::Integer(10))),
                        (RangeBoundary::Integer(21), Some(RangeBoundary::Integer(30))),
                    ]
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""min..max""#;
        let mut parser = Parser::new(s.to_string());

        match RangeArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(
                arg,
                RangeArg {
                    parts: vec![(RangeBoundary::Min, Some(RangeBoundary::Max)),]
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""min..""#;
        let mut parser = Parser::new(s.to_string());

        match RangeArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error range-arg"),
        }

        let s = r#""1.01 .. 1.99""#;
        let mut parser = Parser::new(s.to_string());

        match RangeArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(
                arg,
                RangeArg {
                    parts: vec![(
                        RangeBoundary::Decimal(1.01),
                        Some(RangeBoundary::Decimal(1.99))
                    )]
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }
    }

    #[test]
    pub fn test_length_arg() {
        let s = r#""1..10""#;
        let mut parser = Parser::new(s.to_string());

        match LengthArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(
                arg,
                LengthArg {
                    parts: vec![(
                        LengthBoundary::Integer(1),
                        Some(LengthBoundary::Integer(10))
                    )]
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""1 .. 10 | 21..30""#;
        let mut parser = Parser::new(s.to_string());

        match LengthArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(
                arg,
                LengthArg {
                    parts: vec![
                        (
                            LengthBoundary::Integer(1),
                            Some(LengthBoundary::Integer(10))
                        ),
                        (
                            LengthBoundary::Integer(21),
                            Some(LengthBoundary::Integer(30))
                        ),
                    ]
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""min..max""#;
        let mut parser = Parser::new(s.to_string());

        match LengthArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(
                arg,
                LengthArg {
                    parts: vec![(LengthBoundary::Min, Some(LengthBoundary::Max)),]
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""min..""#;
        let mut parser = Parser::new(s.to_string());

        match LengthArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error length-arg"),
        }
    }

    #[test]
    pub fn test_path_key_expr() {
        let s = "current()/../node";
        match PathKeyExpr::from_str(s) {
            Ok(arg) => assert_eq!(
                arg,
                PathKeyExpr {
                    rel_path_keyexpr: "current()/../node".to_string()
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = "current()/../../../node";
        match PathKeyExpr::from_str(s) {
            Ok(arg) => assert_eq!(
                arg,
                PathKeyExpr {
                    rel_path_keyexpr: "current()/../../../node".to_string()
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = "current()/../../../node/node/node";
        match PathKeyExpr::from_str(s) {
            Ok(arg) => assert_eq!(
                arg,
                PathKeyExpr {
                    rel_path_keyexpr: "current()/../../../node/node/node".to_string()
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = "current ( ) / .. / .. / .. / node / node / node ";
        match PathKeyExpr::from_str(s) {
            Ok(arg) => assert_eq!(
                arg,
                PathKeyExpr {
                    rel_path_keyexpr: "current ( ) / .. / .. / .. / node / node / node "
                        .to_string()
                }
            ),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = "current()/..";
        match PathKeyExpr::from_str(s) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Arg error: path-key-expr"),
        }

        let s = "current()/node/node/node";
        match PathKeyExpr::from_str(s) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Arg error: path-key-expr"),
        }
    }

    #[test]
    pub fn test_absolute_path() {
        let s = "/node1/node2[id=current()/../rel1/rel2]";
        let path = AbsolutePath {
            nodes: vec![
                PathNode {
                    node_identifier: NodeIdentifier::from_str("node1").unwrap(),
                    path_predicate: vec![],
                },
                PathNode {
                    node_identifier: NodeIdentifier::from_str("node2").unwrap(),
                    path_predicate: vec![PathPredicate {
                        path_equality_expr: PathEqualityExpr {
                            node_identifier: NodeIdentifier::from_str("id").unwrap(),
                            path_key_expr: PathKeyExpr {
                                rel_path_keyexpr: "current()/../rel1/rel2".to_string(),
                            },
                        },
                    }],
                },
            ],
        };

        match AbsolutePath::from_str(s) {
            Ok(p) => assert_eq!(p, path),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s =
            "/node1/node2[id1=current()/../rel1/rel2][prefix:id2=current()/../../rel3/rel4]/node3";
        let path = AbsolutePath {
            nodes: vec![
                PathNode {
                    node_identifier: NodeIdentifier::from_str("node1").unwrap(),
                    path_predicate: vec![],
                },
                PathNode {
                    node_identifier: NodeIdentifier::from_str("node2").unwrap(),
                    path_predicate: vec![
                        PathPredicate {
                            path_equality_expr: PathEqualityExpr {
                                node_identifier: NodeIdentifier::from_str("id1").unwrap(),
                                path_key_expr: PathKeyExpr {
                                    rel_path_keyexpr: "current()/../rel1/rel2".to_string(),
                                },
                            },
                        },
                        PathPredicate {
                            path_equality_expr: PathEqualityExpr {
                                node_identifier: NodeIdentifier::from_str("prefix:id2").unwrap(),
                                path_key_expr: PathKeyExpr {
                                    rel_path_keyexpr: "current()/../../rel3/rel4".to_string(),
                                },
                            },
                        },
                    ],
                },
                PathNode {
                    node_identifier: NodeIdentifier::from_str("node3").unwrap(),
                    path_predicate: vec![],
                },
            ],
        };

        match AbsolutePath::from_str(s) {
            Ok(p) => assert_eq!(p, path),
            Err(err) => panic!("{:?}", err.to_string()),
        }
    }

    #[test]
    pub fn test_descendant_path() {
        let s = "node0/node1/node2[id=current()/../rel1/rel2]";

        let absolute_path = AbsolutePath {
            nodes: vec![
                PathNode {
                    node_identifier: NodeIdentifier::from_str("node1").unwrap(),
                    path_predicate: vec![],
                },
                PathNode {
                    node_identifier: NodeIdentifier::from_str("node2").unwrap(),
                    path_predicate: vec![PathPredicate {
                        path_equality_expr: PathEqualityExpr {
                            node_identifier: NodeIdentifier::from_str("id").unwrap(),
                            path_key_expr: PathKeyExpr {
                                rel_path_keyexpr: "current()/../rel1/rel2".to_string(),
                            },
                        },
                    }],
                },
            ],
        };
        let descendant_path = DescendantPath {
            node_identifier: NodeIdentifier::from_str("node0").unwrap(),
            path_predicate: vec![],
            absolute_path: Some(absolute_path),
        };

        match DescendantPath::from_str(s) {
            Ok(p) => assert_eq!(p, descendant_path),
            Err(err) => panic!("{:?}", err.to_string()),
        }
    }

    #[test]
    pub fn test_relative_path() {
        let s = "../../node0/node1/node2";

        let absolute_path = AbsolutePath {
            nodes: vec![
                PathNode {
                    node_identifier: NodeIdentifier::from_str("node1").unwrap(),
                    path_predicate: vec![],
                },
                PathNode {
                    node_identifier: NodeIdentifier::from_str("node2").unwrap(),
                    path_predicate: vec![],
                },
            ],
        };

        let descendant_path = DescendantPath {
            node_identifier: NodeIdentifier::from_str("node0").unwrap(),
            path_predicate: vec![],
            absolute_path: Some(absolute_path),
        };

        let relative_path = RelativePath {
            up: 2u32,
            descendant_path,
        };

        match RelativePath::from_str(s) {
            Ok(p) => assert_eq!(p, relative_path),
            Err(err) => panic!("{:?}", err.to_string()),
        }
    }

    #[test]
    pub fn test_if_feature_expr() {
        let s = r#""p1:id1 and p1:id2 or (p2:id3 and p2:id4) or not p3:id5""#;
        let mut parser = Parser::new(s.to_string());

        match IfFeatureExpr::parse_arg(&mut parser) {
            Ok(expr) => assert_eq!(
                format!("{:?}", expr),
                "p1:id1 and p1:id2 or (p2:id3 and p2:id4) or not p3:id5"
            ),
            Err(_) => panic!(),
        }

        let s = r#""p1:id1 and p1:id2 origin (p2:id3 and p2:id4)""#;
        let mut parser = Parser::new(s.to_string());

        match IfFeatureExpr::parse_arg(&mut parser) {
            Ok(expr) => panic!("{:?}", expr),
            Err(err) => assert_eq!(
                err.to_string(),
                "Argument parse error if-feature-expr: invalid identifier-ref"
            ),
        }

        let s = r#""p1:id1 p1:id2""#;
        let mut parser = Parser::new(s.to_string());

        match IfFeatureExpr::parse_arg(&mut parser) {
            Ok(expr) => panic!("{:?}", expr),
            Err(err) => assert_eq!(
                err.to_string(),
                "Argument parse error if-feature-expr: invalid identifier-ref"
            ),
        }
    }

    #[test]
    pub fn test_key_arg() {
        let s = r#""p1:id1 p1:id2 p2:id3 id4 id5""#;
        let mut parser = Parser::new(s.to_string());

        match KeyArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(
                arg,
                KeyArg {
                    keys: vec![
                        NodeIdentifier::from_str("p1:id1").unwrap(),
                        NodeIdentifier::from_str("p1:id2").unwrap(),
                        NodeIdentifier::from_str("p2:id3").unwrap(),
                        NodeIdentifier::from_str("id4").unwrap(),
                        NodeIdentifier::from_str("id5").unwrap(),
                    ]
                }
            ),
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    #[test]
    pub fn test_absolute_schema_nodeid() {
        let s = r#""/id1/id2/id3""#;
        let mut parser = Parser::new(s.to_string());

        match AbsoluteSchemaNodeid::parse_arg(&mut parser) {
            Ok(arg) => {
                assert_eq!(arg.to_string(), "/id1/id2/id3");
                assert_eq!(arg.nodes.len(), 3);
            }
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    #[test]
    pub fn test_descendant_schema_nodeid() {
        let s = r#""id1/id2/id3""#;
        let mut parser = Parser::new(s.to_string());

        match DescendantSchemaNodeid::parse_arg(&mut parser) {
            Ok(arg) => {
                assert_eq!(arg.to_string(), "id1/id2/id3");
                assert_eq!(arg.nodes.len(), 3);
            }
            Err(err) => panic!("{}", err.to_string()),
        }
    }
}
