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
        // End of Input.
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
        if is_identifier(s) {
            Ok(Identifier { str: s.to_string() })
        } else {
            Err(YangError::ArgumentParseError("identifier"))
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

impl FromStr for IdentifierRef {
    type Err = YangError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
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

impl StmtArg for IdentifierRef {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        IdentifierRef::from_str(&str)
    }
}

///
/// "node-identifier".
///
#[derive(Clone, PartialEq)]
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
    type Err = YangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.find(":") {
            Some(p) => {
                let prefix = Identifier::from_str(&s[..p])?;
                let identifier = Identifier::from_str(&s[p + 1..])?;

                Ok(NodeIdentifier { prefix: Some(prefix), identifier})
            }
            None => {
                let identifier = Identifier::from_str(&s)?;
                Ok(NodeIdentifier { prefix: None, identifier })
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
        NodeIdentifier::from_str(&str)
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
            Err(_) => Err(YangError::ArgumentParseError("url"))
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
            Err(YangError::ArgumentParseError("yang-version"))
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
            Err(YangError::ArgumentParseError("yin-element-arg"))
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
            _ => Err(YangError::ArgumentParseError("fraction-digits-arg"))
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
            Err(YangError::ArgumentParseError("status-arg"))
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
            Err(YangError::ArgumentParseError("config-arg"))
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
            Err(YangError::ArgumentParseError("ordered-by-arg"))
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
                Err(_) => Err(YangError::ArgumentParseError("min-value-arg")),
            }
        } else {
            Err(YangError::ArgumentParseError("min-value-arg"))
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
                Err(_) => Err(YangError::ArgumentParseError("max-value-arg"))
            }
        } else {
            Err(YangError::ArgumentParseError("max-value-arg"))
        }
    }
}

///
/// Integer Value str.
///
#[derive(Debug, Clone)]
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
    type Err = YangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rb = s.trim();

        if rb == "min" {
            Ok(RangeBoundary::Min)
        } else if rb == "max" {
            Ok(RangeBoundary::Max)
        } else if is_decimal_value(rb) {
            match rb.parse::<f64>() {
                Ok(num) => Ok(RangeBoundary::Decimal(num)),
                Err(_) => Err(YangError::ArgumentParseError("range-arg")),
            }
        } else if is_integer_value(rb) {
            match rb.parse::<i64>() {
                Ok(num) => Ok(RangeBoundary::Integer(num)),
                Err(_) => Err(YangError::ArgumentParseError("range-arg")),
            }
        } else {
            Err(YangError::ArgumentParseError("range-arg"))
        }
    }
}

pub type RangePart = (RangeBoundary, Option<RangeBoundary>);

///
/// The "range-arg".
///
#[derive(Debug, Clone, PartialEq)]
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

                lower = RangeBoundary::from_str(bounds[0])?;
                upper = None;
            } else if bounds.len() == 2 {
                if bounds[0] == "" || bounds[1] == "" {
                    return Err(YangError::ArgumentParseError("range-arg"));
                }
                lower = RangeBoundary::from_str(bounds[0])?;
                upper = Some(RangeBoundary::from_str(bounds[1])?);
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
    type Err = YangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lb = s.trim();

        if lb == "min" {
            Ok(LengthBoundary::Min)
        } else if lb == "max" {
            Ok(LengthBoundary::Max)
        } else if is_integer_value(lb) {
            match lb.parse::<u64>() {
                Ok(num) => Ok(LengthBoundary::Integer(num)),
                Err(_) => Err(YangError::ArgumentParseError("length-arg")),
            }
        } else {
            Err(YangError::ArgumentParseError("length-arg"))
        }
    }
}

pub type LengthPart = (LengthBoundary, Option<LengthBoundary>);

///
/// The "length-arg".
///
#[derive(Debug, Clone, PartialEq)]
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

                lower = LengthBoundary::from_str(bounds[0])?;
                upper = None;
            } else if bounds.len() == 2 {
                if bounds[0] == "" || bounds[1] == "" {
                    return Err(YangError::ArgumentParseError("length-arg"));
                }
                lower = LengthBoundary::from_str(bounds[0])?;
                upper = Some(LengthBoundary::from_str(bounds[1])?);
            } else {
                return Err(YangError::ArgumentParseError("length-arg"));
            }

            v.push((lower, upper));
        }

        Ok(LengthArg { parts: v })
    }
}

///
/// The "modifier-arg".
///
#[derive(Debug, Clone, PartialEq)]
pub struct ModifierArg {
}

impl StmtArg for ModifierArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        if str == "invert-match" {
            Ok(ModifierArg { })
        } else {
            Err(YangError::ArgumentParseError("modifier-arg"))
        }
    }
}

///
/// The "position-value-arg".
///
#[derive(Debug, Clone, PartialEq)]
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
/// The "path-arg".
///
#[derive(Debug, Clone, PartialEq)]
pub enum PathArg {
    AbsolutePath(AbsolutePath),
    RelativePath(RelativePath),
}

impl StmtArg for PathArg {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        if str.starts_with('/') {
            Ok(PathArg::AbsolutePath(AbsolutePath::from_str(&str)?))
        } else if str.starts_with("..") {
            Ok(PathArg::RelativePath(RelativePath::from_str(&str)?))
        } else {
            Err(YangError::ArgumentParseError("path-arg"))
        }
    }
}

/// "absolute-path".
#[derive(Debug, Clone, PartialEq)]
pub struct AbsolutePath {
    nodes: Vec<PathNode>,
}

impl FromStr for AbsolutePath {
    type Err = YangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = &s[..];
        let mut nodes = Vec::new();

        while s.len() > 0 {
            if !s.starts_with('/') {
                return Err(YangError::ArgumentParseError("absolute-path"))
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
                                None => Err(YangError::ArgumentParseError("path-predicate"))
                            }?;

                            path_predicate.push(PathPredicate::from_str(&s[..pos])?);
                            s = &s[pos..];

                            s.len() > 0 && s.starts_with('[')
                        } { };
                    }
                }
                None => {
                    node_identifier = NodeIdentifier::from_str(&s)?;
                    nodes.push(PathNode { node_identifier, path_predicate });
                    break;
                }
            }

            nodes.push(PathNode { node_identifier, path_predicate });
        }

        Ok(AbsolutePath { nodes })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathNode {
    node_identifier: NodeIdentifier,
    path_predicate: Vec<PathPredicate>,
}

/// "relative-path".
#[derive(Debug, Clone, PartialEq)]
pub struct RelativePath {
    up: u32,
    descendant_path: DescendantPath,
}

impl FromStr for RelativePath {
    type Err = YangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = &s[..];
        let mut up = 0;

        if !s.starts_with("../") {
            return Err(YangError::ArgumentParseError("relative-path"))
        }

        while {
            up += 1;
            s = &s[3..];
            s.len() > 0 && s.starts_with("../")
        } { };

        let descendant_path = DescendantPath::from_str(s)?;
        Ok(RelativePath{ up, descendant_path })
    }
}

/// "descendant-path".
#[derive(Debug, Clone, PartialEq)]
pub struct DescendantPath {
    node_identifier: NodeIdentifier,
    path_predicate: Vec<PathPredicate>,
    absolute_path: Option<AbsolutePath>,
}

impl FromStr for DescendantPath {
    type Err = YangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = &s[..];
        let node_identifier;
        let mut path_predicate = Vec::new();
        let mut absolute_path = None;

        if s.len() == 0 {
            return Err(YangError::ArgumentParseError("descendant-path"))
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
                            None => Err(YangError::ArgumentParseError("path-predicate"))
                        }?;

                        path_predicate.push(PathPredicate::from_str(&s[..pos])?);
                        s = &s[pos..];

                        s.len() > 0 && s.starts_with('[')
                    } { };
                }

                if s.len() > 0 {
                    absolute_path = Some(AbsolutePath::from_str(s)?);
                }
            }
            None => {
                node_identifier = NodeIdentifier::from_str(s)?;
            }
        }

        Ok(DescendantPath { node_identifier, path_predicate, absolute_path })
    }
}


/// "path-predicate".
#[derive(Debug, Clone, PartialEq)]
pub struct PathPredicate {
    path_equality_expr: PathEqualityExpr,
}

impl FromStr for PathPredicate {
    type Err = YangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with('[') || !s.ends_with(']') {
            Err(YangError::ArgumentParseError("path-predicate"))
        } else {
            Ok(PathPredicate {
                path_equality_expr: PathEqualityExpr::from_str(&s[1..s.len() - 1])?,
            })
        }
    }
}

/// "path-equality-expr".
#[derive(Debug, Clone, PartialEq)]
pub struct PathEqualityExpr {
    node_identifier: NodeIdentifier,
    path_key_expr: PathKeyExpr,
}

impl FromStr for PathEqualityExpr {
    type Err = YangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.find('=') {
            Some(p) => {
                Ok(PathEqualityExpr {
                    node_identifier: NodeIdentifier::from_str(&s[0..p].trim())?,
                    path_key_expr: PathKeyExpr::from_str(&s[p + 1..].trim())?,
                })
            }
            None => Err(YangError::ArgumentParseError("path-equality-expr"))
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
#[derive(Debug, Clone, PartialEq)]
pub struct PathKeyExpr {
    rel_path_keyexpr: String,
}

impl FromStr for PathKeyExpr {
    type Err = YangError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        // TBD: Validation only.
        let paths: Vec<_> = str.split("/").map(|s| s.trim()).collect();
        // Minimum of "current() / .. / node-identifier".
        if paths.len() < 3 {
            return Err(YangError::ArgumentParseError("path-key-expr"))
        // Invalid current function invocation.
        } else if !is_current_function_invocation(&paths[0]) {
            return Err(YangError::ArgumentParseError("path-key-expr"))
        // Validate rel-path-keyexpr.
        } else {
            let mut i = 1;

            if paths[i] != ".." {
                Err(YangError::ArgumentParseError("path-key-expr"))
            } else {
                i += 1;
                while i < paths.len() && paths[i] == ".." {
                    i += 1;
                }
                if i >= paths.len() {
                    return Err(YangError::ArgumentParseError("path-key-expr"))
                }

                if !is_node_identifier(paths[i]) {
                    return Err(YangError::ArgumentParseError("path-key-expr"))
                }

                while i < paths.len() {
                    if !is_node_identifier(paths[i]) {
                        return Err(YangError::ArgumentParseError("path-key-expr"))
                    }
                    i += 1;
                }

                Ok(PathKeyExpr { rel_path_keyexpr: str.to_string() })
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
        Tokenizer {
            str: s,
            pos: 0,
        }
    }

    pub fn line(&mut self) -> &str {
        &self.str[self.pos..]
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
        } else if self.line().starts_with("not") {
            self.pos += 3;
            IfFeatureToken::Not
        } else if self.line().starts_with("and") {
            self.pos += 3;
            IfFeatureToken::And
        } else if self.line().starts_with("or") {
            self.pos += 2;
            IfFeatureToken::Or
        } else {
            let p = match self.line().find(|c: char| !c.is_alphanumeric() && c != '-' && c != '_' && c != '.' && c != ':') {
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
#[derive(Clone, PartialEq)]
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
    pub fn parse(tokenizer: &mut Tokenizer) -> Result<Self, YangError> {
        let mut terms: Vec<IfFeatureTerm> = Vec::new();
        let mut factors: Vec<IfFeatureFactor> = Vec::new();
        let mut not: Option<bool> = None;
        let mut prev = IfFeatureToken::Init;

        loop {
            let token = tokenizer.get_token();

            if let IfFeatureToken::Init = prev {
                match token {
                    IfFeatureToken::Or |
                    IfFeatureToken::And => {
                        return Err(YangError::ArgumentParseError(""));  // TBD
                    }
                    _ => {}
                }
            }

            match token {
                IfFeatureToken::Init => {}
                IfFeatureToken::ParenBegin => {
//                    if prev == ")" || prev == "and" || prev == "(" {
//                        return Err(YangError::ArgumentParseError(""))  // TBD
//                    }

                    let expr = Box::new(IfFeatureExpr::parse(tokenizer)?);
                    factors.push(IfFeatureFactor::IfFeatureExpr((not.take(), expr)));
                }
                IfFeatureToken::ParenEnd => {
//                    if prev == "or" || prev == "and" || prev == "(" {
//                        return Err(YangError::ArgumentParseError(""))  // TBD
//                    }

                    break;
                }
                IfFeatureToken::Or => {
//                    if prev == "or" || prev == "and" || prev == "(" || prev == "not" {
//                        return Err(YangError::ArgumentParseError(""))  // TBD
//                    }

                    terms.push(IfFeatureTerm { factors: factors.drain(..).collect() });
                    factors = Vec::new();
                }
                IfFeatureToken::And => {

                }
                IfFeatureToken::Not => {
                    not.replace(true);
                }
                IfFeatureToken::IdentifierRef(str) => {
                    let identifier_ref = IdentifierRef::from_str(&str)?;
                    factors.push(IfFeatureFactor::IdentifierRef((not.take(), identifier_ref)));
                }
                IfFeatureToken::EndOfLine => break,
            }

//            prev = token;
        }

        terms.push(IfFeatureTerm { factors: factors.drain(..).collect() });
        Ok(IfFeatureExpr { terms })
    }
}

impl StmtArg for IfFeatureExpr {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;
        let mut tokenizer = Tokenizer::new(str);

        Ok(IfFeatureExpr::parse(&mut tokenizer)?)
    }
}

/// "if-feature-term".
#[derive(Debug, Clone, PartialEq)]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_identifier() {
        let s = " hello-world ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, Identifier { str: String::from("hello-world") }),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = " _123.IdEnT.456-789_ ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, Identifier { str: String::from("_123.IdEnT.456-789_") }),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = " 123 ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: identifier"),
        }

        let s = " 123$ ";
        let mut parser = Parser::new(s.to_string());

        match Identifier::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: identifier"),
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
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: identifier"),
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
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: identifier"),
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
    pub fn test_fraction_digits_arg() {
        let s = "18";
        let mut parser = Parser::new(s.to_string());

        match FractionDigitsArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg.digits(), 18),
            Err(err) => panic!("{:?}", err.to_string()),
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

    #[test]
    pub fn test_range_arg() {
        let s = r#""1..10""#;
        let mut parser = Parser::new(s.to_string());

        match RangeArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, RangeArg { parts: vec![(RangeBoundary::Integer(1), Some(RangeBoundary::Integer(10)))]}),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""1 .. 10 | 21..30""#;
        let mut parser = Parser::new(s.to_string());

        match RangeArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, RangeArg { parts: vec![(RangeBoundary::Integer(1), Some(RangeBoundary::Integer(10))),
                                                               (RangeBoundary::Integer(21), Some(RangeBoundary::Integer(30))),
            ]}),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""min..max""#;
        let mut parser = Parser::new(s.to_string());

        match RangeArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, RangeArg { parts: vec![(RangeBoundary::Min, Some(RangeBoundary::Max)),
            ]}),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""min..""#;
        let mut parser = Parser::new(s.to_string());

        match RangeArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: range-arg"),
        }

        let s = r#""1.01 .. 1.99""#;
        let mut parser = Parser::new(s.to_string());

        match RangeArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, RangeArg { parts: vec![(RangeBoundary::Decimal(1.01), Some(RangeBoundary::Decimal(1.99)))]}),
            Err(err) => panic!("{:?}", err.to_string()),
        }
    }

    #[test]
    pub fn test_length_arg() {
        let s = r#""1..10""#;
        let mut parser = Parser::new(s.to_string());

        match LengthArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, LengthArg { parts: vec![(LengthBoundary::Integer(1), Some(LengthBoundary::Integer(10)))]}),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""1 .. 10 | 21..30""#;
        let mut parser = Parser::new(s.to_string());

        match LengthArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, LengthArg { parts: vec![(LengthBoundary::Integer(1), Some(LengthBoundary::Integer(10))),
                                                               (LengthBoundary::Integer(21), Some(LengthBoundary::Integer(30))),
            ]}),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""min..max""#;
        let mut parser = Parser::new(s.to_string());

        match LengthArg::parse_arg(&mut parser) {
            Ok(arg) => assert_eq!(arg, LengthArg { parts: vec![(LengthBoundary::Min, Some(LengthBoundary::Max)),
            ]}),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = r#""min..""#;
        let mut parser = Parser::new(s.to_string());

        match LengthArg::parse_arg(&mut parser) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: length-arg"),
        }
    }

    #[test]
    pub fn test_path_key_expr() {
        let s = "current()/../node";
        match PathKeyExpr::from_str(s) {
            Ok(arg) => assert_eq!(arg, PathKeyExpr { rel_path_keyexpr: "current()/../node".to_string() }),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = "current()/../../../node";
        match PathKeyExpr::from_str(s) {
            Ok(arg) => assert_eq!(arg, PathKeyExpr { rel_path_keyexpr: "current()/../../../node".to_string() }),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = "current()/../../../node/node/node";
        match PathKeyExpr::from_str(s) {
            Ok(arg) => assert_eq!(arg, PathKeyExpr { rel_path_keyexpr: "current()/../../../node/node/node".to_string() }),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = "current ( ) / .. / .. / .. / node / node / node ";
        match PathKeyExpr::from_str(s) {
            Ok(arg) => assert_eq!(arg, PathKeyExpr { rel_path_keyexpr: "current ( ) / .. / .. / .. / node / node / node ".to_string() }),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = "current()/..";
        match PathKeyExpr::from_str(s) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: path-key-expr"),
        }

        let s = "current()/node";
        match PathKeyExpr::from_str(s) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err.to_string(), "Argument parse error: path-key-expr"),
        }
    }

    #[test]
    pub fn test_absolute_path() {
        let s = "/node1/node2[id=current()/../rel1/rel2]";
        let path = AbsolutePath {
            nodes: vec![PathNode { node_identifier: NodeIdentifier::from_str("node1").unwrap(), path_predicate: vec![] },
                        PathNode { node_identifier: NodeIdentifier::from_str("node2").unwrap(), path_predicate:
                                   vec![PathPredicate { path_equality_expr: PathEqualityExpr { node_identifier: NodeIdentifier::from_str("id").unwrap(),
                                                                                               path_key_expr: PathKeyExpr { rel_path_keyexpr: "current()/../rel1/rel2".to_string() } } }] }] };

        match AbsolutePath::from_str(s) {
            Ok(p) => assert_eq!(p, path),
            Err(err) => panic!("{:?}", err.to_string()),
        }

        let s = "/node1/node2[id1=current()/../rel1/rel2][prefix:id2=current()/../../rel3/rel4]/node3";
        let path = AbsolutePath {
            nodes: vec![PathNode { node_identifier: NodeIdentifier::from_str("node1").unwrap(), path_predicate: vec![] },
                        PathNode { node_identifier: NodeIdentifier::from_str("node2").unwrap(), path_predicate:
                                   vec![PathPredicate { path_equality_expr: PathEqualityExpr { node_identifier: NodeIdentifier::from_str("id1").unwrap(),
                                                                                               path_key_expr: PathKeyExpr { rel_path_keyexpr: "current()/../rel1/rel2".to_string() } } },
                                        PathPredicate { path_equality_expr: PathEqualityExpr { node_identifier: NodeIdentifier::from_str("prefix:id2").unwrap(),
                                                                                               path_key_expr: PathKeyExpr { rel_path_keyexpr: "current()/../../rel3/rel4".to_string() } } }] },
                        PathNode { node_identifier: NodeIdentifier::from_str("node3").unwrap(), path_predicate: vec![] },
            ] };

        match AbsolutePath::from_str(s) {
            Ok(p) => assert_eq!(p, path),
            Err(err) => panic!("{:?}", err.to_string()),
        }
    }

    #[test]
    pub fn test_descendant_path() {
        let s = "node0/node1/node2[id=current()/../rel1/rel2]";

        let absolute_path = AbsolutePath {
            nodes: vec![PathNode { node_identifier: NodeIdentifier::from_str("node1").unwrap(), path_predicate: vec![] },
                        PathNode { node_identifier: NodeIdentifier::from_str("node2").unwrap(), path_predicate:
                                   vec![PathPredicate { path_equality_expr: PathEqualityExpr { node_identifier: NodeIdentifier::from_str("id").unwrap(),
                                                                                               path_key_expr: PathKeyExpr { rel_path_keyexpr: "current()/../rel1/rel2".to_string() } } }] }] };
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
            nodes: vec![PathNode { node_identifier: NodeIdentifier::from_str("node1").unwrap(), path_predicate: vec![] },
                        PathNode { node_identifier: NodeIdentifier::from_str("node2").unwrap(), path_predicate: vec![] }]};

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
            Ok(expr) =>
                assert_eq!(format!("{:?}", expr), "p1:id1 and p1:id2 or (p2:id3 and p2:id4) or not p3:id5"),
            Err(_) => panic!(),
        }

        let s = r#""p1:id1 p1:id2""#;
        let mut parser = Parser::new(s.to_string());

        match IfFeatureExpr::parse_arg(&mut parser) {
            Ok(expr) =>
                assert_eq!(format!("{:?}", expr), "p1:id1 and p1:id2 or (p2:id3 and p2:id4) or not p3:id5"),
            Err(_) => panic!(),

        }
    }
}
