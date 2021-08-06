//
// YANG - YANG core rules and enums.
//   Copyright (C) 2021 Toshiaki Takada
//

use std::fmt;
use std::collections::HashMap;

use super::error::*;
use super::parser::*;
use super::arg::*;
use super::stmt::*;

// Statement Parser initialization.
lazy_static! {
    static ref STMT_PARSER: HashMap<&'static str, StmtParserFn> = {
        let mut m = HashMap::new();

        m.insert("module", ModuleStmt::parse as StmtParserFn);
        m.insert("submodule", SubmoduleStmt::parse as StmtParserFn);
        m.insert("yang-version", YangVersionStmt::parse as StmtParserFn);
        m.insert("import", ImportStmt::parse as StmtParserFn);
        m.insert("include", IncludeStmt::parse as StmtParserFn);
        m.insert("namespace", NamespaceStmt::parse as StmtParserFn);
        m.insert("belongs-to", BelongsToStmt::parse as StmtParserFn);
        m.insert("prefix", PrefixStmt::parse as StmtParserFn);
        m.insert("organization", OrganizationStmt::parse as StmtParserFn);
        m.insert("contact", ContactStmt::parse as StmtParserFn);
        m.insert("description", DescriptionStmt::parse as StmtParserFn);
        m.insert("reference", ReferenceStmt::parse as StmtParserFn);
        m.insert("units", UnitsStmt::parse as StmtParserFn);
        m.insert("revision", RevisionStmt::parse as StmtParserFn);
        m.insert("revision-date", RevisionDateStmt::parse as StmtParserFn);
        m.insert("extension", ExtensionStmt::parse as StmtParserFn);
        m.insert("argument", ArgumentStmt::parse as StmtParserFn);
        m.insert("yin-element", YinElementStmt::parse as StmtParserFn);
/*
        m.insert("identity", IdentityStmt::parse as StmtParserFn);
        m.insert("base", BaseStmt::parse as StmtParserFn);
        m.insert("feature", FeatureStmt::parse as StmtParserFn);
        m.insert("if-feature", IfFeatureStmt::parse as StmtParserFn);
        m.insert("typedef", TypedefStmt::parse as StmtParserFn);
        m.insert("type", TypeStmt::parse as StmtParserFn);
        m.insert("range", RangeStmt::parse as StmtParserFn);
*/
        m.insert("fraction-digits", FractionDigitsStmt::parse as StmtParserFn);
/*
        m.insert("length", LengthStmt::parse as StmtParserFn);
        m.insert("pattern", PatternStmt::parse as StmtParserFn);
        m.insert("modifier", ModifierStmt::parse as StmtParserFn);
        m.insert("default", DefaultStmt::parse as StmtParserFn);
        m.insert("enum", EnumStmt::parse as StmtParserFn);
        m.insert("path", PathStmt::parse as StmtParserFn);
        m.insert("require-instance", RequireInstanceStmt::parse as StmtParserFn);
        m.insert("bit", BitStmt::parse as StmtParserFn);
        m.insert("position", PositionStmt::parse as StmtParserFn);
*/
        m.insert("status", StatusStmt::parse as StmtParserFn);
        m.insert("config", ConfigStmt::parse as StmtParserFn);
        m.insert("mandatory", MandatoryStmt::parse as StmtParserFn);
        m.insert("presense", PresenseStmt::parse as StmtParserFn);
        m.insert("ordered-by", OrderedByStmt::parse as StmtParserFn);
        m.insert("must", MustStmt::parse as StmtParserFn);
        m.insert("error-message", ErrorMessageStmt::parse as StmtParserFn);
        m.insert("error-app-tag", ErrorAppTagStmt::parse as StmtParserFn);
        m.insert("min-elements", MinElementsStmt::parse as StmtParserFn);
        m.insert("max-elements", MaxElementsStmt::parse as StmtParserFn);
        m.insert("value", ValueStmt::parse as StmtParserFn);
/*
        m.insert("grouping", GroupingStmt::parse as StmtParserFn);
        m.insert("container", ContainerStmt::parse as StmtParserFn);
        m.insert("leaf", LeafStmt::parse as StmtParserFn);
        m.insert("leaf-list", LeafListStmt::parse as StmtParserFn);
        m.insert("list", ListStmt::parse as StmtParserFn);
        m.insert("key", KeyStmt::parse as StmtParserFn);
        m.insert("unique", UniqueStmt::parse as StmtParserFn);
        m.insert("choice", ChoiceStmt::parse as StmtParserFn);
        m.insert("short-case", ShortCaseStmt::parse as StmtParserFn);
        m.insert("case", CaseStmt::parse as StmtParserFn);
        m.insert("anydata", AnydataStmt::parse as StmtParserFn);
        m.insert("anyxml", AnyxmlStmt::parse as StmtParserFn);
        m.insert("uses", UsesStmt::parse as StmtParserFn);
        m.insert("refine", RefineStmt::parse as StmtParserFn);
        m.insert("uses-augment", UsesAugmentStmt::parse as StmtParserFn);
        m.insert("augment", AugmentStmt::parse as StmtParserFn);
        m.insert("when", WhenStmt::parse as StmtParserFn);
        m.insert("rpc", RpcStmt::parse as StmtParserFn);
        m.insert("action", ActionStmt::parse as StmtParserFn);
        m.insert("input", InputStmt::parse as StmtParserFn);
        m.insert("output", OutputStmt::parse as StmtParserFn);
        m.insert("notification", NotificationStmt::parse as StmtParserFn);
        m.insert("deviation", DeviationStmt::parse as StmtParserFn);
        m.insert("deviation-not-supported", DeviationNotSupportedStmt::parse as StmtParserFn);
        m.insert("deviate-add", DeviateAddStmt::parse as StmtParserFn);
        m.insert("deviate-delete", DeviateDeleteStmt::parse as StmtParserFn);
        m.insert("deviata-replace", DeviateReplaceStmt::parse as StmtParserFn);
*/
        m
    };
}

// Collection of statements in HashMap.
type StmtCollection = HashMap<String, Vec<StmtType>>;

// Statement Parser callback type.
type StmtParserFn = fn(&mut Parser) -> Result<StmtType, YangError>;

// Parse a single statement.
fn call_stmt_parser(parser: &mut Parser, keyword: &str) -> Result<StmtType, YangError> {
    let f = STMT_PARSER.get(keyword).unwrap();
    f(parser)
}

// Get a list of statements in any order.
pub fn parse_stmt_collection(parser: &mut Parser, map: HashMap<&'static str, Repeat>) -> Result<StmtCollection, YangError> {
    let mut stmts: StmtCollection = HashMap::new();

    loop {
        let token = parser.get_token()?;
println!("*** parse_stmts {:?}", token);
        match token {
            Token::Identifier(ref keyword) => {
                if map.contains_key(keyword as &str) {
                    let stmt = call_stmt_parser(parser, &keyword)?;
                    let v =  match stmts.get_mut(keyword as &str) {
                        Some(v) => v,
                        None => {
                            stmts.insert(keyword.to_string(), Vec::new());
                            stmts.get_mut(keyword as &str).unwrap()
                        }
                    };
                    v.push(stmt);
                } else {
                    parser.save_token(token);
                    break;
                }
            }
            _ => {
                parser.save_token(token);
                break;
            }
        }
    }

    // Validation against repetition.
    for (k, rep) in map.iter() {
        let n = match stmts.get(&k.to_string()) {
            Some(v) => v.len(),
            None => 0,
        };

        if !rep.validate(n) {
            return Err(YangError::StatementMismatch(k));
        }
    }

    Ok(stmts)
}

// Yang Statement
pub enum StmtType {
    ModuleStmt(ModuleStmt),
    SubmoduleStmt(SubmoduleStmt),
    YangVersionStmt(YangVersionStmt),
    ImportStmt(ImportStmt),
    IncludeStmt(IncludeStmt),
    NamespaceStmt(NamespaceStmt),
    PrefixStmt(PrefixStmt),
    BelongsToStmt(BelongsToStmt),
    OrganizationStmt(OrganizationStmt),
    ContactStmt(ContactStmt),
    DescriptionStmt(DescriptionStmt),
    ReferenceStmt(ReferenceStmt),
    UnitsStmt(UnitsStmt),
    RevisionStmt(RevisionStmt),
    RevisionDateStmt(RevisionDateStmt),
    ExtensionStmt(ExtensionStmt),
    ArgumentStmt(ArgumentStmt),
    YinElementStmt(YinElementStmt),
/*
    IdentityStmt(IdentityStmt),
    BaseStmt(BaseStmt),
    FeatureStmt(FeatureStmt),
    IfFeatureStmt(IfFeatureStmt),
    TypedefStmt(TypedefStmt),
    TypeStmt(TypeStmt),
    RangeStmt(RangeStmt),
*/
    FractionDigitsStmt(FractionDigitsStmt),
/*
    LengthStmt(LengthStmt),
    PatternStmt(PatternStmt),
    ModifierStmt(ModifierStmt),
    DefaultStmt(DefaultStmt),
    EnumStmt(EnumStmt),
    PathStmt(PathStmt),
    RequireInstanceStmt(RequireInstanceStmt),
    BitStmt(BitStmt),
    PositionStmt(PositionStmt),
*/
    StatusStmt(StatusStmt),
    ConfigStmt(ConfigStmt),
    MandatoryStmt(MandatoryStmt),
    PresenseStmt(PresenseStmt),
    OrderedByStmt(OrderedByStmt),
    MustStmt(MustStmt),
    ErrorMessageStmt(ErrorMessageStmt),
    ErrorAppTagStmt(ErrorAppTagStmt),
    MinElementsStmt(MinElementsStmt),
    MaxElementsStmt(MaxElementsStmt),
    ValueStmt(ValueStmt),
/*
    GroupingStmt(GroupingStmt),
    ContainerStmt(ContainerStmt),
    LeafStmt(LeafStmt),
    LeafListStmt(LeafListStmt),
    ListStmt(ListStmt),
    KeyStmt(KeyStmt),
    UniqueStmt(UniqueStmt),
    ChoiceStmt(ChoiceStmt),
    ShortCaseStmt(ShortCaseStmt),
    CaseStmt(CaseStmt),
    AnydataStmt(AnydataStmt),
    AnyxmlStmt(AnyxmlStmt),
    UsesStmt(UsesStmt),
    RefineStmt(RefineStmt),
    UsesAugmentStmt(UsesAugmentStmt),
    AugmentStmt(AugmentStmt),
    WhenStmt(WhenStmt),
    RpcStmt(RpcStmt),
    ActionStmt(ActionStmt),
    InputStmt(InputStmt),
    OutputStmt(OutputStmt),
    NotificationStmt(NotificationStmt),
    DeviationStmt(DeviationStmt),
    DeviationNotSupportedStmt(DeviationNotSupportedStmt),
    DeviateAddStmt(DeviateAddStmt),
    DeviateDeleteStmt(DeviateDeleteStmt),
    DeviateReplaceStmt(DeviateReplaceStmt),
*/
}

impl fmt::Debug for StmtType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match &*self {
            StmtType::ModuleStmt(stmt) => write!(f, "module-stmt {:?}", stmt),
            StmtType::SubmoduleStmt(stmt) => write!(f, "submodule-stmt {:?}", stmt),
            StmtType::YangVersionStmt(stmt) => write!(f, "yang-version-stmt {:?}", stmt),
            StmtType::ImportStmt(stmt) => write!(f, "import-stmt {:?}", stmt),
            StmtType::IncludeStmt(stmt) => write!(f, "include-stmt {:?}", stmt),
            StmtType::NamespaceStmt(stmt) => write!(f, "namespace-stmt {:?}", stmt),
            StmtType::PrefixStmt(stmt) => write!(f, "prefix-stmt {:?}", stmt),
            StmtType::BelongsToStmt(stmt) => write!(f, "belongs-to-stmt {:?}", stmt),
            StmtType::OrganizationStmt(stmt) => write!(f, "organization-stmt {:?}", stmt),
            StmtType::ContactStmt(stmt) => write!(f, "contact-stmt {:?}", stmt),
            StmtType::DescriptionStmt(stmt) => write!(f, "description-stmt {:?}", stmt),
            StmtType::ReferenceStmt(stmt) => write!(f, "reference-stmt {:?}", stmt),
            StmtType::UnitsStmt(stmt) => write!(f, "units-stmt {:?}", stmt),
            StmtType::RevisionStmt(stmt) => write!(f, "revision-stmt {:?}", stmt),
            StmtType::RevisionDateStmt(stmt) => write!(f, "revision-date-stmt {:?}", stmt),
            StmtType::ExtensionStmt(stmt) => write!(f, "extension-stmt {:?}", stmt),
            StmtType::ArgumentStmt(stmt) => write!(f, "argument-stmt {:?}", stmt),
            StmtType::YinElementStmt(stmt) => write!(f, "yin-element-stmt {:?}", stmt),
/*
            StmtType::IdentityStmt(stmt) => write!(f, "identity-stmt {:?}", stmt),
            StmtType::BaseStmt(stmt) => write!(f, "base-stmt {:?}", stmt),
            StmtType::FeatureStmt(stmt) => write!(f, "feature-stmt {:?}", stmt),
            StmtType::IfFeatureStmt(stmt) => write!(f, "if-feature-stmt {:?}", stmt),
            StmtType::TypedefStmt(stmt) => write!(f, "typedef-stmt {:?}", stmt),
            StmtType::TypeStmt(stmt) => write!(f, "type-stmt {:?}", stmt),
            StmtType::RangeStmt(stmt) => write!(f, "range-stmt {:?}", stmt),
*/
            StmtType::FractionDigitsStmt(stmt) => write!(f, "fraction-digits-stmt {:?}", stmt),
/*
            StmtType::LengthStmt(stmt) => write!(f, "length-stmt {:?}", stmt),
            StmtType::PatternStmt(stmt) => write!(f, "pattern-stmt {:?}", stmt),
            StmtType::ModifierStmt(stmt) => write!(f, "modifier-stmt {:?}", stmt),
            StmtType::DefaultStmt(stmt) => write!(f, "default-stmt {:?}", stmt),
            StmtType::EnumStmt(stmt) => write!(f, "enum-stmt {:?}", stmt),
            StmtType::PathStmt(stmt) => write!(f, "path-stmt {:?}", stmt),
            StmtType::RequireInstanceStmt(stmt) => write!(f, "require-instance-stmt {:?}", stmt),
            StmtType::BitStmt(stmt) => write!(f, "bit-stmt {:?}", stmt),
            StmtType::PositionStmt(stmt) => write!(f, "position-stmt {:?}", stmt),
*/
            StmtType::StatusStmt(stmt) => write!(f, "status-stmt {:?}", stmt),
            StmtType::ConfigStmt(stmt) => write!(f, "config-stmt {:?}", stmt),
            StmtType::MandatoryStmt(stmt) => write!(f, "mandatory-stmt {:?}", stmt),
            StmtType::PresenseStmt(stmt) => write!(f, "presense-stmt {:?}", stmt),
            StmtType::OrderedByStmt(stmt) => write!(f, "ordered-by-stmt {:?}", stmt),
            StmtType::MustStmt(stmt) => write!(f, "must-stmt {:?}", stmt),
            StmtType::ErrorMessageStmt(stmt) => write!(f, "error-message-stmt {:?}", stmt),
            StmtType::ErrorAppTagStmt(stmt) => write!(f, "error-app-tag-stmt {:?}", stmt),
            StmtType::MinElementsStmt(stmt) => write!(f, "min-elements-stmt {:?}", stmt),
            StmtType::MaxElementsStmt(stmt) => write!(f, "max-elements-stmt {:?}", stmt),
            StmtType::ValueStmt(stmt) => write!(f, "value-stmt {:?}", stmt),
/*
            StmtType::GroupingStmt(stmt) => write!(f, "grouping-stmt {:?}", stmt),
            StmtType::ContainerStmt(stmt) => write!(f, "container-stmt {:?}", stmt),
            StmtType::LeafStmt(stmt) => write!(f, "leaf-stmt {:?}", stmt),
            StmtType::LeafListStmt(stmt) => write!(f, "leaf-list-stmt {:?}", stmt),
            StmtType::ListStmt(stmt) => write!(f, "list-stmt {:?}", stmt),
            StmtType::KeyStmt(stmt) => write!(f, "key-stmt {:?}", stmt),
            StmtType::UniqueStmt(stmt) => write!(f, "unique-stmt {:?}", stmt),
            StmtType::ChoiceStmt(stmt) => write!(f, "choice-stmt {:?}", stmt),
            StmtType::ShortCaseStmt(stmt) => write!(f, "short-case-stmt {:?}", stmt),
            StmtType::CaseStmt(stmt) => write!(f, "case-stmt {:?}", stmt),
            StmtType::AnydataStmt(stmt) => write!(f, "anydata-stmt {:?}", stmt),
            StmtType::AnyxmlStmt(stmt) => write!(f, "anyxml-stmt {:?}", stmt),
            StmtType::UsesStmt(stmt) => write!(f, "uses-stmt {:?}", stmt),
            StmtType::RefineStmt(stmt) => write!(f, "refine-stmt {:?}", stmt),
            StmtType::UsesAugmentStmt(stmt) => write!(f, "uses-augment-stmt {:?}", stmt),
            StmtType::AugmentStmt(stmt) => write!(f, "augment-stmt {:?}", stmt),
            StmtType::WhenStmt(stmt) => write!(f, "when-stmt {:?}", stmt),
            StmtType::RpcStmt(stmt) => write!(f, "rpc-stmt {:?}", stmt),
            StmtType::ActionStmt(stmt) => write!(f, "action-stmt {:?}", stmt),
            StmtType::InputStmt(stmt) => write!(f, "input-stmt {:?}", stmt),
            StmtType::OutputStmt(stmt) => write!(f, "output-stmt {:?}", stmt),
            StmtType::NotificationStmt(stmt) => write!(f, "notification-stmt {:?}", stmt),
            StmtType::DeviationStmt(stmt) => write!(f, "deviation-stmt {:?}", stmt),
            StmtType::DeviationNotSupportedStmt(stmt) => write!(f, "deviation-not-supported-stmt {:?}", stmt),
            StmtType::DeviateAddStmt(stmt) => write!(f, "deviate-add-stmt {:?}", stmt),
            StmtType::DeviateDeleteStmt(stmt) => write!(f, "deviate-delete-stmt {:?}", stmt),
            StmtType::DeviateReplaceStmt(stmt) => write!(f, "deviate-replace-stmt {:?}", stmt),
*/
        }
    }
}

// String helper for core rules.
pub fn is_integer_value(s: &str) -> bool {
    if s.starts_with("-") {
        is_non_negative_integer_value(&s[1..])
    } else {
        is_non_negative_integer_value(s)
    }
}

pub fn is_non_negative_integer_value(s: &str) -> bool {
    s == "0" || is_positive_integer_value(s)
}

pub fn is_positive_integer_value(s: &str) -> bool {
    let mut chars = s.chars();
    let c = chars.next().unwrap();

    if c != '0' && c.is_ascii_digit() {
        chars.all(|c: char| c.is_ascii_digit())
    } else {
        false
    }
}

fn is_zero_integer_value(s: &str) -> bool {
    s.chars().all(|c: char| c.is_ascii_digit())
}

pub fn is_decimal_value(s: &str) -> bool {
    if let Some(p) = s.find('.') {
        let is = &s[..p];
        let fs = &s[p + 1..];

        if is_integer_value(is) && fs.len() > 0 {
            is_zero_integer_value(fs)
        } else {
            false
        }
    } else {
        false
    }
}

pub fn parse_range_boundary(s: &str) -> Result<RangeBoundary, YangError> {
    let rb = s.trim();

    if s == "min" {
        Ok(RangeBoundary::Min)
    } else if s == "max" {
        Ok(RangeBoundary::Max)
    } else if is_decimal_value(s) {
        match s.parse::<f64>() {
            Ok(num) => Ok(RangeBoundary::Decimal(num)),
            Err(_) => Err(YangError::ArgumentParseError("range-arg".to_string())),
        }
    } else if is_integer_value(s) {
        match s.parse::<i64>() {
            Ok(num) => Ok(RangeBoundary::Integer(num)),
            Err(_) => Err(YangError::ArgumentParseError("range-arg".to_string())),
        }
    } else {
        Err(YangError::ArgumentParseError("range-arg".to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_integer_value() {
        let s = "0123456789";
        assert_eq!(is_integer_value(&s), false);

        let s = "0";
        assert_eq!(is_integer_value(&s), true);

        let s = "1234567890";
        assert_eq!(is_integer_value(&s), true);

        let s = "abc";
        assert_eq!(is_integer_value(&s), false);

        let s = "-1";
        assert_eq!(is_integer_value(&s), true);

        let s = "3.14159265";
        assert_eq!(is_integer_value(&s), false);
    }

    #[test]
    pub fn test_non_negative_integer_value() {
        let s = "0123456789";
        assert_eq!(is_non_negative_integer_value(&s), false);

        let s = "0";
        assert_eq!(is_non_negative_integer_value(&s), true);

        let s = "1234567890";
        assert_eq!(is_non_negative_integer_value(&s), true);

        let s = "abc";
        assert_eq!(is_non_negative_integer_value(&s), false);

        let s = "-1";
        assert_eq!(is_non_negative_integer_value(&s), false);

        let s = "3.14159265";
        assert_eq!(is_non_negative_integer_value(&s), false);
    }

    #[test]
    pub fn test_positive_integer_value() {
        let s = "0123456789";
        assert_eq!(is_positive_integer_value(&s), false);

        let s = "0";
        assert_eq!(is_positive_integer_value(&s), false);

        let s = "1234567890";
        assert_eq!(is_positive_integer_value(&s), true);

        let s = "abc";
        assert_eq!(is_positive_integer_value(&s), false);

        let s = "-1";
        assert_eq!(is_positive_integer_value(&s), false);

        let s = "3.14159265";
        assert_eq!(is_positive_integer_value(&s), false);
    }

    #[test]
    pub fn test_zero_integer_value() {
        let s = "0123456789";
        assert_eq!(is_zero_integer_value(&s), true);

        let s = "0";
        assert_eq!(is_zero_integer_value(&s), true);

        let s = "1234567890";
        assert_eq!(is_zero_integer_value(&s), true);

        let s = "abc";
        assert_eq!(is_zero_integer_value(&s), false);

        let s = "-1";
        assert_eq!(is_zero_integer_value(&s), false);

        let s = "3.14159265";
        assert_eq!(is_zero_integer_value(&s), false);
    }

    #[test]
    pub fn test_decimal_value() {
        let s = "0123456789";
        assert_eq!(is_decimal_value(&s), false);

        let s = "0";
        assert_eq!(is_decimal_value(&s), false);

        let s = "1234567890";
        assert_eq!(is_decimal_value(&s), false);

        let s = "abc";
        assert_eq!(is_decimal_value(&s), false);

        let s = "-1.0";
        assert_eq!(is_decimal_value(&s), true);

        let s = "3.14159265";
        assert_eq!(is_decimal_value(&s), true);
    }
}