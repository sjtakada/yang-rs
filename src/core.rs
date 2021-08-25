//
// YANG - YANG core rules and enums.
//   Copyright (C) 2021 Toshiaki Takada
//

use std::fmt;
use std::collections::HashMap;

use super::error::*;
use super::parser::*;
use super::stmt::*;

// Statement Parser initialization.
lazy_static! {
    pub static ref STMT_PARSER: HashMap<Keyword, StmtParserFn> = {
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
        m.insert("identity", IdentityStmt::parse as StmtParserFn);
        m.insert("base", BaseStmt::parse as StmtParserFn);
        m.insert("feature", FeatureStmt::parse as StmtParserFn);
        m.insert("if-feature", IfFeatureStmt::parse as StmtParserFn);
        m.insert("typedef", TypedefStmt::parse as StmtParserFn);
        m.insert("type", TypeStmt::parse as StmtParserFn);
        m.insert("range", RangeStmt::parse as StmtParserFn);
        m.insert("fraction-digits", FractionDigitsStmt::parse as StmtParserFn);
        m.insert("length", LengthStmt::parse as StmtParserFn);
        m.insert("pattern", PatternStmt::parse as StmtParserFn);
        m.insert("modifier", ModifierStmt::parse as StmtParserFn);
        m.insert("default", DefaultStmt::parse as StmtParserFn);
        m.insert("enum", EnumStmt::parse as StmtParserFn);
        m.insert("path", PathStmt::parse as StmtParserFn);
        m.insert("require-instance", RequireInstanceStmt::parse as StmtParserFn);
        m.insert("bit", BitStmt::parse as StmtParserFn);
        m.insert("position", PositionStmt::parse as StmtParserFn);
        m.insert("status", StatusStmt::parse as StmtParserFn);
        m.insert("config", ConfigStmt::parse as StmtParserFn);
        m.insert("mandatory", MandatoryStmt::parse as StmtParserFn);
        m.insert("presence", PresenceStmt::parse as StmtParserFn);
        m.insert("ordered-by", OrderedByStmt::parse as StmtParserFn);
        m.insert("must", MustStmt::parse as StmtParserFn);
        m.insert("error-message", ErrorMessageStmt::parse as StmtParserFn);
        m.insert("error-app-tag", ErrorAppTagStmt::parse as StmtParserFn);
        m.insert("min-elements", MinElementsStmt::parse as StmtParserFn);
        m.insert("max-elements", MaxElementsStmt::parse as StmtParserFn);
        m.insert("value", ValueStmt::parse as StmtParserFn);
        m.insert("grouping", GroupingStmt::parse as StmtParserFn);
        m.insert("container", ContainerStmt::parse as StmtParserFn);
        m.insert("leaf", LeafStmt::parse as StmtParserFn);
        m.insert("leaf-list", LeafListStmt::parse as StmtParserFn);
        m.insert("list", ListStmt::parse as StmtParserFn);
        m.insert("key", KeyStmt::parse as StmtParserFn);
        m.insert("unique", UniqueStmt::parse as StmtParserFn);
        m.insert("choice", ChoiceStmt::parse as StmtParserFn);
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
        m.insert("deviate", DeviateStmt::parse as StmtParserFn);
        m
    };
}

// Keyword.
pub type Keyword = &'static str;

// Statement collection.
pub type StmtCollection = HashMap<String, Vec<StmtType>>;

// Statement Parser callback type.
type StmtParserFn = fn(&mut Parser) -> Result<StmtType, YangError>;

// Yang Statement
#[derive(PartialEq)]
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
    IdentityStmt(IdentityStmt),
    BaseStmt(BaseStmt),
    FeatureStmt(FeatureStmt),
    IfFeatureStmt(IfFeatureStmt),
    TypedefStmt(TypedefStmt),
    TypeStmt(TypeStmt),
    RangeStmt(RangeStmt),
    FractionDigitsStmt(FractionDigitsStmt),
    LengthStmt(LengthStmt),
    PatternStmt(PatternStmt),
    ModifierStmt(ModifierStmt),
    DefaultStmt(DefaultStmt),
    EnumStmt(EnumStmt),
    PathStmt(PathStmt),
    RequireInstanceStmt(RequireInstanceStmt),
    BitStmt(BitStmt),
    PositionStmt(PositionStmt),
    StatusStmt(StatusStmt),
    ConfigStmt(ConfigStmt),
    MandatoryStmt(MandatoryStmt),
    PresenceStmt(PresenceStmt),
    OrderedByStmt(OrderedByStmt),
    MustStmt(MustStmt),
    ErrorMessageStmt(ErrorMessageStmt),
    ErrorAppTagStmt(ErrorAppTagStmt),
    MinElementsStmt(MinElementsStmt),
    MaxElementsStmt(MaxElementsStmt),
    ValueStmt(ValueStmt),
    GroupingStmt(GroupingStmt),
    ContainerStmt(ContainerStmt),
    LeafStmt(LeafStmt),
    LeafListStmt(LeafListStmt),
    ListStmt(ListStmt),
    KeyStmt(KeyStmt),
    UniqueStmt(UniqueStmt),
    ChoiceStmt(ChoiceStmt),
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
    DeviateStmt(DeviateStmt),
}

impl fmt::Debug for StmtType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match &*self {
            StmtType::ModuleStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::SubmoduleStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::YangVersionStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ImportStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::IncludeStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::NamespaceStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::PrefixStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::BelongsToStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::OrganizationStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ContactStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::DescriptionStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ReferenceStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::UnitsStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::RevisionStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::RevisionDateStmt(stmt) => write!(f,"{:?}", stmt),
            StmtType::ExtensionStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ArgumentStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::YinElementStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::IdentityStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::BaseStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::FeatureStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::IfFeatureStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::TypedefStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::TypeStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::RangeStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::FractionDigitsStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::LengthStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::PatternStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ModifierStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::DefaultStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::EnumStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::PathStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::RequireInstanceStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::BitStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::PositionStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::StatusStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ConfigStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::MandatoryStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::PresenceStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::OrderedByStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::MustStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ErrorMessageStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ErrorAppTagStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::MinElementsStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::MaxElementsStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ValueStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::GroupingStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ContainerStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::LeafStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::LeafListStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ListStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::KeyStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::UniqueStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ChoiceStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::CaseStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::AnydataStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::AnyxmlStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::UsesStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::RefineStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::UsesAugmentStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::AugmentStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::WhenStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::RpcStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::ActionStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::InputStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::OutputStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::NotificationStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::DeviationStmt(stmt) => write!(f, "{:?}", stmt),
            StmtType::DeviateStmt(stmt) => write!(f, "{:?}", stmt),
        }
    }
}

// String helper functions for Basic rules.
pub fn is_node_identifier(s: &str) -> bool {
    let parts: Vec<_> = s.split(":").collect();

    if parts.len() == 1 {
        is_identifier(parts[0])
    } else if parts.len() == 2 {
        is_identifier(parts[0]) && is_identifier(parts[1]) 
    } else {
        false
    }
}

pub fn is_current_function_invocation(s: &str) -> bool {
    let s = s.trim();

    if !s.starts_with("current") {
        false
    } else {
        let s = &s[7..].trim_start();

        if !s.starts_with("(") {
            false
        } else {
            let s = &s[1..].trim();

            if s != &")" {
                false
            } else {
                true
            }
        }
    }
}

pub fn is_identifier(s: &str) -> bool {
    if !s.starts_with(|c: char| c.is_alphabetic() || c == '_') {
        false
    } else if s.len() > 1 {
        if let Some(_) = &s[1..].find(|c: char| !c.is_alphabetic() && !c.is_ascii_digit() && c != '_' && c != '-' && c != '.') {
            false
        } else {
            true
        }
    } else {
        true
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_current_function_invocation() {
        let s = "current()";
        assert_eq!(is_current_function_invocation(&s), true);

        let s = "  current () ";
        assert_eq!(is_current_function_invocation(&s), true);

        let s = "current ( )";
        assert_eq!(is_current_function_invocation(&s), true);

        let s = "current (   ) ";
        assert_eq!(is_current_function_invocation(&s), true);

        let s = "current (   ) ";
        assert_eq!(is_current_function_invocation(&s), true);

        let s = "current ( 0 ) ";
        assert_eq!(is_current_function_invocation(&s), false);
    }

    #[test]
    pub fn test_identifier() {
        let s = "identifier";
        assert_eq!(is_identifier(&s), true);

        let s = "_0123456789-abcdefghijklmnokprstuvwxyz_";
        assert_eq!(is_identifier(&s), true);

        let s = "_ABCDEFGHIJKLMNOKPRSTUVWXYZ-0123456789_";
        assert_eq!(is_identifier(&s), true);

        let s = "1a";
        assert_eq!(is_identifier(&s), false);

        let s = "_";
        assert_eq!(is_identifier(&s), true);

        let s = "_3.14159265";
        assert_eq!(is_identifier(&s), true);

        let s = "_127.0.0.1";
        assert_eq!(is_identifier(&s), true);

        let s = "_ff02::1";
        assert_eq!(is_identifier(&s), false);
    }

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
