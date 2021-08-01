//
// YANG - YANG struct
//  Copyright (C) 2021 Toshiaki Takada
//

use std::fmt;
use std::collections::HashMap;

use url::Url;

use super::error::*;
use super::parser::*;
use super::arg::*;

#[macro_use]
use crate::collect_a_stmt;
use crate::collect_vec_stmt;
use crate::collect_opt_stmt;

#[derive(Clone, Debug)]
pub struct Repeat {
    min: usize,
    max: usize,
}

impl Repeat {
    pub fn new(min: Option<usize>, max: Option<usize>) -> Repeat {
        let lower = match min {
            Some(min) => min,
            None => 0,
        };
        let upper = match max {
            Some(max) => max,
            None => usize::MAX,
        };

        Repeat {
            min: lower,
            max: upper,
        }
    }

    pub fn validate(&self, n: usize) -> bool {
        if self.min <= n && n <= self.max {
            true
        } else {
            false
        }
    }
}

// Collection of statements in HashMap.
type StmtCollection = HashMap<String, Vec<StmtType>>;

// Statement Parser callback type.
type StmtParserFn = fn(&mut Parser) -> Result<StmtType, YangError>;

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
/*
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
        m.insert("presense", PresenseStmt::parse as StmtParserFn);
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
/*
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
    PresenseStmt(PresenseStmt),
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
/*
            StmtType::RevisionDateStmt(stmt) => write!(f, "revision-date-stmt {:?}", stmt),
            StmtType::ExtensionStmt(stmt) => write!(f, "extension-stmt {:?}", stmt),
            StmtType::ArgumentStmt(stmt) => write!(f, "argument-stmt {:?}", stmt),
            StmtType::YinElementStmt(stmt) => write!(f, "yin-element-stmt {:?}", stmt),
            StmtType::IdentityStmt(stmt) => write!(f, "identity-stmt {:?}", stmt),
            StmtType::BaseStmt(stmt) => write!(f, "base-stmt {:?}", stmt),
            StmtType::FeatureStmt(stmt) => write!(f, "feature-stmt {:?}", stmt),
            StmtType::IfFeatureStmt(stmt) => write!(f, "if-feature-stmt {:?}", stmt),
            StmtType::TypedefStmt(stmt) => write!(f, "typedef-stmt {:?}", stmt),
            StmtType::TypeStmt(stmt) => write!(f, "type-stmt {:?}", stmt),
            StmtType::RangeStmt(stmt) => write!(f, "range-stmt {:?}", stmt),
            StmtType::FractionDigitsStmt(stmt) => write!(f, "fraction-digits-stmt {:?}", stmt),
            StmtType::LengthStmt(stmt) => write!(f, "length-stmt {:?}", stmt),
            StmtType::PatternStmt(stmt) => write!(f, "pattern-stmt {:?}", stmt),
            StmtType::ModifierStmt(stmt) => write!(f, "modifier-stmt {:?}", stmt),
            StmtType::DefaultStmt(stmt) => write!(f, "default-stmt {:?}", stmt),
            StmtType::EnumStmt(stmt) => write!(f, "enum-stmt {:?}", stmt),
            StmtType::PathStmt(stmt) => write!(f, "path-stmt {:?}", stmt),
            StmtType::RequireInstanceStmt(stmt) => write!(f, "require-instance-stmt {:?}", stmt),
            StmtType::BitStmt(stmt) => write!(f, "bit-stmt {:?}", stmt),
            StmtType::PositionStmt(stmt) => write!(f, "position-stmt {:?}", stmt),
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

//
// Trait for a single YANG statement.
//
pub trait Stmt {
    /// Arg type.
    type Arg;

    /// Sub Statements.
    type SubStmts;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str;

    /// Return true if this statement has sub-statements.
    fn has_substmts() -> bool {
        false
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        false
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_arg: Self::Arg, _substmts: Self::SubStmts) -> StmtType where Self: Sized {
        panic!();
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(_arg: Self::Arg) -> StmtType where Self: Sized {
        panic!();
    }

    /// Parse a statement arg.
    fn parse_arg(parser: &mut Parser) -> Result<Self::Arg, YangError> where Self::Arg: StmtArg {
        Self::Arg::parse_arg(parser)
    }

    /// Parse substatements.
    fn parse_substmts(_parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        panic!();
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError>  where Self::Arg: StmtArg, Self: Sized {
        let arg = Self::Arg::parse_arg(parser)?;

        if Self::has_substmts() {
            if let Token::BlockBegin = parser.get_token()? {
                let substmts = Self::parse_substmts(parser)?;

                if let Token::BlockEnd = parser.get_token()? {
                    Ok(Self::new_with_substmts(arg, substmts))
                } else {
                    Err(YangError::UnexpectedToken(parser.line()))
                }
            } else {
                Err(YangError::UnexpectedToken(parser.line()))
            }
        } else if Self::opt_substmts() {
            match parser.get_token()? {
                Token::StatementEnd => {
                    Ok(Self::new_with_arg(arg))
                }
                Token::BlockBegin => {
                    let substmts = Self::parse_substmts(parser)?;

                    if let Token::BlockEnd = parser.get_token()? {
                        Ok(Self::new_with_substmts(arg, substmts))
                    } else {
                        Err(YangError::UnexpectedToken(parser.line()))
                    }
                }
                _ => {
                    Err(YangError::UnexpectedToken(parser.line()))
                }
            }
        } else {
            if let Token::StatementEnd = parser.get_token()? {
                Ok(Self::new_with_arg(arg))
            } else {
                Err(YangError::UnexpectedToken(parser.line()))
            }
        }
    }
}

///
/// 7.1. The "module" Statement.
///
#[derive(Debug, Clone)]
pub struct ModuleStmt {
    /// Module identifier.
    identifier_arg: Identifier,

    /// Module header statements.
    module_header: ModuleHeaderStmts,

    /// Linkage statements.
    linkage: LinkageStmts,

    /// Meta statements.
    meta: MetaStmts,

    /// Revision statements.
    revision: RevisionStmts,
//    body: BodyStmts,
}

impl Stmt for ModuleStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (ModuleHeaderStmts, LinkageStmts, MetaStmts, RevisionStmts);

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "module"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let identifier_arg = ModuleStmt::parse_arg(parser)?;

        if let Token::BlockBegin = parser.get_token()? {
            let module_header = ModuleHeaderStmts::parse(parser)?;
            let linkage = LinkageStmts::parse(parser)?;
            let meta = MetaStmts::parse(parser)?;
            let revision = RevisionStmts::parse(parser)?;
            // let body = BodyStmts::parser(parser)?;

            if let Token::BlockEnd = parser.get_token()? {
                Ok(StmtType::ModuleStmt(ModuleStmt {
                    identifier_arg,
                    module_header,
                    linkage,
                    meta,
                    revision,
                    // body,
                } ))
            } else {
                Err(YangError::UnexpectedToken(parser.line()))
            }
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

///
/// 7.2. The "submodule" Statement.
///
#[derive(Debug, Clone)]
pub struct SubmoduleStmt {
    /// Submodule identifier.
    identifier_arg: Identifier,

    /// Submodule header statements.
    submodule_header: SubmoduleHeaderStmts,

    /// Linkage statements.
    linkage: LinkageStmts,

    /// Meta statements.
    meta: MetaStmts,

    /// Revision statements.
    revision: RevisionStmts,
//    body: BodyStmts,
}

impl Stmt for SubmoduleStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (SubmoduleHeaderStmts, LinkageStmts, MetaStmts, RevisionStmts);

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "submodule"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let identifier_arg = SubmoduleStmt::parse_arg(parser)?;

        if let Token::BlockBegin = parser.get_token()? {
            let submodule_header = SubmoduleHeaderStmts::parse(parser)?;
            let linkage = LinkageStmts::parse(parser)?;
            let meta = MetaStmts::parse(parser)?;
            let revision = RevisionStmts::parse(parser)?;
            // let body = BodyStmts::parse(parser)?;

            if let Token::BlockEnd = parser.get_token()? {
                Ok(StmtType::SubmoduleStmt(SubmoduleStmt {
                    identifier_arg,
                    submodule_header,
                    linkage,
                    meta,
                    revision,
                } ))
            } else {
                Err(YangError::UnexpectedToken(parser.line()))
            }
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

///
/// 7.1.2. The "yang-version" Statement.
///
#[derive(Debug, Clone)]
pub struct YangVersionStmt {
    yang_version_arg: YangVersionArg,
}

impl Stmt for YangVersionStmt {
    /// Arg type.
    type Arg = YangVersionArg;

    /// Sub Statements.
    type SubStmts = Self::Arg;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "yang-version"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::YangVersionStmt(YangVersionStmt {
            yang_version_arg: arg
        })
    }
}

///
/// 7.1.5. The "import" Statement.
///
#[derive(Debug, Clone)]
pub struct ImportStmt {
    identifier_arg: Identifier,
    prefix: PrefixStmt,
//    revision_date: Option<RevisionDateStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for ImportStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (PrefixStmt, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "import"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ImportStmt(ImportStmt {
            identifier_arg: arg,
            prefix: substmts.0,
            description: substmts.1,
            reference: substmts.2,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("prefix", Repeat::new(Some(1), Some(1))),
//            ("revision-date", Repeat::new(Some(1), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok((collect_a_stmt!(stmts, PrefixStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// 7.1.6. The "include" Statement.
///
#[derive(Debug, Clone)]
pub struct IncludeStmt {
    identifier_arg: Identifier,
//    revision_date: Option<RevisionDateStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for IncludeStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "include"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::IncludeStmt(IncludeStmt {
            identifier_arg: arg,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::IncludeStmt(IncludeStmt {
            identifier_arg: arg,
            description: substmts.0,
            reference: substmts.1,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
//            ("revision-date", Repeat::new(Some(1), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok((collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// 7.1.3. The "namespace" Statement.
///
#[derive(Debug, Clone)]
pub struct NamespaceStmt {
    uri_str: Url,
}

impl Stmt for NamespaceStmt {
    /// Arg type.
    type Arg = Url;

    /// Sub Statements.
    type SubStmts = Self::Arg;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "namespace"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::NamespaceStmt(NamespaceStmt { uri_str: arg })
    }
}

///
/// 7.1.4. The "prefix" Statement.
///
#[derive(Debug, Clone)]
pub struct PrefixStmt {
    prefix_arg_str: Identifier,
}

impl Stmt for PrefixStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = Self::Arg;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "prefix"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::PrefixStmt(PrefixStmt { prefix_arg_str: arg })
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct BelongsToStmt {
    /// 
    identifier_arg: Identifier,

    /// Prefix statement.
    prefix: PrefixStmt,
}

impl Stmt for BelongsToStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (PrefixStmt,);

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "belongs-to"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::BelongsToStmt(BelongsToStmt {
            identifier_arg: arg,
            prefix: substmts.0,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("prefix", Repeat::new(Some(1), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok((collect_a_stmt!(stmts, PrefixStmt)?,))
    }
}

///
/// 7.1.7. The "Organization" Statement.
///
#[derive(Debug, Clone)]
pub struct OrganizationStmt {
    string: String,
}

impl Stmt for OrganizationStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = Self::Arg;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "organization"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::OrganizationStmt(OrganizationStmt { string: arg })
    }
}

///
/// 7.1.8. The "contact" Statement.
///
#[derive(Debug, Clone)]
pub struct ContactStmt {
    string: String,
}

impl Stmt for ContactStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = Self::Arg;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "contact"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ContactStmt(ContactStmt { string: arg })
    }
}

///
/// 7.21.3. "The "description" Statement.
/// 
#[derive(Debug, Clone)]
pub struct DescriptionStmt {
    string: String,
}

impl Stmt for DescriptionStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = Self::Arg;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "description"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::DescriptionStmt(DescriptionStmt { string: arg })
    }
}

///
/// 7.21.4. "The "reference" Statement.
/// 
#[derive(Debug, Clone)]
pub struct ReferenceStmt {
    string: String,
}

impl Stmt for ReferenceStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = Self::Arg;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "reference"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ReferenceStmt(ReferenceStmt { string: arg })
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct UnitsStmt {
    string: String,
}

impl Stmt for UnitsStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = Self::Arg;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "units"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::UnitsStmt(UnitsStmt { string: arg })
    }
}

///
/// 7.1.9. The "revision" Statement.
///
#[derive(Debug, Clone)]
pub struct RevisionStmt {
    /// Revision date.
    revision_date: DateArg,

    /// Description.
    description: Option<DescriptionStmt>,

    /// Reference.
    reference: Option<ReferenceStmt>,
}

impl Stmt for RevisionStmt {
    /// Arg type.
    type Arg = DateArg;

    /// Sub Statements.
    type SubStmts = (Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "revision"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::RevisionStmt(RevisionStmt {
            revision_date: arg,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::RevisionStmt(RevisionStmt {
            revision_date: arg,
            description: substmts.0,
            reference: substmts.1,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;
        
        Ok((collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }

/*
    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let revision_date = RevisionStmt::parse_arg(parser)?;

        if let Token::BlockBegin = parser.get_token()? {
            let map: HashMap<&'static str, Repeat> = [
                ("description", Repeat::new(Some(0), Some(1))),
                ("reference", Repeat::new(Some(0), Some(1))),
            ].iter().cloned().collect();

            let mut stmts = parse_stmt_collection(parser, map)?;

            let token = parser.get_token()?;
            if let Token::BlockEnd = token {
                let stmt = RevisionStmt {
                    revision_date,
                    description: collect_opt_stmt!(stmts, DescriptionStmt)?,
                    reference: collect_opt_stmt!(stmts, ReferenceStmt)?,
                };

                Ok(StmtType::RevisionStmt(stmt))
            } else {
                Err(YangError::UnexpectedToken(parser.line()))
            }
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
*/
}

/*  XXXX TBD

///
///
///
#[derive(Debug, Clone)]
pub struct RevisionDateStmt {
}

impl Stmt for RevisionDateStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "revision-date"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ExtensionStmt {
}

impl Stmt for ExtensionStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "extension"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ArgumentStmt {
}

impl Stmt for ArgumentStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "argument"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct YinElementStmt {
}

impl Stmt for YinElementStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "yin-element"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct IdentityStmt {
}

impl Stmt for IdentityStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "identity"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct BaseStmt {
}

impl Stmt for BaseStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "base"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct FeatureStmt {
}

impl Stmt for FeatureStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "feature"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct IfFeatureStmt {
}

impl Stmt for IfFeatureStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "if-feature"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct TypedefStmt {
}

impl Stmt for TypedefStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "typedef"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct TypeStmt {
}

impl Stmt for TypeStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "type"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct RangeStmt {
}

impl Stmt for RangeStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "range"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct FractionDigitsStmt {
}

impl Stmt for FractionDigitsStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "fraction-digits"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct LengthStmt {
}

impl Stmt for LengthStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "length"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct PatternStmt {
}

impl Stmt for PatternStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "pattern"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ModifierStmt {
}

impl Stmt for ModifierStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "modifier"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct DefaultStmt {
}

impl Stmt for DefaultStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "default"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct EnumStmt {
}

impl Stmt for EnumStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "enum"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct PathStmt {
}

impl Stmt for PathStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "path"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct RequireInstanceStmt {
}

impl Stmt for RequireInstanceStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "require-instance"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct BitStmt {
}

impl Stmt for BitStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "bit"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct PositionStmt {
}

impl Stmt for PositionStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "position"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct StatusStmt {
}

impl Stmt for StatusStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "status"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ConfigStmt {
}

impl Stmt for ConfigStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "config"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct MandatoryStmt {
}

impl Stmt for MandatoryStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "mandatory"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct PresenseStmt {
}

impl Stmt for PresenseStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "presense"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct OrderedByStmt {
}

impl Stmt for OrderedByStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "ordered-by"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct MustStmt {
}

impl Stmt for MustStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "must"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ErrorMessageStmt {
}

impl Stmt for ErrorMessageStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "error-message"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ErrorAppTagStmt {
}

impl Stmt for ErrorAppTagStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "error-app-tag"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct MinElementsStmt {
}

impl Stmt for MinElementsStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "min-elements"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct MaxElementsStmt {
}

impl Stmt for MaxElementsStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "max-elements"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ValueStmt {
}

impl Stmt for ValueStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "value"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct GroupingStmt {
}

impl Stmt for GroupingStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "grouping"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ContainerStmt {
}

impl Stmt for ContainerStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "container"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct LeafStmt {
}

impl Stmt for LeafStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "leaf"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct LeafListStmt {
}

impl Stmt for LeafListStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "leaf-list"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ListStmt {
}

impl Stmt for ListStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "list"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct KeyStmt {
}

impl Stmt for KeyStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "key"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct UniqueStmt {
}

impl Stmt for UniqueStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "unique"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ChoiceStmt {
}

impl Stmt for ChoiceStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "choice"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ShortCaseStmt {
}

impl Stmt for ShortCaseStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "short-case"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct CaseStmt {
}

impl Stmt for CaseStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "case"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct AnydataStmt {
}

impl Stmt for AnydataStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "anydata"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct AnyxmlStmt {
}

impl Stmt for AnyxmlStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "anyxml"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct UsesStmt {
}

impl Stmt for UsesStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "uses"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct RefineStmt {
}

impl Stmt for RefineStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "refine"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct UsesAugmentStmt {
}

impl Stmt for UsesAugmentStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "uses-augment"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct AugmentStmt {
}

impl Stmt for AugmentStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "augment"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct WhenStmt {
}

impl Stmt for WhenStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "when"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct RpcStmt {
}

impl Stmt for RpcStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "rpc"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ActionStmt {
}

impl Stmt for ActionStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "action"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct InputStmt {
}

impl Stmt for InputStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "input"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct OutputStmt {
}

impl Stmt for OutputStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "output"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct NotificationStmt {
}

impl Stmt for NotificationStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "notification"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct DeviationStmt {
}

impl Stmt for DeviationStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "deviation"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct DeviationNotSupportedStmt {
}

impl Stmt for DeviationNotSupportedStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "deviation-not-supported"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct DeviateAddStmt {
}

impl Stmt for DeviateAddStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "deviate-add"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct DeviateDeleteStmt {
}

impl Stmt for DeviateDeleteStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "deviate-delete"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct DeviateReplaceStmt {
}

impl Stmt for DeviateReplaceStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "deviate-replace"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

TBD */


///
/// Module Header Statements.
///
#[derive(Debug, Clone)]
pub struct ModuleHeaderStmts {
    yang_version: YangVersionStmt,
    namespace: NamespaceStmt,
    prefix: PrefixStmt,
}

impl ModuleHeaderStmts {
    pub fn parse(parser: &mut Parser) -> Result<ModuleHeaderStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("yang-version", Repeat::new(Some(1), Some(1))),
            ("namespace", Repeat::new(Some(1), Some(1))),
            ("prefix", Repeat::new(Some(1), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok(ModuleHeaderStmts {
            yang_version: collect_a_stmt!(stmts, YangVersionStmt)?,
            namespace: collect_a_stmt!(stmts, NamespaceStmt)?,
            prefix: collect_a_stmt!(stmts, PrefixStmt)?,
        })
    }
}


///
/// Submodule Header Statements.
///
#[derive(Debug, Clone)]
pub struct SubmoduleHeaderStmts {
    yang_version: YangVersionStmt,
    belongs_to: BelongsToStmt,
}

impl SubmoduleHeaderStmts {
    pub fn parse(parser: &mut Parser) -> Result<SubmoduleHeaderStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("yang-version", Repeat::new(Some(1), Some(1))),
            ("belongs-to", Repeat::new(Some(1), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok(SubmoduleHeaderStmts {
            yang_version: collect_a_stmt!(stmts, YangVersionStmt)?,
            belongs_to: collect_a_stmt!(stmts, BelongsToStmt)?,
        })
    }
}

///
/// Meta Statements.
///
#[derive(Debug, Clone)]
pub struct MetaStmts {
    organization: Option<OrganizationStmt>,
    contact: Option<ContactStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl MetaStmts {
    pub fn parse(parser: &mut Parser) -> Result<MetaStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("organization", Repeat::new(Some(0), None)),
            ("contact", Repeat::new(Some(0), None)),
            ("description", Repeat::new(Some(0), None)),
            ("reference", Repeat::new(Some(0), None)),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok(MetaStmts {
            organization: collect_opt_stmt!(stmts, OrganizationStmt)?,
            contact: collect_opt_stmt!(stmts, ContactStmt)?,
            description: collect_opt_stmt!(stmts, DescriptionStmt)?,
            reference: collect_opt_stmt!(stmts, ReferenceStmt)?,
        })
  }
}

///
/// Linkage Statements.
///
#[derive(Debug, Clone)]
pub struct LinkageStmts {
    import: Vec<ImportStmt>,
    include: Vec<IncludeStmt>,
}

impl LinkageStmts {
    pub fn parse(parser: &mut Parser) -> Result<LinkageStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("import", Repeat::new(Some(0), None)),
            ("include", Repeat::new(Some(0), None)),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok(LinkageStmts {
            import: collect_vec_stmt!(stmts, ImportStmt)?,
            include: collect_vec_stmt!(stmts, IncludeStmt)?,
        })
    }
}

///
/// Revision Statements.
///
#[derive(Debug, Clone)]
pub struct RevisionStmts {
    revision: Vec<RevisionStmt>
}

impl RevisionStmts {
    pub fn parse(parser: &mut Parser) -> Result<RevisionStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("revision", Repeat::new(Some(0), None)),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok(RevisionStmts {
            revision: collect_vec_stmt!(stmts, RevisionStmt)?,
        })
    }
}

//
// TBD: body-stmts
//

//
// data-def-stmt
//




/*
#[derive(Debug, Clone)]
pub struct Stmt {
}

impl Stmt for Stmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        ""
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}
*/

