//
// YANG - YANG compound statement
//  Copyright (C) 2021 Toshiaki Takada
//

use derive_getters::Getters;

use super::core::*;
use super::error::*;
use super::parser::*;
use super::stmt::*;
use super::substmt::*;

use crate::collect_a_stmt;
use crate::collect_nonempty_vec_stmt;
use crate::collect_opt_stmt;
use crate::collect_vec_stmt;

///
/// Trait for compound YANG statements.
///
pub trait Compound {
    /// Return list fo statement keyword.
    fn keywords() -> Vec<Keyword> {
        panic!("undefined");
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef>;
}

///
/// "module-header" statements.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ModuleHeaderStmts {
    /// "yang-version" statement.
    yang_version: Option<YangVersionStmt>,

    /// "namespace" statement.
    namespace: NamespaceStmt,

    /// "prefix" statement.
    prefix: PrefixStmt,
}

impl Compound for ModuleHeaderStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(YangVersionStmt::keyword)),
            SubStmtDef::HasOne(SubStmtWith::Stmt(NamespaceStmt::keyword)),
            SubStmtDef::HasOne(SubStmtWith::Stmt(PrefixStmt::keyword)),
        ]
    }
}

impl ModuleHeaderStmts {
    /// Parse sub statements.
    pub fn parse(parser: &mut Parser) -> Result<ModuleHeaderStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(ModuleHeaderStmts {
            yang_version: collect_opt_stmt!(stmts, YangVersionStmt)?,
            namespace: collect_a_stmt!(stmts, NamespaceStmt)?,
            prefix: collect_a_stmt!(stmts, PrefixStmt)?,
        })
    }
}

///
/// "submodule-header" statements.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct SubmoduleHeaderStmts {
    /// "yang-version" statement.
    yang_version: Option<YangVersionStmt>,

    /// "belongs-to" statement.
    belongs_to: BelongsToStmt,
}

impl Compound for SubmoduleHeaderStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(YangVersionStmt::keyword)),
            SubStmtDef::HasOne(SubStmtWith::Stmt(BelongsToStmt::keyword)),
        ]
    }
}

impl SubmoduleHeaderStmts {
    /// Parse sub statements.
    pub fn parse(parser: &mut Parser) -> Result<SubmoduleHeaderStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(SubmoduleHeaderStmts {
            yang_version: collect_opt_stmt!(stmts, YangVersionStmt)?,
            belongs_to: collect_a_stmt!(stmts, BelongsToStmt)?,
        })
    }
}

///
/// "body" Statements.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct BodyStmts {
    /// "extension" statment.
    extension: Vec<ExtensionStmt>,

    /// "feature" statement.
    feature: Vec<FeatureStmt>,

    /// "identity" statement.
    identity: Vec<IdentityStmt>,

    /// "typedef" statement.
    typedef: Vec<TypedefStmt>,

    /// "grouping" statement.
    grouping: Vec<GroupingStmt>,

    /// "data-def" statement.
    data_def: DataDefStmt,

    /// "augment" statement.
    augment: Vec<AugmentStmt>,

    /// "rpc" statement.
    rpc: Vec<RpcStmt>,

    /// "notification" statement.
    notification: Vec<NotificationStmt>,

    /// "deviation" statement.
    deviation: Vec<DeviationStmt>,
}

impl Compound for BodyStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(ExtensionStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(FeatureStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IdentityStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(TypedefStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(GroupingStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(AugmentStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(RpcStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(NotificationStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(DeviationStmt::keyword)),
        ]
    }
}

impl BodyStmts {
    pub fn parse(parser: &mut Parser) -> Result<BodyStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(BodyStmts {
            extension: collect_vec_stmt!(stmts, ExtensionStmt)?,
            feature: collect_vec_stmt!(stmts, FeatureStmt)?,
            identity: collect_vec_stmt!(stmts, IdentityStmt)?,
            typedef: collect_vec_stmt!(stmts, TypedefStmt)?,
            grouping: collect_vec_stmt!(stmts, GroupingStmt)?,
            data_def: DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,
            )),
            augment: collect_vec_stmt!(stmts, AugmentStmt)?,
            rpc: collect_vec_stmt!(stmts, RpcStmt)?,
            notification: collect_vec_stmt!(stmts, NotificationStmt)?,
            deviation: collect_vec_stmt!(stmts, DeviationStmt)?,
        })
    }
}

///
/// "meta" statements.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct MetaStmts {
    /// "organization" statement.
    organization: Option<OrganizationStmt>,

    /// "contact" statement.
    contact: Option<ContactStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "refrence statement.
    reference: Option<ReferenceStmt>,
}

impl Compound for MetaStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(OrganizationStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ContactStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }
}

impl MetaStmts {
    pub fn parse(parser: &mut Parser) -> Result<MetaStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(MetaStmts {
            organization: collect_opt_stmt!(stmts, OrganizationStmt)?,
            contact: collect_opt_stmt!(stmts, ContactStmt)?,
            description: collect_opt_stmt!(stmts, DescriptionStmt)?,
            reference: collect_opt_stmt!(stmts, ReferenceStmt)?,
        })
    }
}

///
/// "linkage" statements.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct LinkageStmts {
    /// "import" statement.
    import: Vec<ImportStmt>,

    /// "include" statement.
    include: Vec<IncludeStmt>,
}

impl Compound for LinkageStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(ImportStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IncludeStmt::keyword)),
        ]
    }
}

impl LinkageStmts {
    pub fn parse(parser: &mut Parser) -> Result<LinkageStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(LinkageStmts {
            import: collect_vec_stmt!(stmts, ImportStmt)?,
            include: collect_vec_stmt!(stmts, IncludeStmt)?,
        })
    }
}

///
/// "revision" statements.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct RevisionStmts {
    /// "revision" statement.
    revision: Vec<RevisionStmt>,
}

impl Compound for RevisionStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(
            RevisionStmt::keyword,
        ))]
    }
}

impl RevisionStmts {
    pub fn parse(parser: &mut Parser) -> Result<RevisionStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(RevisionStmts {
            revision: collect_vec_stmt!(stmts, RevisionStmt)?,
        })
    }
}

///
/// "numerical-restrictions".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct NumericalRestrictions {
    /// "reange" statement.
    range: Option<RangeStmt>,
}

impl Compound for NumericalRestrictions {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(RangeStmt::keyword))]
    }
}

impl NumericalRestrictions {
    pub fn parse(parser: &mut Parser) -> Result<NumericalRestrictions, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(NumericalRestrictions {
            range: collect_opt_stmt!(stmts, RangeStmt)?,
        })
    }
}

///
/// "decimal64-specification".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct Decimal64Specification {
    /// "fraction-digits" statement.
    fraction_digits: FractionDigitsStmt,

    /// "range" statement.
    range: Option<RangeStmt>,
}

impl Compound for Decimal64Specification {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::HasOne(SubStmtWith::Stmt(FractionDigitsStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(RangeStmt::keyword)),
        ]
    }
}

impl Decimal64Specification {
    pub fn parse(parser: &mut Parser) -> Result<Decimal64Specification, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(Decimal64Specification {
            fraction_digits: collect_a_stmt!(stmts, FractionDigitsStmt)?,
            range: collect_opt_stmt!(stmts, RangeStmt)?,
        })
    }
}

///
/// "string-restrictions".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct StringRestrictions {
    /// "length" statement.
    length: Option<LengthStmt>,

    /// "pattern" statement.
    pattern: Vec<PatternStmt>,
}

///
/// "enum-specification".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct EnumSpecification {
    /// "enum" statement.
    enum_: Vec<EnumStmt>,
}

///
/// "leafref-specification".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct LeafrefSpecification {
    /// "path" statement.
    path: PathStmt,

    /// "require-instance" statement.
    require_instance: Option<RequireInstanceStmt>,
}

///
/// "identityref-specification".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct IdentityrefSpecification {
    /// "base" statement.
    base: Vec<BaseStmt>,
}

///
/// "instance-identifier-specification".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct InstanceIdentifierSpecification {
    /// "require-instance" statement.
    require_instance: Option<RequireInstanceStmt>,
}

///
/// "bits-specification".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct BitsSpecification {
    /// "bit" statement.
    bit: Vec<BitStmt>,
}

///
/// "union-specification".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct UnionSpecification {
    /// "type" statement.
    type_: Vec<TypeStmt>,
}

///
/// "binary-specification".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct BinarySpecification {
    /// "length" statement.
    length: Option<LengthStmt>,
}

///
/// "type-body" Statements.
///
#[derive(Debug, Clone, PartialEq)]
pub enum TypeBodyStmts {
    NumericalRestrictions(NumericalRestrictions),
    Decimal64Specification(Decimal64Specification),
    StringRestrictions(StringRestrictions),
    EnumSpecification(EnumSpecification),
    LeafrefSpecification(LeafrefSpecification),
    IdentityrefSpecification(IdentityrefSpecification),
    InstanceIdentifierSpecification(InstanceIdentifierSpecification),
    BitsSpecification(BitsSpecification),
    UnionSpecification(UnionSpecification),
    BinarySpecification(BinarySpecification),
}

impl TypeBodyStmts {
    /// Return substatements definition.
    pub fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(FractionDigitsStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(RangeStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(PatternStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(LengthStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(PathStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(RequireInstanceStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(EnumStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(BaseStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(BitStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(TypeStmt::keyword)),
        ]
        // TBD: This check is very loose at this moment.
    }

    // decimal64 specification		fraction-digits-stmt [range-stmt]	(any order)
    // numerical restrictions		[range-stmt]
    // string restrictions			[length-stmt] *pattern-stmt		(any order)
    // binary-specification			[length-stmt]
    // leafref-specification		path-stmt [require-instance-stmt]	(any order)
    // instance-identifier-specpfication	[require-instance-stmt]
    // enum specification			1*enum-stmt
    // identityref-specification		1*base-stmt
    // bits-specification			1*bit-stmt
    // union-specification			1*type-stmt
    pub fn parse(parser: &mut Parser) -> Result<Option<TypeBodyStmts>, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        let type_body = if let Ok(fraction_digits) = collect_a_stmt!(stmts, FractionDigitsStmt) {
            let range = if let Ok(range) = collect_a_stmt!(stmts, RangeStmt) {
                Some(range)
            } else {
                None
            };

            TypeBodyStmts::Decimal64Specification(Decimal64Specification {
                fraction_digits,
                range,
            })
        } else if let Ok(range) = collect_a_stmt!(stmts, RangeStmt) {
            TypeBodyStmts::NumericalRestrictions(NumericalRestrictions { range: Some(range) })
        } else if let Ok(pattern) = collect_nonempty_vec_stmt!(stmts, PatternStmt) {
            // TBD: need check pattern.len()
            let length = if let Ok(length) = collect_a_stmt!(stmts, LengthStmt) {
                Some(length)
            } else {
                None
            };

            TypeBodyStmts::StringRestrictions(StringRestrictions { pattern, length })
        } else if let Ok(length) = collect_a_stmt!(stmts, LengthStmt) {
            TypeBodyStmts::BinarySpecification(BinarySpecification {
                length: Some(length),
            })
        } else if let Ok(path) = collect_a_stmt!(stmts, PathStmt) {
            let require_instance =
                if let Ok(require_instance) = collect_a_stmt!(stmts, RequireInstanceStmt) {
                    Some(require_instance)
                } else {
                    None
                };

            TypeBodyStmts::LeafrefSpecification(LeafrefSpecification {
                path,
                require_instance,
            })
        } else if let Ok(require_instance) = collect_a_stmt!(stmts, RequireInstanceStmt) {
            TypeBodyStmts::InstanceIdentifierSpecification(InstanceIdentifierSpecification {
                require_instance: Some(require_instance),
            })
        } else if let Ok(enum_) = collect_nonempty_vec_stmt!(stmts, EnumStmt) {
            TypeBodyStmts::EnumSpecification(EnumSpecification { enum_ })
        } else if let Ok(base) = collect_nonempty_vec_stmt!(stmts, BaseStmt) {
            TypeBodyStmts::IdentityrefSpecification(IdentityrefSpecification { base })
        } else if let Ok(bit) = collect_nonempty_vec_stmt!(stmts, BitStmt) {
            TypeBodyStmts::BitsSpecification(BitsSpecification { bit })
        } else if let Ok(type_) = collect_nonempty_vec_stmt!(stmts, TypeStmt) {
            TypeBodyStmts::UnionSpecification(UnionSpecification { type_ })
        } else {
            return Ok(None);
        };

        Ok(Some(type_body))
    }
}

//
// Trait for selection of YANG statements.
//
pub trait Selection {
    /// Sub Statements.
    type SubStmts;

    /// Return list fo statement keyword.
    fn keywords() -> Vec<Keyword> {
        panic!("undefined");
    }

    /// Constructor with empty substatements.
    fn new() -> Self
    where
        Self: Sized,
    {
        panic!("{:?}", Self::keywords());
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_substmts: Self::SubStmts) -> Self
    where
        Self: Sized,
    {
        panic!("{:?}", Self::keywords());
    }
}

///
/// "typedef" stmt / "grouping" stmt
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct TypedefOrGrouping {
    /// "typedef" statement.
    typedef: Vec<TypedefStmt>,

    /// "grouping" statement.
    grouping: Vec<GroupingStmt>,
}

impl Selection for TypedefOrGrouping {
    /// Sub Statements.
    type SubStmts = (Vec<TypedefStmt>, Vec<GroupingStmt>);

    /// Return list fo statement keyword.
    fn keywords() -> Vec<Keyword> {
        vec![TypedefStmt::keyword(), GroupingStmt::keyword()]
    }

    /// Constructor with empty substatements.
    fn new() -> Self
    where
        Self: Sized,
    {
        Self {
            typedef: Vec::new(),
            grouping: Vec::new(),
        }
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(substmts: Self::SubStmts) -> Self
    where
        Self: Sized,
    {
        Self {
            typedef: substmts.0,
            grouping: substmts.1,
        }
    }
}

///
/// "data-def-stmt".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct DataDefStmt {
    /// "container" statement.
    container: Vec<ContainerStmt>,

    /// "leaf" statement.
    leaf: Vec<LeafStmt>,

    /// "leaf-list" statement.
    leaf_list: Vec<LeafListStmt>,

    /// "list" statement.
    list: Vec<ListStmt>,

    /// "choice" statement.
    choice: Vec<ChoiceStmt>,

    /// "anydata" statement.
    anydata: Vec<AnydataStmt>,

    /// "anyxml" statement.
    anyxml: Vec<AnyxmlStmt>,

    /// "uses" statement.
    uses: Vec<UsesStmt>,
}

impl Selection for DataDefStmt {
    /// Sub Statements.
    type SubStmts = (
        Vec<ContainerStmt>,
        Vec<LeafStmt>,
        Vec<LeafListStmt>,
        Vec<ListStmt>,
        Vec<ChoiceStmt>,
        Vec<AnydataStmt>,
        Vec<AnyxmlStmt>,
        Vec<UsesStmt>,
    );

    /// Return list fo statement keyword.
    fn keywords() -> Vec<Keyword> {
        vec![
            ContainerStmt::keyword(),
            LeafStmt::keyword(),
            LeafListStmt::keyword(),
            ListStmt::keyword(),
            ChoiceStmt::keyword(),
            AnydataStmt::keyword(),
            AnyxmlStmt::keyword(),
            UsesStmt::keyword(),
        ]
    }

    /// Constructor with empty substatements.
    fn new() -> Self
    where
        Self: Sized,
    {
        Self {
            container: Vec::new(),
            leaf: Vec::new(),
            leaf_list: Vec::new(),
            list: Vec::new(),
            choice: Vec::new(),
            anydata: Vec::new(),
            anyxml: Vec::new(),
            uses: Vec::new(),
        }
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(substmts: Self::SubStmts) -> Self
    where
        Self: Sized,
    {
        Self {
            container: substmts.0,
            leaf: substmts.1,
            leaf_list: substmts.2,
            list: substmts.3,
            choice: substmts.4,
            anydata: substmts.5,
            anyxml: substmts.6,
            uses: substmts.7,
        }
    }
}

///
/// "data-def-stmt" / "case-stmt" / "action-stmt" / "notification-stmt".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct DataDefOrElse {
    /// "container" statement.
    container: Vec<ContainerStmt>,

    /// "leaf" statement.
    leaf: Vec<LeafStmt>,

    /// "leaf-list" statement.
    leaf_list: Vec<LeafListStmt>,

    /// "list" statement.
    list: Vec<ListStmt>,

    /// "choice" statement.
    choice: Vec<ChoiceStmt>,

    /// "anydata" statement.
    anydata: Vec<AnydataStmt>,

    /// "anyxml" statement.
    anyxml: Vec<AnyxmlStmt>,

    /// "uses" statement.
    uses: Vec<UsesStmt>,

    /// "case" statement.
    case: Vec<CaseStmt>,

    /// "action" statement.
    action: Vec<ActionStmt>,

    /// "notification" statement.
    notification: Vec<NotificationStmt>,
}

impl Selection for DataDefOrElse {
    /// Sub Statements.
    type SubStmts = (
        Vec<ContainerStmt>,
        Vec<LeafStmt>,
        Vec<LeafListStmt>,
        Vec<ListStmt>,
        Vec<ChoiceStmt>,
        Vec<AnydataStmt>,
        Vec<AnyxmlStmt>,
        Vec<UsesStmt>,
        Vec<CaseStmt>,
        Vec<ActionStmt>,
        Vec<NotificationStmt>,
    );

    /// Return list fo statement keyword.
    fn keywords() -> Vec<Keyword> {
        vec![
            ContainerStmt::keyword(),
            LeafStmt::keyword(),
            LeafListStmt::keyword(),
            ListStmt::keyword(),
            ChoiceStmt::keyword(),
            AnydataStmt::keyword(),
            AnyxmlStmt::keyword(),
            UsesStmt::keyword(),
            CaseStmt::keyword(),
            ActionStmt::keyword(),
            NotificationStmt::keyword(),
        ]
    }

    /// Constructor with empty substatements.
    fn new() -> Self
    where
        Self: Sized,
    {
        Self {
            container: Vec::new(),
            leaf: Vec::new(),
            leaf_list: Vec::new(),
            list: Vec::new(),
            choice: Vec::new(),
            anydata: Vec::new(),
            anyxml: Vec::new(),
            uses: Vec::new(),
            case: Vec::new(),
            action: Vec::new(),
            notification: Vec::new(),
        }
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(substmts: Self::SubStmts) -> Self
    where
        Self: Sized,
    {
        Self {
            container: substmts.0,
            leaf: substmts.1,
            leaf_list: substmts.2,
            list: substmts.3,
            choice: substmts.4,
            anydata: substmts.5,
            anyxml: substmts.6,
            uses: substmts.7,
            case: substmts.8,
            action: substmts.9,
            notification: substmts.10,
        }
    }
}

///
/// "short-case-stmt" / "case-stmt".
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ShortCaseOrCaseStmt {
    /// "choice" statement.
    choice: Vec<ChoiceStmt>,

    /// "container" statement.
    container: Vec<ContainerStmt>,

    /// "leaf" statement.
    leaf: Vec<LeafStmt>,

    /// "leaf-list" statement.
    leaf_list: Vec<LeafListStmt>,

    /// "list" statement.
    list: Vec<ListStmt>,

    /// "anydata" statement.
    anydata: Vec<AnydataStmt>,

    /// "anyxml" statement.
    anyxml: Vec<AnyxmlStmt>,

    /// "case" statement.
    case: Vec<CaseStmt>,
}

impl Selection for ShortCaseOrCaseStmt {
    /// Sub Statements.
    type SubStmts = (
        Vec<ChoiceStmt>,
        Vec<ContainerStmt>,
        Vec<LeafStmt>,
        Vec<LeafListStmt>,
        Vec<ListStmt>,
        Vec<AnydataStmt>,
        Vec<AnyxmlStmt>,
        Vec<CaseStmt>,
    );

    /// Return list fo statement keyword.
    fn keywords() -> Vec<Keyword> {
        vec![
            ChoiceStmt::keyword(),
            ContainerStmt::keyword(),
            LeafStmt::keyword(),
            LeafListStmt::keyword(),
            ListStmt::keyword(),
            AnydataStmt::keyword(),
            AnyxmlStmt::keyword(),
            CaseStmt::keyword(),
        ]
    }

    /// Constructor with empty substatements.
    fn new() -> Self
    where
        Self: Sized,
    {
        Self {
            choice: Vec::new(),
            container: Vec::new(),
            leaf: Vec::new(),
            leaf_list: Vec::new(),
            list: Vec::new(),
            anydata: Vec::new(),
            anyxml: Vec::new(),
            case: Vec::new(),
        }
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(substmts: Self::SubStmts) -> Self
    where
        Self: Sized,
    {
        Self {
            choice: substmts.0,
            container: substmts.1,
            leaf: substmts.2,
            leaf_list: substmts.3,
            list: substmts.4,
            anydata: substmts.5,
            anyxml: substmts.6,
            case: substmts.7,
        }
    }
}
