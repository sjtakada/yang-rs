//
// YANG - YANG compound statement
//  Copyright (C) 2021 Toshiaki Takada
//

use super::core::*;
use super::error::*;
use super::parser::*;
use super::stmt::*;
use super::substmt::*;

use crate::collect_a_stmt;
use crate::collect_vec_stmt;
use crate::collect_opt_stmt;
//use crate::parse_a_stmt;

//
// Trait for compound YANG statements.
//
pub trait Compound {
    /// Return list fo statement keyword.
    fn keywords() -> Vec<Keyword> {
        panic!("undefined");
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef>;
}

///
/// Module Header Statements.
///
#[derive(Debug, Clone)]
pub struct ModuleHeaderStmts {
    yang_version: YangVersionStmt,
    namespace: NamespaceStmt,
    prefix: PrefixStmt,
}

impl Compound for ModuleHeaderStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::HasOne(SubStmtWith::Stmt(YangVersionStmt::keyword)),
             SubStmtDef::HasOne(SubStmtWith::Stmt(NamespaceStmt::keyword)),
             SubStmtDef::HasOne(SubStmtWith::Stmt(PrefixStmt::keyword)),
        ]
    }
}

impl ModuleHeaderStmts {
    pub fn parse(parser: &mut Parser) -> Result<ModuleHeaderStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

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

impl Compound for SubmoduleHeaderStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::HasOne(SubStmtWith::Stmt(YangVersionStmt::keyword)),
             SubStmtDef::HasOne(SubStmtWith::Stmt(BelongsToStmt::keyword)),
        ]
    }
}

impl SubmoduleHeaderStmts {
    pub fn parse(parser: &mut Parser) -> Result<SubmoduleHeaderStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

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

impl Compound for MetaStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(OrganizationStmt::keyword)),
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
/// Linkage Statements.
///
#[derive(Debug, Clone)]
pub struct LinkageStmts {
    import: Vec<ImportStmt>,
    include: Vec<IncludeStmt>,
}

impl Compound for LinkageStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(ImportStmt::keyword)),
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
/// Revision Statements.
///
#[derive(Debug, Clone)]
pub struct RevisionStmts {
    revision: Vec<RevisionStmt>
}

impl Compound for RevisionStmts {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(RevisionStmt::keyword)),
        ]
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
/// "data-def-stmt".
///
#[derive(Debug, Clone)]
pub struct DataDefStmt {
//    container: Option<ContainerStmt>,
//    leaf: Option<LeafStmt>,
//    leaf_list: Option<LeafListStmt>,
//    list: Option<ListStmt>,
//    choice: Option<ChoiceStmt>,
    anydata: Option<AnydataStmt>,
    anyxml: Option<AnyxmlStmt>,
    uses: Option<UsesStmt>,
}

impl Compound for DataDefStmt {
    /// Return list fo statement keyword.
    fn keywords() -> Vec<Keyword> {
        vec![AnydataStmt::keyword(), AnyxmlStmt::keyword(), UsesStmt::keyword()]
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        Vec::new()
    }
}


///
/// "numerical-restrictions".
///
#[derive(Debug, Clone)]
pub struct NumericalRestrictions {
    range: Option<RangeStmt>,
}

impl Compound for NumericalRestrictions {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(RangeStmt::keyword)),
        ]
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
#[derive(Debug, Clone)]
pub struct Decimal64Specification {
    fraction_digits: FractionDigitsStmt,
    range: Option<RangeStmt>,
}

impl Compound for Decimal64Specification {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::HasOne(SubStmtWith::Stmt(FractionDigitsStmt::keyword)),
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
#[derive(Debug, Clone)]
pub struct StringRestrictions {
    length: Option<LengthStmt>,
    pattern: Vec<PatternStmt>,
}

///
/// "enum-specification".
///
#[derive(Debug, Clone)]
pub struct EnumSpecification {
    enum_: Vec<EnumStmt>,
}

///
/// "leafref-specification".
///
#[derive(Debug, Clone)]
pub struct LeafrefSpecification {
    path: PathStmt,
    require_instance: Option<RequireInstanceStmt>,
}

///
/// "identityref-specification".
///
#[derive(Debug, Clone)]
pub struct IdentityrefSpecification {
    base: Vec<BaseStmt>,
}

///
/// "instance-identifier-specification".
///
#[derive(Debug, Clone)]
pub struct InstanceIdentifierSpecification {
    require_instance: Option<RequireInstanceStmt>,
}

///
/// "bits-specification".
///
#[derive(Debug, Clone)]
pub struct BitsSpecification {
    bit: Vec<BitStmt>,
}

///
/// "union-specification".
///
#[derive(Debug, Clone)]
pub struct UnionSpecification {
    type_: Vec<TypeStmt>,
}

///
/// "binary-specification".
///
#[derive(Debug, Clone)]
pub struct BinarySpecification {
    length: Option<LengthStmt>,
}

///
/// "type-body" Statements.
///
#[derive(Debug, Clone)]
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
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(FractionDigitsStmt::keyword)),
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
    pub fn parse(parser: &mut Parser) -> Result<TypeBodyStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        let type_body =
            if let Ok(fraction_digits) = collect_a_stmt!(stmts, FractionDigitsStmt) {
                let range = if let Ok(range) = collect_a_stmt!(stmts, RangeStmt) {
                    Some(range)
                } else {
                    None
                };

                TypeBodyStmts::Decimal64Specification(
                    Decimal64Specification { fraction_digits, range })
            } else if let Ok(range) = collect_a_stmt!(stmts, RangeStmt) {
                TypeBodyStmts::NumericalRestrictions(
                    NumericalRestrictions { range: Some(range) })
            } else if let Ok(pattern) = collect_vec_stmt!(stmts, PatternStmt) {
                // TBD: need check pattern.len()
                let length = if let Ok(length) = collect_a_stmt!(stmts, LengthStmt) {
                    Some(length)
                } else {
                    None
                };

                TypeBodyStmts::StringRestrictions(
                    StringRestrictions { pattern, length })
            } else if let Ok(length) = collect_a_stmt!(stmts, LengthStmt) {
                TypeBodyStmts::BinarySpecification(
                    BinarySpecification { length: Some(length) })
            } else if let Ok(path) = collect_a_stmt!(stmts, PathStmt) {
                let require_instance = if let Ok(require_instance) = collect_a_stmt!(stmts, RequireInstanceStmt) {
                    Some(require_instance)
                } else {
                    None
                };

                TypeBodyStmts::LeafrefSpecification(
                    LeafrefSpecification { path, require_instance })
            } else if let Ok(require_instance) = collect_a_stmt!(stmts, RequireInstanceStmt) {
                TypeBodyStmts::InstanceIdentifierSpecification(
                    InstanceIdentifierSpecification { require_instance: Some(require_instance) })
            } else if let Ok(enum_) = collect_vec_stmt!(stmts, EnumStmt) {
                TypeBodyStmts::EnumSpecification(
                    EnumSpecification { enum_ })
            } else if let Ok(base) = collect_vec_stmt!(stmts, BaseStmt) {
                TypeBodyStmts::IdentityrefSpecification(
                    IdentityrefSpecification { base })
            } else if let Ok(bit) = collect_vec_stmt!(stmts, BitStmt) {
                TypeBodyStmts::BitsSpecification(
                    BitsSpecification { bit })
            } else if let Ok(type_) = collect_vec_stmt!(stmts, TypeStmt) {
                TypeBodyStmts::UnionSpecification(
                    UnionSpecification { type_ })
            } else {
                return Err(YangError::MissingStatement(""))
            };

        Ok(type_body)
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

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_substmts: Self::SubStmts) -> Self where Self: Sized {
        panic!();
    }
}

#[derive(Debug, Clone)]
pub struct TypedefOrGrouping {
    typedef: Vec<TypedefStmt>,
    grouping: Vec<GroupingStmt>,
}

impl Selection for TypedefOrGrouping {
    /// Sub Statements.
    type SubStmts = (Vec<TypedefStmt>, Vec<GroupingStmt>);

    /// Return list fo statement keyword.
    fn keywords() -> Vec<Keyword> {
        vec![TypedefStmt::keyword(), GroupingStmt::keyword()]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(substmts: Self::SubStmts) -> Self where Self: Sized {
        Self {
            typedef: substmts.0,
            grouping: substmts.1,
        }
    }
}
