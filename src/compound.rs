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
use crate::parse_a_stmt;

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
    pub fn parse(parser: &mut Parser) -> Result<TypeBodyStmts, YangError> {
        let token = parser.peek_token()?;
        let stmts = match token {
            Token::Identifier(ref keyword) => {
                match keyword as &str {
                    "range" => {
                        let range = parse_a_stmt!(RangeStmt, parser)?;
                        if parser.expect_keyword("fraction-digits")? {
                            let decimal64_specification = Decimal64Specification {
                                fraction_digits: parse_a_stmt!(FractionDigitsStmt, parser)?,
                                range: Some(range),
                            };
                            TypeBodyStmts::Decimal64Specification(decimal64_specification)
                        } else {
                            let numerical_restrictions = NumericalRestrictions { range: Some(range) };
                            TypeBodyStmts::NumericalRestrictions(numerical_restrictions)
                        }
                    }
                    "fraction-digits" => {
                        let fraction_digits = parse_a_stmt!(FractionDigitsStmt, parser)?;
                        if parser.expect_keyword("range")? {
                            let decimal64_specification = Decimal64Specification {
                                fraction_digits: fraction_digits,
                                range: Some(parse_a_stmt!(RangeStmt, parser)?),
                            };
                            TypeBodyStmts::Decimal64Specification(decimal64_specification)
                        } else {
                            let decimal64_specification = Decimal64Specification {
                                fraction_digits: fraction_digits,
                                range: None,
                            };
                            TypeBodyStmts::Decimal64Specification(decimal64_specification)
                        }
                    }
                    "length" => {
/*
                        let length = parse_a_stmt!(LengthStmt, parser)?;
                        if parser.expect_keyword("pattern")? {
                            let string_restrictions = StringRestrictions {
                                length: parse_a_stmt!(LengthStmt, parser)?,
                                pattern: parse_a_stmt!(PatternStmt, parser)?,
                            };
                            TypeBodyStmts::StringRestrictions(string_restrictions)
                        } else {
                            let binary_specification = BinarySpecification { length: Some(length) };
                            TypeBodyStmts::BinarySpecification(binary_specification)
                        }
*/
                        panic!();
                    }
                    "pattern" => {
                        panic!();
                    }
                    "enum" => {
                        panic!();
                    }
                    "path" => {
                        panic!();
                    }
                    "require-instance" => {
                        panic!();
                    }
                    "base" => {
                        panic!();
                    }
                    "bit" => {
                        panic!();
                    }
                    "type" => {
                        panic!();
                    }
                    "binary" => {
                        panic!();
                    }
                    _ => return Err(YangError::UnexpectedStatement(parser.line())),
                }
            }
            Token::BlockEnd => {
                panic!();
            }
            _ => return Err(YangError::UnexpectedStatement(parser.line())),
        };

        // numerical restrictions		[range-stmt]
        // decimal64 specification		fraction-digits-stmt [range-stmt]	(any order)
        // string restrictions			[length-stmt] *pattern-stmt		(any order)
        // enum specification			1*enum-stmt
        // leafref-specification		path-stmt [require-instance-stmt]	(any order)
        // identityref-specification		1*base-stmt
        // instance-identifier-specpfication	[require-instance-stmt]
        // bits-specification			1*bit-stmt
        // union-specification			1*type-stmt
        // binary-specification			[length-stmt]

//        let stmts = TypeBodyStmts::NumericalRestrictions(NumericalRestrictions { range: None });

        Ok(stmts)
    }
}
