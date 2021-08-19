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
    // Return list fo statement keyword.
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
        vec![SubStmtDef::HaveOne(SubStmtWith::Stmt(YangVersionStmt::keyword)),
             SubStmtDef::HaveOne(SubStmtWith::Stmt(NamespaceStmt::keyword)),
             SubStmtDef::HaveOne(SubStmtWith::Stmt(PrefixStmt::keyword)),
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
        vec![SubStmtDef::HaveOne(SubStmtWith::Stmt(YangVersionStmt::keyword)),
             SubStmtDef::HaveOne(SubStmtWith::Stmt(BelongsToStmt::keyword)),
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
/// Numerical Restrictions
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
/// Decimal64 Specification
///
#[derive(Debug, Clone)]
pub struct Decimal64Specification {
    fraction_digits: FractionDigitsStmt,
    range: Option<RangeStmt>,
}

impl Compound for Decimal64Specification {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::HaveOne(SubStmtWith::Stmt(FractionDigitsStmt::keyword)),
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
/// Type Body Statements.
///
#[derive(Debug, Clone)]
pub enum TypeBodyStmts {
    NumericalRestrictions(NumericalRestrictions),
    Decimal64Specification(Decimal64Specification),
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
        // string specification			[length-stmt] *pattern-stmt		(any order)
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
