//
// YANG - YANG statement
//  Copyright (C) 2021 Toshiaki Takada
//

use url::Url;

use super::core::*;
use super::error::*;
use super::parser::*;
use super::arg::*;
use super::substmt::*;
use super::compound::*;
use super::compound::Compound;

use crate::collect_a_stmt;
use crate::collect_vec_stmt;
use crate::collect_opt_stmt;

//
// Trait for a single YANG statement.
//
pub trait Stmt {
    /// Arg type.
    type Arg;

    /// Sub Statements.
    type SubStmts;

    /// Return statement keyword.
    fn keyword() -> Keyword;

    /// Return true if this statement has sub-statements.
    fn has_substmts() -> bool {
        false
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        false
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        panic!("{:?}", Self::keyword());
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(_arg: Self::Arg) -> StmtType where Self: Sized {
        panic!("{:?}", Self::keyword());
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_arg: Self::Arg, _substmts: Self::SubStmts) -> StmtType where Self: Sized {
        panic!("{:?}", Self::keyword());
    }

    /// Parse substatements.
    fn parse_substmts(_parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        panic!("{:?}", Self::keyword());
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
/// The "module" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleStmt {
    /// Identifier arg.
    arg: Identifier,

    /// Module header statements.
    module_header: ModuleHeaderStmts,

    /// Linkage statements.
    linkage: LinkageStmts,

    /// Meta statements.
    meta: MetaStmts,

    /// Revision statements.
    revision: RevisionStmts,

    /// Body statements.
    body: BodyStmts,
}

impl Stmt for ModuleStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (ModuleHeaderStmts, LinkageStmts, MetaStmts, RevisionStmts, BodyStmts);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "module"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ModuleStmt(ModuleStmt {
            arg,
            module_header: substmts.0,
            linkage: substmts.1,
            meta: substmts.2,
            revision: substmts.3,
            body: substmts.4,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let module_header = ModuleHeaderStmts::parse(parser)?;
        let linkage = LinkageStmts::parse(parser)?;
        let meta = MetaStmts::parse(parser)?;
        let revision = RevisionStmts::parse(parser)?;
        let body = BodyStmts::parse(parser)?;

        Ok((module_header, linkage, meta, revision, body))
    }
}

///
/// The "submodule" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct SubmoduleStmt {
    /// Identifier arg.
    arg: Identifier,

    /// Submodule header statements.
    submodule_header: SubmoduleHeaderStmts,

    /// Linkage statements.
    linkage: LinkageStmts,

    /// Meta statements.
    meta: MetaStmts,

    /// Revision statements.
    revision: RevisionStmts,

    /// Body statements.
    body: BodyStmts,
}

impl Stmt for SubmoduleStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (SubmoduleHeaderStmts, LinkageStmts, MetaStmts, RevisionStmts, BodyStmts);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "submodule"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::SubmoduleStmt(SubmoduleStmt {
            arg,
            submodule_header: substmts.0,
            linkage: substmts.1,
            meta: substmts.2,
            revision: substmts.3,
            body: substmts.4,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let submodule_header = SubmoduleHeaderStmts::parse(parser)?;
        let linkage = LinkageStmts::parse(parser)?;
        let meta = MetaStmts::parse(parser)?;
        let revision = RevisionStmts::parse(parser)?;
        let body = BodyStmts::parse(parser)?;

        Ok((submodule_header, linkage, meta, revision, body))
    }
}

///
/// The "yang-version" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct YangVersionStmt {
    /// Yang version arg.
    arg: YangVersionArg,
}

impl Stmt for YangVersionStmt {
    /// Arg type.
    type Arg = YangVersionArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "yang-version"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::YangVersionStmt(YangVersionStmt {
            arg,
        })
    }
}

///
/// The "import" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    /// Identifier arg.
    arg: Identifier,

    /// Prefix statement.
    prefix: PrefixStmt,

    /// Revision date statement.
    revision_date: Option<RevisionDateStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for ImportStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (PrefixStmt, Option<RevisionDateStmt>,Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "import"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::HasOne(SubStmtWith::Stmt(PrefixStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(RevisionDateStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ImportStmt(ImportStmt {
            arg,
            prefix: substmts.0,
            revision_date: substmts.1,
            description: substmts.2,
            reference: substmts.3,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_a_stmt!(stmts, PrefixStmt)?,
            collect_opt_stmt!(stmts, RevisionDateStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "include" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct IncludeStmt {
    /// Identifier arg.
    arg: Identifier,

    /// Revision date statement.
    revision_date: Option<RevisionDateStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for IncludeStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<RevisionDateStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "include"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(RevisionDateStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::IncludeStmt(IncludeStmt {
            arg,
            revision_date: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::IncludeStmt(IncludeStmt {
            arg,
            revision_date: substmts.0,
            description: substmts.1,
            reference: substmts.2,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, RevisionDateStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "namespace" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct NamespaceStmt {
    /// URI.
    arg: Url,
}

impl Stmt for NamespaceStmt {
    /// Arg type.
    type Arg = Url;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "namespace"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::NamespaceStmt(NamespaceStmt { arg })
    }
}

///
/// The "prefix" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixStmt {
    /// Identifier arg.
    arg: Identifier,
}

impl Stmt for PrefixStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "prefix"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::PrefixStmt(PrefixStmt { arg })
    }
}

///
/// The "belongs-to" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct BelongsToStmt {
    /// Identifier arg.
    arg: Identifier,

    /// Prefix statement.
    prefix: PrefixStmt,
}

impl Stmt for BelongsToStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (PrefixStmt,);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "belongs-to"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::HasOne(SubStmtWith::Stmt(PrefixStmt::keyword)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::BelongsToStmt(BelongsToStmt {
            arg,
            prefix: substmts.0,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_a_stmt!(stmts, PrefixStmt)?,))
    }
}

///
/// The "organization" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct OrganizationStmt {
    /// String.
    arg: String,
}

impl Stmt for OrganizationStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "organization"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::OrganizationStmt(OrganizationStmt { arg })
    }
}

///
/// The "contact" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ContactStmt {
    /// String.
    arg: String,
}

impl Stmt for ContactStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "contact"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ContactStmt(ContactStmt { arg })
    }
}

///
/// The "description" Statement.
/// 
#[derive(Debug, Clone, PartialEq)]
pub struct DescriptionStmt {
    /// String.
    arg: String,
}

impl Stmt for DescriptionStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "description"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::DescriptionStmt(DescriptionStmt { arg })
    }
}

///
/// The "reference" Statement.
/// 
#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceStmt {
    /// String.
    arg: String,
}

impl Stmt for ReferenceStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "reference"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ReferenceStmt(ReferenceStmt { arg })
    }
}

///
/// The "units" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct UnitsStmt {
    /// String.
    arg: String,
}

impl Stmt for UnitsStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "units"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::UnitsStmt(UnitsStmt { arg })
    }
}

///
/// The "revision" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct RevisionStmt {
    /// Revision date.
    arg: DateArg,

    /// Description statement..
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for RevisionStmt {
    /// Arg type.
    type Arg = DateArg;

    /// Sub Statements.
    type SubStmts = (Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "revision"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::RevisionStmt(RevisionStmt {
            arg,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::RevisionStmt(RevisionStmt {
            arg,
            description: substmts.0,
            reference: substmts.1,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;
        
        Ok((collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "revision-date" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct RevisionDateStmt {
    /// Revision date.
    arg: DateArg,
}

impl Stmt for RevisionDateStmt {
    /// Arg type.
    type Arg = DateArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "revision-date"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::RevisionDateStmt(RevisionDateStmt { arg })
    }
}

///
/// The "extension" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ExtensionStmt {
    /// Identifier arg.
    arg: Identifier,

    /// Argument statement.
    argument: Option<ArgumentStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for ExtensionStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<ArgumentStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "extension"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(ArgumentStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ExtensionStmt(ExtensionStmt {
            arg,
            argument: None,
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ExtensionStmt(ExtensionStmt {
            arg,
            argument: substmts.0,
            status: substmts.1,
            description: substmts.2,
            reference: substmts.3,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, ArgumentStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "argument" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ArgumentStmt {
    /// Identifier arg.
    arg: Identifier,

    /// Yin element statement.
    yin_element: Option<YinElementStmt>,
}

impl Stmt for ArgumentStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<YinElementStmt>,);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "argument"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(YinElementStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ArgumentStmt(ArgumentStmt {
            arg,
            yin_element: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ArgumentStmt(ArgumentStmt {
            arg,
            yin_element: substmts.0,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, YinElementStmt)?,))
    }
}

///
/// The "yin-element" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct YinElementStmt {
    /// Yin element arg.
    arg: YinElementArg,
}

impl Stmt for YinElementStmt {
    /// Arg type.
    type Arg = YinElementArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "yin-element"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::YinElementStmt(YinElementStmt {
            arg,
        })
    }
}

///
/// The "identity" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct IdentityStmt {
    /// Identifier arg.
    arg: Identifier,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Base statement.
    base: Vec<BaseStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for IdentityStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Vec<BaseStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "identity"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(BaseStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::IdentityStmt(IdentityStmt {
            arg,
            if_feature: Vec::new(),
            base: Vec::new(),
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::IdentityStmt(IdentityStmt {
            arg,
            if_feature: substmts.0,
            base: substmts.1,
            status: substmts.2,
            description: substmts.3,
            reference: substmts.4,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, BaseStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "base" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct BaseStmt {
    /// Identifier-ref arg.
    arg: IdentifierRef,
}

impl Stmt for BaseStmt {
    /// Arg type.
    type Arg = IdentifierRef;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "base"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::BaseStmt(BaseStmt {
            arg,
        })
    }
}

///
/// The "feature" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct FeatureStmt {
    /// Identifier arg.
    arg: Identifier,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for FeatureStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "feature"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::FeatureStmt(FeatureStmt {
            arg,
            if_feature: Vec::new(),
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::FeatureStmt(FeatureStmt {
            arg,
            if_feature: substmts.0,
            status: substmts.1,
            description: substmts.2,
            reference: substmts.3,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "if-feature" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct IfFeatureStmt {
    /// If-feature-expr str.
    arg: IfFeatureExpr,
}

impl Stmt for IfFeatureStmt {
    /// Arg type.
    type Arg = IfFeatureExpr;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "if-feature"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::IfFeatureStmt(IfFeatureStmt { arg })
    }
}

///
/// The "typedef" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct TypedefStmt {
    /// Identifier arg.
    arg: Identifier,

    /// Type statement.
    type_: TypeStmt,

    /// Units statement.
    units: Option<UnitsStmt>,

    /// Default statement.
    default: Option<DefaultStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for TypedefStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (TypeStmt, Option<UnitsStmt>, Option<DefaultStmt>,
                     Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "typedef"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::HasOne(SubStmtWith::Stmt(TypeStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(UnitsStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DefaultStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::TypedefStmt(TypedefStmt {
            arg,
            type_: substmts.0,
            units: substmts.1,
            default: substmts.2,
            status: substmts.3,
            description: substmts.4,
            reference: substmts.5,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;
        
        Ok((collect_a_stmt!(stmts, TypeStmt)?,
            collect_opt_stmt!(stmts, UnitsStmt)?,
            collect_opt_stmt!(stmts, DefaultStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "type" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct TypeStmt {
    /// Identifier ref arg.
    arg: IdentifierRef,

    /// Type-body statements.
    type_body: Option<TypeBodyStmts>,
}

impl Stmt for TypeStmt {
    /// Arg type.
    type Arg = IdentifierRef;

    /// Sub Statements.
    type SubStmts = Option<TypeBodyStmts>;

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "type"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::TypeStmt(TypeStmt {
            arg,
            type_body: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::TypeStmt(TypeStmt {
            arg,
            type_body: substmts,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let type_body = TypeBodyStmts::parse(parser)?;

        Ok(Some(type_body))
    }
}

///
/// The "range" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct RangeStmt {
    /// Range Arg.
    arg: RangeArg,

    /// Error Message Statement.
    error_message: Option<ErrorMessageStmt>,
    
    /// Error App Tag Statement.
    error_app_tag: Option<ErrorAppTagStmt>,

    /// Description Statement.
    description: Option<DescriptionStmt>,

    /// Reference Statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for RangeStmt {
    /// Arg type.
    type Arg = RangeArg;

    /// Sub Statements.
    type SubStmts = (Option<ErrorMessageStmt>, Option<ErrorAppTagStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "range"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(ErrorMessageStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ErrorAppTagStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::RangeStmt(RangeStmt {
            arg,
            error_message: None,
            error_app_tag: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::RangeStmt(RangeStmt {
            arg,
            error_message: substmts.0,
            error_app_tag: substmts.1,
            description: substmts.2,
            reference: substmts.3,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, ErrorMessageStmt)?,
            collect_opt_stmt!(stmts, ErrorAppTagStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,))
    }
}

///
/// The "fraction-digits" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct FractionDigitsStmt {
    /// Fraction-digits arg.
    arg: FractionDigitsArg,
}

impl Stmt for FractionDigitsStmt {
    /// Arg type.
    type Arg = FractionDigitsArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "fraction-digits"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::FractionDigitsStmt(FractionDigitsStmt {
            arg,
        })
    }
}

///
/// The "length" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct LengthStmt {
    /// Length arg.
    arg: LengthArg,

    /// Error-message statement.
    error_message: Option<ErrorMessageStmt>,

    /// Error-app-tag statement.
    error_app_tag: Option<ErrorAppTagStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for LengthStmt {
    /// Arg type.
    type Arg = LengthArg;

    /// Sub Statements.
    type SubStmts = (Option<ErrorMessageStmt>, Option<ErrorAppTagStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "length"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(ErrorMessageStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ErrorAppTagStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::LengthStmt(LengthStmt {
            arg,
            error_message: None,
            error_app_tag: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::LengthStmt(LengthStmt {
            arg,
            error_message: substmts.0,
            error_app_tag: substmts.1,
            description: substmts.2,
            reference: substmts.3,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, ErrorMessageStmt)?,
            collect_opt_stmt!(stmts, ErrorAppTagStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,))
    }
}

///
/// The "pattern" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct PatternStmt {
    /// String.
    arg: String,

    /// Modifier statement.
    modifier: Option<ModifierStmt>,

    /// Error-message statement.
    error_message: Option<ErrorMessageStmt>,

    /// Error-app-tag statement.
    error_app_tag: Option<ErrorAppTagStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for PatternStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (Option<ModifierStmt>, Option<ErrorMessageStmt>, Option<ErrorAppTagStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "pattern"
    }


    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(ModifierStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ErrorMessageStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ErrorAppTagStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::PatternStmt(PatternStmt {
            arg,
            modifier: None,
            error_message: None,
            error_app_tag: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::PatternStmt(PatternStmt {
            arg,
            modifier: substmts.0,
            error_message: substmts.1,
            error_app_tag: substmts.2,
            description: substmts.3,
            reference: substmts.4,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, ModifierStmt)?,
            collect_opt_stmt!(stmts, ErrorMessageStmt)?,
            collect_opt_stmt!(stmts, ErrorAppTagStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,))
    }
}

///
/// The "modifier" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ModifierStmt {
    /// Modifier arg.
    arg: ModifierArg,
}

impl Stmt for ModifierStmt {
    /// Arg type.
    type Arg = ModifierArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "modifier"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ModifierStmt(ModifierStmt {
            arg,
        })
    }
}

///
/// The "default" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct DefaultStmt {
    /// String.
    arg: String,
}

impl Stmt for DefaultStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "default"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::DefaultStmt(DefaultStmt {
            arg,
        })
    }
}

///
/// The "enum" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct EnumStmt {
    /// String.
    arg: String,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Value statement.
    value: Option<ValueStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for EnumStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Option<ValueStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "enum"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ValueStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::EnumStmt(EnumStmt {
            arg,
            if_feature: Vec::new(),
            value: None,
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::EnumStmt(EnumStmt {
            arg,
            if_feature: substmts.0,
            value: substmts.1,
            status: substmts.2,
            description: substmts.3,
            reference: substmts.4,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, ValueStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "path" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct PathStmt {
    /// Path arg.
    arg: PathArg,
}

impl Stmt for PathStmt {
    /// Arg type.
    type Arg = PathArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "path"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::PathStmt(PathStmt {
            arg,
        })
    }
}

///
/// The "require-instance" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct RequireInstanceStmt {
    /// Require-instance arg.
    arg: RequireInstanceArg,
}

impl Stmt for RequireInstanceStmt {
    /// Arg type.
    type Arg = RequireInstanceArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "require-instance"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::RequireInstanceStmt(RequireInstanceStmt {
            arg,
        })
    }
}

///
/// The "bit" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct BitStmt {
    /// Identifier arg.
    arg: Identifier,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Position statement.
    position: Option<PositionStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for BitStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Option<PositionStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "bit"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(PositionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::BitStmt(BitStmt {
            arg,
            if_feature: Vec::new(),
            position: None,
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::BitStmt(BitStmt {
            arg,
            if_feature: substmts.0,
            position: substmts.1,
            status: substmts.2,
            description: substmts.3,
            reference: substmts.4,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, PositionStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "position" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct PositionStmt {
    /// Position value arg.
    arg: PositionValueArg,
}

impl Stmt for PositionStmt {
    /// Arg type.
    type Arg = PositionValueArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "position"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::PositionStmt(PositionStmt {
            arg,
        })
    }
}

///
/// The "status" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct StatusStmt {
    /// Status arg.
    arg: StatusArg,
}

impl Stmt for StatusStmt {
    /// Arg type.
    type Arg = StatusArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "status"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::StatusStmt(StatusStmt {
            arg,
        })
    }
}


///
/// The "config" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ConfigStmt {
    /// Config arg.
    arg: ConfigArg,
}

impl Stmt for ConfigStmt {
    /// Arg type.
    type Arg = ConfigArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "config"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ConfigStmt(ConfigStmt {
            arg,
        })
    }
}

///
/// The "mandatory" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct MandatoryStmt {
    /// Mandatory arg.
    arg: MandatoryArg,
}

impl Stmt for MandatoryStmt {
    /// Arg type.
    type Arg = MandatoryArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "mandatory"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::MandatoryStmt(MandatoryStmt {
            arg,
        })
    }
}

///
/// The "presence" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct PresenceStmt {
    /// String.
    arg: String,
}

impl Stmt for PresenceStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "presence"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::PresenceStmt(PresenceStmt {
            arg,
        })
    }
}

///
/// The "ordered-by" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct OrderedByStmt {
    /// Ordered-by arg.
    arg: OrderedByArg,
}

impl Stmt for OrderedByStmt {
    /// Arg type.
    type Arg = OrderedByArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "ordered-by"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::OrderedByStmt(OrderedByStmt {
            arg,
        })
    }
}

///
/// The "must" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct MustStmt {
    /// String.
    arg: String,

    /// Error-message statement.
    error_message: Option<ErrorMessageStmt>,

    /// Error-app-tag statement.
    error_app_tag: Option<ErrorAppTagStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for MustStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (Option<ErrorMessageStmt>, Option<ErrorAppTagStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "must"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(ErrorMessageStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ErrorAppTagStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::MustStmt(MustStmt {
            arg,
            error_message: None,
            error_app_tag: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::MustStmt(MustStmt {
            arg,
            error_message: substmts.0,
            error_app_tag: substmts.1,
            description: substmts.2,
            reference: substmts.3,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, ErrorMessageStmt)?,
            collect_opt_stmt!(stmts, ErrorAppTagStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "error-message" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ErrorMessageStmt {
    /// String.
    arg: String,
}

impl Stmt for ErrorMessageStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "error-message"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ErrorMessageStmt(ErrorMessageStmt {
            arg,
        })
    }
}

///
/// The "error-app-tag" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ErrorAppTagStmt {
    /// String.
    arg: String,
}

impl Stmt for ErrorAppTagStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "error-app-tag"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ErrorAppTagStmt(ErrorAppTagStmt {
            arg,
        })
    }
}

///
/// The "min-elements" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct MinElementsStmt {
    /// Min-value arg.
    arg: MinValueArg,
}

impl Stmt for MinElementsStmt {
    /// Arg type.
    type Arg = MinValueArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "min-elements"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::MinElementsStmt(MinElementsStmt { arg })
    }
}

///
/// The "max-elements" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct MaxElementsStmt {
    /// Max-value arg.
    arg: MaxValueArg,
}

impl Stmt for MaxElementsStmt {
    /// Arg type.
    type Arg = MaxValueArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "max-elements"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::MaxElementsStmt(MaxElementsStmt { arg })
    }
}

///
/// The "value" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ValueStmt {
    /// Integer-value.
    arg: IntegerValue,
}

impl Stmt for ValueStmt {
    /// Arg type.
    type Arg = IntegerValue;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "value"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ValueStmt(ValueStmt { arg })
    }
}

///
/// The "grouping" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct GroupingStmt {
    /// Identifier arg.
    arg: Identifier,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Typedef / Grouping statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// Data-def statement.
    data_def: DataDefStmt,

    /// Action statement.
    action: Vec<ActionStmt>,

    /// Notification statement.
    notification: Vec<NotificationStmt>,
}

impl Stmt for GroupingStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<StatusStmt>,Option<DescriptionStmt>, Option<ReferenceStmt>,
                     TypedefOrGrouping, DataDefStmt, Vec<ActionStmt>, Vec<NotificationStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "grouping"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(ActionStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(NotificationStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::GroupingStmt(GroupingStmt {
            arg,
            status: None,
            description: None,
            reference: None,
            typedef_or_grouping: TypedefOrGrouping::new(),
            data_def: DataDefStmt::new(),
            action: Vec::new(),
            notification: Vec::new(),
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::GroupingStmt(GroupingStmt {
            arg,
            status: substmts.0,
            description: substmts.1,
            reference: substmts.2,
            typedef_or_grouping: substmts.3,
            data_def: substmts.4,
            action: substmts.5,
            notification: substmts.6,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,)),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
            collect_vec_stmt!(stmts, ActionStmt)?,
            collect_vec_stmt!(stmts, NotificationStmt)?,
        ))
    }
}

///
/// The "container" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ContainerStmt {
    /// Identifier arg.
    arg: Identifier,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Must statement.
    must: Vec<MustStmt>,

    /// Presence statement.
    presence: Option<PresenceStmt>,

    /// Config statement.
    config: Option<ConfigStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Typedef / Grouping statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// Data-def statement.
    data_def: DataDefStmt,

    /// Action statement.
    action: Vec<ActionStmt>,

    /// Notification statement.
    notification: Vec<NotificationStmt>,
}

impl Stmt for ContainerStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Vec<MustStmt>, Option<PresenceStmt>,
                     Option<ConfigStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>,
                     TypedefOrGrouping, DataDefStmt, Vec<ActionStmt>, Vec<NotificationStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "container"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(PresenceStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
             SubStmtDef::OneOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(ActionStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(NotificationStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ContainerStmt(ContainerStmt {
            arg,
            when: None,
            if_feature: Vec::new(),
            must: Vec::new(),
            presence: None,
            config: None,
            status: None,
            description: None,
            reference: None,
            typedef_or_grouping: TypedefOrGrouping::new(),
            data_def: DataDefStmt::new(),
            action: Vec::new(),
            notification: Vec::new(),
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ContainerStmt(ContainerStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            must: substmts.2,
            presence: substmts.3,
            config: substmts.4,
            status: substmts.5,
            description: substmts.6,
            reference: substmts.7,
            typedef_or_grouping: substmts.8,
            data_def: substmts.9,
            action: substmts.10,
            notification: substmts.11,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, PresenceStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,)),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
            collect_vec_stmt!(stmts, ActionStmt)?,
            collect_vec_stmt!(stmts, NotificationStmt)?,
        ))
    }
}

///
/// The "leaf" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct LeafStmt {
    /// Identifier arg.
    arg: Identifier,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Type statement.
    type_: TypeStmt,

    /// Units statement.
    units: Option<UnitsStmt>,

    /// Must statement.
    must: Vec<MustStmt>,

    /// Default statement.
    default: Option<DefaultStmt>,

    /// Config statement.
    config: Option<ConfigStmt>,

    /// Mandatory statement.
    mandatory: Option<MandatoryStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for LeafStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, TypeStmt, Option<UnitsStmt>, Vec<MustStmt>,
                     Option<DefaultStmt>, Option<ConfigStmt>, Option<MandatoryStmt>, Option<StatusStmt>,
                     Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "leaf"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::HasOne(SubStmtWith::Stmt(TypeStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(UnitsStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DefaultStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MandatoryStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::LeafStmt(LeafStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            type_: substmts.2,
            units: substmts.3,
            must: substmts.4,
            default: substmts.5,
            config: substmts.6,
            mandatory: substmts.7,
            status: substmts.8,
            description: substmts.9,
            reference: substmts.10,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_a_stmt!(stmts, TypeStmt)?,
            collect_opt_stmt!(stmts, UnitsStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, DefaultStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, MandatoryStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "leaf-list" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct LeafListStmt {
    /// Identifier arg.
    arg: Identifier,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Type statement.
    type_: TypeStmt,

    /// Units statement.
    units: Option<UnitsStmt>,

    /// Must statement.
    must: Vec<MustStmt>,

    /// Default statement.
    default: Vec<DefaultStmt>,

    /// Config statement.
    config: Option<ConfigStmt>,

    /// Min-elements statement.
    min_elements: Option<MinElementsStmt>,

    /// Max-elements statement.
    max_elements: Option<MaxElementsStmt>,

    /// Ordered-by statement.
    ordered_by: Option<OrderedByStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for LeafListStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, TypeStmt, Option<UnitsStmt>, Vec<MustStmt>,
                     Vec<DefaultStmt>, Option<ConfigStmt>, Option<MinElementsStmt>, Option<MaxElementsStmt>,
                     Option<OrderedByStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "leaf-list"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::HasOne(SubStmtWith::Stmt(TypeStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(UnitsStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(DefaultStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MinElementsStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MaxElementsStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(OrderedByStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::LeafListStmt(LeafListStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            type_: substmts.2,
            units: substmts.3,
            must: substmts.4,
            default: substmts.5,
            config: substmts.6,
            min_elements: substmts.7,
            max_elements: substmts.8,
            ordered_by: substmts.9,
            status: substmts.10,
            description: substmts.11,
            reference: substmts.12,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_a_stmt!(stmts, TypeStmt)?,
            collect_opt_stmt!(stmts, UnitsStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_vec_stmt!(stmts, DefaultStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, MinElementsStmt)?,
            collect_opt_stmt!(stmts, MaxElementsStmt)?,
            collect_opt_stmt!(stmts, OrderedByStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "list" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ListStmt {
    /// Identifier arg.
    arg: Identifier,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Must statement.
    must: Vec<MustStmt>,

    /// Key statement.
    key: Option<KeyStmt>,

    /// Unique statement.
    unique: Vec<UniqueStmt>,

    /// Config statement.
    config: Option<ConfigStmt>,

    /// Min-elements statement.
    min_elements: Option<MinElementsStmt>,

    /// Max-elements statement.
    max_elements: Option<MaxElementsStmt>,

    /// Ordered-by statement.
    ordered_by: Option<OrderedByStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Typedef / Grouping statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// Data-def statement.
    data_def: DataDefStmt,

    /// Action statement.
    action: Vec<ActionStmt>,

    /// Notification statement.
    notification: Vec<NotificationStmt>,
}

impl Stmt for ListStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Vec<MustStmt>, Option<KeyStmt>,
                     Vec<UniqueStmt>, Option<ConfigStmt>, Option<MinElementsStmt>, Option<MaxElementsStmt>,
                     Option<OrderedByStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>,
                     TypedefOrGrouping, DataDefStmt, Vec<ActionStmt>, Vec<NotificationStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "list"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(KeyStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(UniqueStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MinElementsStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MaxElementsStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(OrderedByStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
             SubStmtDef::OneOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(ActionStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(NotificationStmt::keyword)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ListStmt(ListStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            must: substmts.2,
            key: substmts.3,
            unique: substmts.4,
            config: substmts.5,
            min_elements: substmts.6,
            max_elements: substmts.7,
            ordered_by: substmts.8,
            status: substmts.9,
            description: substmts.10,
            reference: substmts.11,
            typedef_or_grouping: substmts.12,
            data_def: substmts.13,
            action: substmts.14,
            notification: substmts.15,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, KeyStmt)?,
            collect_vec_stmt!(stmts, UniqueStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, MinElementsStmt)?,
            collect_opt_stmt!(stmts, MaxElementsStmt)?,
            collect_opt_stmt!(stmts, OrderedByStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,)),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
            collect_vec_stmt!(stmts, ActionStmt)?,
            collect_vec_stmt!(stmts, NotificationStmt)?,
        ))
    }
}

///
/// The "key" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct KeyStmt {
    /// Key arg.
    arg: KeyArg,
}

impl Stmt for KeyStmt {
    /// Arg type.
    type Arg = KeyArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "key"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::KeyStmt(KeyStmt {
            arg,
        })
    }
}

///
/// The list's "unique" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct UniqueStmt {
    /// Unique arg.
    arg: UniqueArg,
}

impl Stmt for UniqueStmt {
    /// Arg type.
    type Arg = UniqueArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "unique"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::UniqueStmt(UniqueStmt {
            arg,
        })
    }
}

///
/// The "choice" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ChoiceStmt {
    /// Identifier arg.
    arg: Identifier,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Default statement.
    default: Option<DefaultStmt>,

    /// Config statement.
    config: Option<ConfigStmt>,

    /// Mandatory statement.
    mandatory: Option<MandatoryStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Short-case / Case statement.
    short_case_or_case: ShortCaseOrCaseStmt,
}

impl Stmt for ChoiceStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "choice"
    }

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Option<DefaultStmt>, Option<ConfigStmt>,
                     Option<MandatoryStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>,
                     ShortCaseOrCaseStmt);

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DefaultStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MandatoryStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::HasOne(SubStmtWith::Selection(ShortCaseOrCaseStmt::keywords)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ChoiceStmt(ChoiceStmt {
            arg,
            when: None,
            if_feature: Vec::new(),
            default: None,
            config: None,
            mandatory: None,
            status: None,
            description: None,
            reference: None,
            short_case_or_case: ShortCaseOrCaseStmt::new(),
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ChoiceStmt(ChoiceStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            default: substmts.2,
            config: substmts.3,
            mandatory: substmts.4,
            status: substmts.5,
            description: substmts.6,
            reference: substmts.7,
            short_case_or_case: substmts.8,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, DefaultStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, MandatoryStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            ShortCaseOrCaseStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, CaseStmt)?,
            )),
        ))
    }
}

///
/// The "case" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct CaseStmt {
    /// Identifier arg.
    arg: Identifier,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Data-def statement.
    data_def: DataDefStmt,
}

impl Stmt for CaseStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "case"
    }

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Option<StatusStmt>,
                     Option<DescriptionStmt>, Option<ReferenceStmt>, DataDefStmt);

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::CaseStmt(CaseStmt {
            arg,
            when: None,
            if_feature: Vec::new(),
            status: None,
            description: None,
            reference: None,
            data_def: DataDefStmt::new(),
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::CaseStmt(CaseStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            status: substmts.2,
            description: substmts.3,
            reference: substmts.4,
            data_def: substmts.5,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
        ))
    }
}

///
/// The "anydata" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct AnydataStmt {
    /// Identifier arg.
    arg: Identifier,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Must statement.
    must: Vec<MustStmt>,

    /// Config statement.
    config: Option<ConfigStmt>,

    /// Mandatory statement.
    mandatory: Option<MandatoryStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for AnydataStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Vec<MustStmt>, Option<ConfigStmt>,
                     Option<MandatoryStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "anydata"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MandatoryStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::AnydataStmt(AnydataStmt {
            arg,
            when: None,
            if_feature: Vec::new(),
            must: Vec::new(),
            config: None,
            mandatory: None,
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::AnydataStmt(AnydataStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            must: substmts.2,
            config: substmts.3,
            mandatory: substmts.4,
            status: substmts.5,
            description: substmts.6,
            reference: substmts.7,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, MandatoryStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "anyxml" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct AnyxmlStmt {
    /// Identifier arg.
    arg: Identifier,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Must statement.
    must: Vec<MustStmt>,

    /// Config statement.
    config: Option<ConfigStmt>,

    /// Mandatory statement.
    mandatory: Option<MandatoryStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for AnyxmlStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Vec<MustStmt>, Option<ConfigStmt>,
                     Option<MandatoryStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "anyxml"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MandatoryStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::AnyxmlStmt(AnyxmlStmt {
            arg,
            when: None,
            if_feature: Vec::new(),
            must: Vec::new(),
            config: None,
            mandatory: None,
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::AnyxmlStmt(AnyxmlStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            must: substmts.2,
            config: substmts.3,
            mandatory: substmts.4,
            status: substmts.5,
            description: substmts.6,
            reference: substmts.7,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, MandatoryStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "uses" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct UsesStmt {
    /// Identifier-ref arg.
    arg: IdentifierRef,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Refine statement.
    refine: Vec<RefineStmt>,

    /// Uses statement.
    uses_augment: Vec<UsesAugmentStmt>,
}

impl Stmt for UsesStmt {
    /// Arg type.
    type Arg = IdentifierRef;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Option<StatusStmt>, Option<DescriptionStmt>,
                     Option<ReferenceStmt>, Vec<RefineStmt>, Vec<UsesAugmentStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "uses"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(RefineStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(UsesAugmentStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::UsesStmt(UsesStmt {
            arg,
            when: None,
            if_feature: Vec::new(),
            status: None,
            description: None,
            reference: None,
            refine: Vec::new(),
            uses_augment: Vec::new(),
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::UsesStmt(UsesStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            status: substmts.2,
            description: substmts.3,
            reference: substmts.4,
            refine: substmts.5,
            uses_augment: substmts.6,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            collect_vec_stmt!(stmts, RefineStmt)?,
            collect_vec_stmt!(stmts, UsesAugmentStmt)?))
    }
}

///
/// The "refine" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct RefineStmt {
    /// Refine arg.
    arg: RefineArg,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Must statement.
    must: Vec<MustStmt>,

    /// Presence statement.
    presence: Option<PresenceStmt>,

    /// Default statement.
    default: Vec<DefaultStmt>,

    /// Config statement.
    config: Option<ConfigStmt>,

    /// Mandatory statement.
    mandatory: Option<MandatoryStmt>,

    /// Min-elements statement.
    min_elements: Option<MinElementsStmt>,

    /// Max-elements statement.
    max_elements: Option<MaxElementsStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for RefineStmt {
    /// Arg type.
    type Arg = RefineArg;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Vec<MustStmt>, Option<PresenceStmt>,
                     Vec<DefaultStmt>, Option<ConfigStmt>, Option<MandatoryStmt>,
                     Option<MinElementsStmt>, Option<MaxElementsStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "refine"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(PresenceStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(DefaultStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MandatoryStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MinElementsStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MaxElementsStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::RefineStmt(RefineStmt {
            arg,
            if_feature: Vec::new(),
            must: Vec::new(),
            presence: None,
            default: Vec::new(),
            config: None,
            mandatory: None,
            min_elements: None,
            max_elements: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::RefineStmt(RefineStmt {
            arg,
            if_feature: substmts.0,
            must: substmts.1,
            presence: substmts.2,
            default: substmts.3,
            config: substmts.4,
            mandatory: substmts.5,
            min_elements: substmts.6,
            max_elements: substmts.7,
            description: substmts.8,
            reference: substmts.9,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, PresenceStmt)?,
            collect_vec_stmt!(stmts, DefaultStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, MandatoryStmt)?,
            collect_opt_stmt!(stmts, MinElementsStmt)?,
            collect_opt_stmt!(stmts, MaxElementsStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "uses-augment" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct UsesAugmentStmt {
    /// Uses augment arg.
    arg: UsesAugmentArg,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Data-def / case / action / notification statement.
    data_def_or_else: DataDefOrElse,
}

impl Stmt for UsesAugmentStmt {
    /// Arg type.
    type Arg = UsesAugmentArg;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Option<StatusStmt>,
                     Option<DescriptionStmt>, Option<ReferenceStmt>, DataDefOrElse);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "uses-augment"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::OneOrMore(SubStmtWith::Selection(DataDefOrElse::keywords)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::UsesAugmentStmt(UsesAugmentStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            status: substmts.2,
            description: substmts.3,
            reference: substmts.4,
            data_def_or_else: substmts.5,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            DataDefOrElse::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,
                collect_vec_stmt!(stmts, CaseStmt)?,
                collect_vec_stmt!(stmts, ActionStmt)?,
                collect_vec_stmt!(stmts, NotificationStmt)?,
            )),
        ))
    }
}

///
/// The "augment" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct AugmentStmt {
    /// Augment arg.
    arg: AugmentArg,

    /// When statement.
    when: Option<WhenStmt>,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Data-def / case / action / notification statement.
    data_def_or_else: DataDefOrElse,
}

impl Stmt for AugmentStmt {
    /// Arg type.
    type Arg = AugmentArg;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Option<StatusStmt>,
                     Option<DescriptionStmt>, Option<ReferenceStmt>, DataDefOrElse);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "augment"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::OneOrMore(SubStmtWith::Selection(DataDefOrElse::keywords)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::AugmentStmt(AugmentStmt {
            arg,
            when: substmts.0,
            if_feature: substmts.1,
            status: substmts.2,
            description: substmts.3,
            reference: substmts.4,
            data_def_or_else: substmts.5,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            DataDefOrElse::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,
                collect_vec_stmt!(stmts, CaseStmt)?,
                collect_vec_stmt!(stmts, ActionStmt)?,
                collect_vec_stmt!(stmts, NotificationStmt)?,
            )),
        ))
    }
}

///
/// The "when" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct WhenStmt {
    /// String.
    arg: String,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for WhenStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "when"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::WhenStmt(WhenStmt {
            arg,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::WhenStmt(WhenStmt {
            arg,
            description: substmts.0,
            reference: substmts.1,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
/// The "rpc" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct RpcStmt {
    /// Identifier arg.
    arg: Identifier,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Typedef / grouping statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// Input statement.
    input: Option<InputStmt>,

    /// Output statement.
    output: Option<OutputStmt>,
}

impl Stmt for RpcStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>,
                     TypedefOrGrouping, Option<InputStmt>, Option<OutputStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "rpc"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
             SubStmtDef::Optional(SubStmtWith::Stmt(InputStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(OutputStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::RpcStmt(RpcStmt {
            arg,
            if_feature: Vec::new(),
            status: None,
            description: None,
            reference: None,
            typedef_or_grouping: TypedefOrGrouping::new(),
            input: None,
            output: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::RpcStmt(RpcStmt {
            arg,
            if_feature: substmts.0,
            status: substmts.1,
            description: substmts.2,
            reference: substmts.3,
            typedef_or_grouping: substmts.4,
            input: substmts.5,
            output: substmts.6,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,
            )),
            collect_opt_stmt!(stmts, InputStmt)?,
            collect_opt_stmt!(stmts, OutputStmt)?,
        ))
    }
}

///
/// The "action" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct ActionStmt {
    /// Identifier arg.
    arg: Identifier,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Typedef / Grouping statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// Input statement.
    input: Option<InputStmt>,

    /// Output statement.
    output: Option<OutputStmt>,
}

impl Stmt for ActionStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>,
                     TypedefOrGrouping, Option<InputStmt>, Option<OutputStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "action"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
             SubStmtDef::Optional(SubStmtWith::Stmt(InputStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(OutputStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ActionStmt(ActionStmt {
            arg,
            if_feature: Vec::new(),
            status: None,
            description: None,
            reference: None,
            typedef_or_grouping: TypedefOrGrouping::new(),
            input: None,
            output: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ActionStmt(ActionStmt {
            arg,
            if_feature: substmts.0,
            status: substmts.1,
            description: substmts.2,
            reference: substmts.3,
            typedef_or_grouping: substmts.4,
            input: substmts.5,
            output: substmts.6,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,
            )),
            collect_opt_stmt!(stmts, InputStmt)?,
            collect_opt_stmt!(stmts, OutputStmt)?,
        ))
    }
}

///
/// The "input" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct InputStmt {
    /// Must statement.
    must: Vec<MustStmt>,

    /// Typedef / Grouping statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// Data-def statement.
    data_def: DataDefStmt,
}

impl Stmt for InputStmt {
    /// Arg type.
    type Arg = NoArg;

    /// Sub Statements.
    type SubStmts = (Vec<MustStmt>, TypedefOrGrouping, DataDefStmt);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "input"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
             SubStmtDef::OneOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::InputStmt(InputStmt {
            must: substmts.0,
            typedef_or_grouping: substmts.1,
            data_def: substmts.2,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_vec_stmt!(stmts, MustStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,)),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
        ))
    }
}

///
/// The "output" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct OutputStmt {
    /// Must statement.
    must: Vec<MustStmt>,

    /// Typedef / Grouping statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// Data-def statement.
    data_def: DataDefStmt,
}

impl Stmt for OutputStmt {
    /// Arg type.
    type Arg = NoArg;

    /// Sub Statements.
    type SubStmts = (Vec<MustStmt>, TypedefOrGrouping, DataDefStmt);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "output"
    }


    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
             SubStmtDef::OneOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::OutputStmt(OutputStmt {
            must: substmts.0,
            typedef_or_grouping: substmts.1,
            data_def: substmts.2,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_vec_stmt!(stmts, MustStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,)),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
        ))
    }
}

///
/// The "notification" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct NotificationStmt {
    /// Identifier arg.
    arg: Identifier,

    /// If-feature statement.
    if_feature: Vec<IfFeatureStmt>,

    /// Must statement.
    must: Vec<MustStmt>,

    /// Status statement.
    status: Option<StatusStmt>,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Typedef / Grouping statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// Data-def statement.
    data_def: DataDefStmt,
}

impl Stmt for NotificationStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Vec<MustStmt>, Option<StatusStmt>, Option<DescriptionStmt>,
                     Option<ReferenceStmt>, TypedefOrGrouping, DataDefStmt);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "notification"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::NotificationStmt(NotificationStmt {
            arg,
            if_feature: Vec::new(),
            must: Vec::new(),
            status: None,
            description: None,
            reference: None,
            typedef_or_grouping: TypedefOrGrouping::new(),
            data_def: DataDefStmt::new(),
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::NotificationStmt(NotificationStmt {
            arg,
            if_feature: substmts.0,
            must: substmts.1,
            status: substmts.2,
            description: substmts.3,
            reference: substmts.4,
            typedef_or_grouping: substmts.5,
            data_def: substmts.6,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,)),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
        ))
    }
}

///
/// The "deviation" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct DeviationStmt {
    /// Deviation arg.
    arg: DeviationArg,

    /// Description statement.
    description: Option<DescriptionStmt>,

    /// Reference statement.
    reference: Option<ReferenceStmt>,

    /// Deviate statement.
    deviate: Vec<DeviateStmt>,
}

impl Stmt for DeviationStmt {
    /// Arg type.
    type Arg = DeviationArg;

    /// Sub Statements.
    type SubStmts = (Option<DescriptionStmt>, Option<ReferenceStmt>, Vec<DeviateStmt>);

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "deviation"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
             SubStmtDef::OneOrMore(SubStmtWith::Stmt(DeviateStmt::keyword)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::DeviationStmt(DeviationStmt {
            arg,
            description: substmts.0,
            reference: substmts.1,
            deviate: substmts.2,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            collect_vec_stmt!(stmts, DeviateStmt)?,))
    }
}

///
/// The "deviate" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub enum DeviateStmt {
    /// "deviate not-supported".
    NotSupported,

    /// "deviate add".
    Add(DeviateAddStmt),

    /// "deviate replace".
    Replace(DeviateReplaceStmt),

    /// "deviate delete".
    Delete(DeviateDeleteStmt),
}

impl Stmt for DeviateStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "deviate"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError>  where Self::Arg: StmtArg, Self: Sized {
        let arg = Self::Arg::parse_arg(parser)?;

        match &arg as &str {
            "add" => {
                let stmt = DeviateAddStmt::parse(parser)?;
                Ok(StmtType::DeviateStmt(DeviateStmt::Add(stmt)))
            }
            "replace" => {
                let stmt = DeviateDeleteStmt::parse(parser)?;
                Ok(StmtType::DeviateStmt(DeviateStmt::Delete(stmt)))
            }
            "delete" => {
                let stmt = DeviateReplaceStmt::parse(parser)?;
                Ok(StmtType::DeviateStmt(DeviateStmt::Replace(stmt)))
            }
            "not-supported" => {
                Ok(StmtType::DeviateStmt(DeviateStmt::NotSupported))
            }
            _ => Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

///
/// The "deviate add" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct DeviateAddStmt {
    /// Units statement.
    units: Option<UnitsStmt>,

    /// Must statement.
    must: Vec<MustStmt>,

    /// Unique statement.
    unqiue: Vec<UniqueStmt>,

    /// Default statement.
    default: Vec<DefaultStmt>,

    /// Config statement.
    config: Option<ConfigStmt>,

    /// Mandatory statement.
    mandatory: Option<MandatoryStmt>,

    /// Min-elements statement.
    min_elements: Option<MinElementsStmt>,

    /// Max-elements statement.
    max_elements: Option<MaxElementsStmt>,
}

impl Compound for DeviateAddStmt {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(UnitsStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(UniqueStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(DefaultStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MandatoryStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MinElementsStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MaxElementsStmt::keyword)),
        ]
    }
}

impl DeviateAddStmt {
    pub fn parse(parser: &mut Parser) -> Result<DeviateAddStmt, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(DeviateAddStmt {
            units: collect_opt_stmt!(stmts, UnitsStmt)?,
            must: collect_vec_stmt!(stmts, MustStmt)?,
            unqiue: collect_vec_stmt!(stmts, UniqueStmt)?,
            default: collect_vec_stmt!(stmts, DefaultStmt)?,
            config: collect_opt_stmt!(stmts, ConfigStmt)?,
            mandatory: collect_opt_stmt!(stmts, MandatoryStmt)?,
            min_elements: collect_opt_stmt!(stmts, MinElementsStmt)?,
            max_elements: collect_opt_stmt!(stmts, MaxElementsStmt)?,
        })
    }
}

///
/// The "deviate delete" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct DeviateDeleteStmt {
    /// Units statement.
    units: Option<UnitsStmt>,

    /// Must statement.
    must: Vec<MustStmt>,

    /// Unique statement.
    unqiue: Vec<UniqueStmt>,

    /// Default statement.
    default: Vec<DefaultStmt>,
}

impl Compound for DeviateDeleteStmt {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(UnitsStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(UniqueStmt::keyword)),
             SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(DefaultStmt::keyword)),
        ]
    }
}

impl DeviateDeleteStmt {
    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<DeviateDeleteStmt, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(DeviateDeleteStmt {
            units: collect_opt_stmt!(stmts, UnitsStmt)?,
            must: collect_vec_stmt!(stmts, MustStmt)?,
            unqiue: collect_vec_stmt!(stmts, UniqueStmt)?,
            default: collect_vec_stmt!(stmts, DefaultStmt)?,
        })
    }
}

///
/// The "deviate replace" Statement.
///
#[derive(Debug, Clone, PartialEq)]
pub struct DeviateReplaceStmt {
    /// Type statement.
    type_: Option<TypeStmt>,

    /// Units statement.
    units: Option<UnitsStmt>,

    /// Default statement.
    default: Option<DefaultStmt>,

    /// Config statement.
    config: Option<ConfigStmt>,

    /// Mandatory statement.
    mandatory: Option<MandatoryStmt>,

    /// Min-elements statement.
    min_elements: Option<MinElementsStmt>,

    /// Max-elements statement.
    max_elements: Option<MaxElementsStmt>,
}

impl Compound for DeviateReplaceStmt {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(TypeStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(UnitsStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(DefaultStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MandatoryStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MinElementsStmt::keyword)),
             SubStmtDef::Optional(SubStmtWith::Stmt(MaxElementsStmt::keyword)),
        ]
    }
}

impl DeviateReplaceStmt {
    pub fn parse(parser: &mut Parser) -> Result<DeviateReplaceStmt, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok(DeviateReplaceStmt {
            type_: collect_opt_stmt!(stmts, TypeStmt)?,
            units: collect_opt_stmt!(stmts, UnitsStmt)?,
            default: collect_opt_stmt!(stmts, DefaultStmt)?,
            config: collect_opt_stmt!(stmts, ConfigStmt)?,
            mandatory: collect_opt_stmt!(stmts, MandatoryStmt)?,
            min_elements: collect_opt_stmt!(stmts, MinElementsStmt)?,
            max_elements: collect_opt_stmt!(stmts, MaxElementsStmt)?,
        })
    }
}


#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use super::*;

    #[test]
    pub fn test_import_stmt() {
        let s = r#"openconfig-inet-types {
                       prefix oc-inet;
                       revision-date 2017-07-06;
                   }"#;

        let mut parser = Parser::new(s.to_string());
        match ImportStmt::parse(&mut parser) {
            Ok(stmt) => {
                assert_eq!(stmt,
                           StmtType::ImportStmt(ImportStmt {
                               arg: Identifier::from_str("openconfig-inet-types").unwrap(),
                               prefix: PrefixStmt { arg: Identifier::from_str("oc-inet").unwrap() },
                               revision_date: Some(RevisionDateStmt { arg: DateArg::from_str("2017-07-06").unwrap() } ),
                               description: None,
                               reference: None })
                );
            }
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    #[test]
    pub fn test_include_stmt() {
        let s = r#"openconfig-inet-types {
                       prefix oc-inet;
                       revision-date 2017-07-06;
                   }"#;

        let mut parser = Parser::new(s.to_string());
        match IncludeStmt::parse(&mut parser) {
            Ok(stmt) => panic!("{:?}", stmt),
            Err(err) => assert_eq!(err.to_string(), "Unexpected token at line 1"),
        }

        let s = r#"openconfig-inet-types {
                       revision-date 2017-07-06;
                   }"#;

        let mut parser = Parser::new(s.to_string());
        match IncludeStmt::parse(&mut parser) {
            Ok(stmt) => {
                println!("{:?}", stmt);

                assert_eq!(stmt,
                           StmtType::IncludeStmt(IncludeStmt {
                               arg: Identifier::from_str("openconfig-inet-types").unwrap(),
                               revision_date: Some(RevisionDateStmt { arg: DateArg::from_str("2017-07-06").unwrap() } ),
                               description: None,
                               reference: None })
                );
            }
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    
    
    #[test]
    pub fn test_typedef_stmt() {
// TBD
        let s = r#"area-id {
	type union {

            type ipv4-address {

            }

            type uint32 {
		range "0..4294967295";
            }
	}
    }"#;

        let mut parser = Parser::new(s.to_string());

        let res = TypedefStmt::parse(&mut parser);
        println!("{:?}", res);
    }
}
