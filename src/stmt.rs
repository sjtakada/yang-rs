//
// YANG - YANG statement
//  Copyright (C) 2021 Toshiaki Takada
//

use std::str::FromStr;
use url::Url;

use derive_getters::Getters;

use super::arg::*;
use super::compound::Compound;
use super::compound::*;
use super::core::*;
use super::error::*;
use super::parser::*;
use super::substmt::*;

use crate::collect_a_stmt;
use crate::collect_opt_stmt;
use crate::collect_vec_stmt;

///
/// Trait for a single YANG statement.
///
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
    fn new_with_arg(_arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        panic!("{:?}", Self::keyword());
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_arg: Self::Arg, _substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        panic!("{:?}", Self::keyword());
    }

    /// Parse substatements.
    fn parse_substmts(_parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        panic!("{:?}", Self::keyword());
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<YangStmt, YangError>
    where
        Self::Arg: StmtArg,
        Self: Sized,
    {
        let arg = Self::Arg::parse_arg(parser)?;

        if Self::has_substmts() {
            let token = parser.get_token()?;
            match token {
                Token::BlockBegin => {
                    let substmts = Self::parse_substmts(parser)?;
                    let token = parser.get_token()?;

                    match token {
                        Token::BlockEnd => Ok(Self::new_with_substmts(arg, substmts)),
                        _ => Err(YangError::UnexpectedToken(token.to_string())),
                    }
                }
                _ => Err(YangError::UnexpectedToken(format!(
                    "'{}' in '{}', expected BlockBegin",
                    token.to_string(),
                    Self::keyword()
                ))),
            }
        } else if Self::opt_substmts() {
            let token = parser.get_token()?;
            match token {
                Token::StatementEnd => Ok(Self::new_with_arg(arg)),
                Token::BlockBegin => {
                    let substmts = Self::parse_substmts(parser)?;
                    let token = parser.get_token()?;
                    match token {
                        Token::BlockEnd => Ok(Self::new_with_substmts(arg, substmts)),
                        _ => Err(YangError::UnexpectedToken(token.to_string())),
                    }
                }
                _ => Err(YangError::UnexpectedToken(format!(
                    "'{}' in '{}', expected BlockBegin or ;",
                    token.to_string(),
                    Self::keyword()
                ))),
            }
        } else {
            let token = parser.get_token()?;
            match token {
                Token::StatementEnd => Ok(Self::new_with_arg(arg)),
                _ => Err(YangError::UnexpectedToken(format!(
                    "'{}' in '{}', expected ;",
                    token.to_string(),
                    Self::keyword()
                ))),
            }
        }
    }
}

///
/// The "module" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ModuleStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "module-header" statements.
    module_header: ModuleHeaderStmts,

    /// "linkage" statements.
    linkage: LinkageStmts,

    /// "meta" statements.
    meta: MetaStmts,

    /// "revision" statements.
    revision: RevisionStmts,

    /// "body" statements.
    body: BodyStmts,
}

impl Stmt for ModuleStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        ModuleHeaderStmts,
        LinkageStmts,
        MetaStmts,
        RevisionStmts,
        BodyStmts,
    );

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "module"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ModuleStmt(ModuleStmt {
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
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct SubmoduleStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "submodule-header" statements.
    submodule_header: SubmoduleHeaderStmts,

    /// "linkage" statements.
    linkage: LinkageStmts,

    /// "meta" statements.
    meta: MetaStmts,

    /// "revision" statements.
    revision: RevisionStmts,

    /// "body" statements.
    body: BodyStmts,
}

impl Stmt for SubmoduleStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        SubmoduleHeaderStmts,
        LinkageStmts,
        MetaStmts,
        RevisionStmts,
        BodyStmts,
    );

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "submodule"
    }

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::SubmoduleStmt(SubmoduleStmt {
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
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::YangVersionStmt(YangVersionStmt { arg })
    }
}

///
/// The "import" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ImportStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "prefix" statement.
    prefix: PrefixStmt,

    /// "revision-date" statement.
    revision_date: Option<RevisionDateStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for ImportStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        PrefixStmt,
        Option<RevisionDateStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::HasOne(SubStmtWith::Stmt(PrefixStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(RevisionDateStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ImportStmt(ImportStmt {
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

        Ok((
            collect_a_stmt!(stmts, PrefixStmt)?,
            collect_opt_stmt!(stmts, RevisionDateStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "include" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct IncludeStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "revision-date" statement.
    revision_date: Option<RevisionDateStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for IncludeStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Option<RevisionDateStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(RevisionDateStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::IncludeStmt(IncludeStmt {
            arg,
            revision_date: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::IncludeStmt(IncludeStmt {
            arg,
            revision_date: substmts.0,
            description: substmts.1,
            reference: substmts.2,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((
            collect_opt_stmt!(stmts, RevisionDateStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "namespace" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::NamespaceStmt(NamespaceStmt { arg })
    }
}

///
/// The "prefix" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct PrefixStmt {
    /// "dentifier-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::PrefixStmt(PrefixStmt { arg })
    }
}

///
/// The "belongs-to" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct BelongsToStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "prefix" statement.
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
        vec![SubStmtDef::HasOne(SubStmtWith::Stmt(PrefixStmt::keyword))]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::BelongsToStmt(BelongsToStmt {
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
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::OrganizationStmt(OrganizationStmt { arg })
    }
}

///
/// The "contact" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ContactStmt(ContactStmt { arg })
    }
}

///
/// The "description" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::DescriptionStmt(DescriptionStmt { arg })
    }
}

///
/// The "reference" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ReferenceStmt(ReferenceStmt { arg })
    }
}

///
/// The "units" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::UnitsStmt(UnitsStmt { arg })
    }
}

///
/// The "revision" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct RevisionStmt {
    /// "revision-date".
    arg: DateArg,

    /// "description" statement..
    description: Option<DescriptionStmt>,

    /// "reference" statement.
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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::RevisionStmt(RevisionStmt {
            arg,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::RevisionStmt(RevisionStmt {
            arg,
            description: substmts.0,
            reference: substmts.1,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "revision-date" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct RevisionDateStmt {
    /// "revision-date".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::RevisionDateStmt(RevisionDateStmt { arg })
    }
}

///
/// The "extension" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ExtensionStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "argument" statement.
    argument: Option<ArgumentStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for ExtensionStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Option<ArgumentStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(ArgumentStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ExtensionStmt(ExtensionStmt {
            arg,
            argument: None,
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ExtensionStmt(ExtensionStmt {
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

        Ok((
            collect_opt_stmt!(stmts, ArgumentStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "argument" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ArgumentStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "yin-element" statement.
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
        vec![SubStmtDef::Optional(SubStmtWith::Stmt(
            YinElementStmt::keyword,
        ))]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ArgumentStmt(ArgumentStmt {
            arg,
            yin_element: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ArgumentStmt(ArgumentStmt {
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
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct YinElementStmt {
    /// "yin-element-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::YinElementStmt(YinElementStmt { arg })
    }
}

///
/// The "identity" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct IdentityStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "base" statement.
    base: Vec<BaseStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for IdentityStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Vec<IfFeatureStmt>,
        Vec<BaseStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(BaseStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::IdentityStmt(IdentityStmt {
            arg,
            if_feature: Vec::new(),
            base: Vec::new(),
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::IdentityStmt(IdentityStmt {
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

        Ok((
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, BaseStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "base" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct BaseStmt {
    /// "dentifier-ref-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::BaseStmt(BaseStmt { arg })
    }
}

///
/// The "feature" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct FeatureStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for FeatureStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Vec<IfFeatureStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::FeatureStmt(FeatureStmt {
            arg,
            if_feature: Vec::new(),
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::FeatureStmt(FeatureStmt {
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

        Ok((
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "if-feature" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct IfFeatureStmt {
    /// "if-feature-expr-str".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::IfFeatureStmt(IfFeatureStmt { arg })
    }
}

///
/// The "typedef" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct TypedefStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "type" statement.
    type_: TypeStmt,

    /// "units" statement.
    units: Option<UnitsStmt>,

    /// "default" statement.
    default: Option<DefaultStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for TypedefStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        TypeStmt,
        Option<UnitsStmt>,
        Option<DefaultStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::HasOne(SubStmtWith::Stmt(TypeStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(UnitsStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DefaultStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::TypedefStmt(TypedefStmt {
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

        Ok((
            collect_a_stmt!(stmts, TypeStmt)?,
            collect_opt_stmt!(stmts, UnitsStmt)?,
            collect_opt_stmt!(stmts, DefaultStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "type" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct TypeStmt {
    /// "identifier-ref-arg".
    arg: IdentifierRef,

    /// "type-body" statements.
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::TypeStmt(TypeStmt {
            arg,
            type_body: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::TypeStmt(TypeStmt {
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
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct RangeStmt {
    /// "range-arg".
    arg: RangeArg,

    /// "error-message" statement.
    error_message: Option<ErrorMessageStmt>,

    /// "error-app-tag" statement.
    error_app_tag: Option<ErrorAppTagStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for RangeStmt {
    /// Arg type.
    type Arg = RangeArg;

    /// Sub Statements.
    type SubStmts = (
        Option<ErrorMessageStmt>,
        Option<ErrorAppTagStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(ErrorMessageStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ErrorAppTagStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::RangeStmt(RangeStmt {
            arg,
            error_message: None,
            error_app_tag: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::RangeStmt(RangeStmt {
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

        Ok((
            collect_opt_stmt!(stmts, ErrorMessageStmt)?,
            collect_opt_stmt!(stmts, ErrorAppTagStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "fraction-digits" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct FractionDigitsStmt {
    /// "fraction-digits-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::FractionDigitsStmt(FractionDigitsStmt { arg })
    }
}

///
/// The "length" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct LengthStmt {
    /// "length-arg".
    arg: LengthArg,

    /// "error-message" statement.
    error_message: Option<ErrorMessageStmt>,

    /// "error-app-tag" statement.
    error_app_tag: Option<ErrorAppTagStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for LengthStmt {
    /// Arg type.
    type Arg = LengthArg;

    /// Sub Statements.
    type SubStmts = (
        Option<ErrorMessageStmt>,
        Option<ErrorAppTagStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(ErrorMessageStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ErrorAppTagStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::LengthStmt(LengthStmt {
            arg,
            error_message: None,
            error_app_tag: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::LengthStmt(LengthStmt {
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

        Ok((
            collect_opt_stmt!(stmts, ErrorMessageStmt)?,
            collect_opt_stmt!(stmts, ErrorAppTagStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "pattern" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct PatternStmt {
    /// String.
    arg: String,

    /// "modifier" statement.
    modifier: Option<ModifierStmt>,

    /// "error-message" statement.
    error_message: Option<ErrorMessageStmt>,

    /// "error-app-tag" statement.
    error_app_tag: Option<ErrorAppTagStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for PatternStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (
        Option<ModifierStmt>,
        Option<ErrorMessageStmt>,
        Option<ErrorAppTagStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(ModifierStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ErrorMessageStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ErrorAppTagStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::PatternStmt(PatternStmt {
            arg,
            modifier: None,
            error_message: None,
            error_app_tag: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::PatternStmt(PatternStmt {
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

        Ok((
            collect_opt_stmt!(stmts, ModifierStmt)?,
            collect_opt_stmt!(stmts, ErrorMessageStmt)?,
            collect_opt_stmt!(stmts, ErrorAppTagStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "modifier" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ModifierStmt {
    /// "modifier-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ModifierStmt(ModifierStmt { arg })
    }
}

///
/// The "default" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::DefaultStmt(DefaultStmt { arg })
    }
}

///
/// The "enum" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct EnumStmt {
    /// String.
    arg: String,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "value" statement.
    value: Option<ValueStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for EnumStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (
        Vec<IfFeatureStmt>,
        Option<ValueStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ValueStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::EnumStmt(EnumStmt {
            arg,
            if_feature: Vec::new(),
            value: None,
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::EnumStmt(EnumStmt {
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

        Ok((
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, ValueStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "path" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct PathStmt {
    /// "path" arg.
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::PathStmt(PathStmt { arg })
    }
}

///
/// The "require-instance" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct RequireInstanceStmt {
    /// "require-instance" arg.
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::RequireInstanceStmt(RequireInstanceStmt { arg })
    }
}

///
/// The "bit" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct BitStmt {
    /// "identifier" arg.
    arg: Identifier,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "position" statement.
    position: Option<PositionStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for BitStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Vec<IfFeatureStmt>,
        Option<PositionStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(PositionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::BitStmt(BitStmt {
            arg,
            if_feature: Vec::new(),
            position: None,
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::BitStmt(BitStmt {
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

        Ok((
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, PositionStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "position" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct PositionStmt {
    /// "position-value-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::PositionStmt(PositionStmt { arg })
    }
}

///
/// The "status" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct StatusStmt {
    /// "status-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::StatusStmt(StatusStmt { arg })
    }
}

///
/// The "config" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ConfigStmt {
    /// "config-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ConfigStmt(ConfigStmt { arg })
    }
}

///
/// The "mandatory" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct MandatoryStmt {
    /// "mandatory-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::MandatoryStmt(MandatoryStmt { arg })
    }
}

///
/// The "presence" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::PresenceStmt(PresenceStmt { arg })
    }
}

///
/// The "ordered-by" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct OrderedByStmt {
    /// "ordered-by-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::OrderedByStmt(OrderedByStmt { arg })
    }
}

///
/// The "must" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct MustStmt {
    /// String.
    arg: String,

    /// Error-message statement.
    error_message: Option<ErrorMessageStmt>,

    /// Error-app-tag statement.
    error_app_tag: Option<ErrorAppTagStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for MustStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (
        Option<ErrorMessageStmt>,
        Option<ErrorAppTagStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(ErrorMessageStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ErrorAppTagStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::MustStmt(MustStmt {
            arg,
            error_message: None,
            error_app_tag: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::MustStmt(MustStmt {
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

        Ok((
            collect_opt_stmt!(stmts, ErrorMessageStmt)?,
            collect_opt_stmt!(stmts, ErrorAppTagStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "error-message" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ErrorMessageStmt(ErrorMessageStmt { arg })
    }
}

///
/// The "error-app-tag" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ErrorAppTagStmt(ErrorAppTagStmt { arg })
    }
}

///
/// The "min-elements" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct MinElementsStmt {
    /// "min-value-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::MinElementsStmt(MinElementsStmt { arg })
    }
}

///
/// The "max-elements" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct MaxElementsStmt {
    /// "max-value-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::MaxElementsStmt(MaxElementsStmt { arg })
    }
}

///
/// The "value" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ValueStmt {
    /// "integer-value".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ValueStmt(ValueStmt { arg })
    }
}

///
/// The "grouping" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct GroupingStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "typedef" / "grouping" statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// "data-def" statement.
    data_def: DataDefStmt,

    /// "action" statement.
    action: Vec<ActionStmt>,

    /// "notification" statement.
    notification: Vec<NotificationStmt>,
}

impl GroupingStmt {
    pub fn typedef(&self) -> &Vec<TypedefStmt> {
        &self.typedef_or_grouping.typedef()
    }

    pub fn grouping(&self) -> &Vec<GroupingStmt> {
        &self.typedef_or_grouping.grouping()
    }

    pub fn container(&self) -> &Vec<ContainerStmt> {
        &self.data_def.container()
    }

    pub fn leaf(&self) -> &Vec<LeafStmt> {
        &self.data_def.leaf()
    }

    pub fn leaf_list(&self) -> &Vec<LeafListStmt> {
        &self.data_def.leaf_list()
    }

    pub fn list(&self) -> &Vec<ListStmt> {
        &self.data_def.list()
    }

    pub fn choice(&self) -> &Vec<ChoiceStmt> {
        &self.data_def.choice()
    }

    pub fn anydata(&self) -> &Vec<AnydataStmt> {
        &self.data_def.anydata()
    }

    pub fn anyxml(&self) -> &Vec<AnyxmlStmt> {
        &self.data_def.anyxml()
    }

    pub fn uses(&self) -> &Vec<UsesStmt> {
        &self.data_def.uses()
    }
}

impl Stmt for GroupingStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        TypedefOrGrouping,
        DataDefStmt,
        Vec<ActionStmt>,
        Vec<NotificationStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(ActionStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(NotificationStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::GroupingStmt(GroupingStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::GroupingStmt(GroupingStmt {
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

        Ok((
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,
            )),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,
            )),
            collect_vec_stmt!(stmts, ActionStmt)?,
            collect_vec_stmt!(stmts, NotificationStmt)?,
        ))
    }
}

///
/// The "container" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ContainerStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "when" statement.
    when: Option<WhenStmt>,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "must" statement.
    must: Vec<MustStmt>,

    /// "presence" statement.
    presence: Option<PresenceStmt>,

    /// "config" statement.
    config: Option<ConfigStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "typedef" / "grouping" statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// "data-def" statement.
    data_def: DataDefStmt,

    /// "action" statement.
    action: Vec<ActionStmt>,

    /// "notification" statement.
    notification: Vec<NotificationStmt>,
}

impl ContainerStmt {
    pub fn typedef(&self) -> &Vec<TypedefStmt> {
        &self.typedef_or_grouping.typedef()
    }

    pub fn grouping(&self) -> &Vec<GroupingStmt> {
        &self.typedef_or_grouping.grouping()
    }

    pub fn container(&self) -> &Vec<ContainerStmt> {
        &self.data_def.container()
    }

    pub fn leaf(&self) -> &Vec<LeafStmt> {
        &self.data_def.leaf()
    }

    pub fn leaf_list(&self) -> &Vec<LeafListStmt> {
        &self.data_def.leaf_list()
    }

    pub fn list(&self) -> &Vec<ListStmt> {
        &self.data_def.list()
    }

    pub fn choice(&self) -> &Vec<ChoiceStmt> {
        &self.data_def.choice()
    }

    pub fn anydata(&self) -> &Vec<AnydataStmt> {
        &self.data_def.anydata()
    }

    pub fn anyxml(&self) -> &Vec<AnyxmlStmt> {
        &self.data_def.anyxml()
    }

    pub fn uses(&self) -> &Vec<UsesStmt> {
        &self.data_def.uses()
    }
}

impl Stmt for ContainerStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Option<WhenStmt>,
        Vec<IfFeatureStmt>,
        Vec<MustStmt>,
        Option<PresenceStmt>,
        Option<ConfigStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        TypedefOrGrouping,
        DataDefStmt,
        Vec<ActionStmt>,
        Vec<NotificationStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ContainerStmt(ContainerStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ContainerStmt(ContainerStmt {
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

        Ok((
            collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, PresenceStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,
            )),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,
            )),
            collect_vec_stmt!(stmts, ActionStmt)?,
            collect_vec_stmt!(stmts, NotificationStmt)?,
        ))
    }
}

///
/// The "leaf" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct LeafStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "when" statement.
    when: Option<WhenStmt>,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "type" statement.
    type_: TypeStmt,

    /// "units" statement.
    units: Option<UnitsStmt>,

    /// "must" statement.
    must: Vec<MustStmt>,

    /// "default" statement.
    default: Option<DefaultStmt>,

    /// "config" statement.
    config: Option<ConfigStmt>,

    /// "mandatory" statement.
    mandatory: Option<MandatoryStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for LeafStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Option<WhenStmt>,
        Vec<IfFeatureStmt>,
        TypeStmt,
        Option<UnitsStmt>,
        Vec<MustStmt>,
        Option<DefaultStmt>,
        Option<ConfigStmt>,
        Option<MandatoryStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::LeafStmt(LeafStmt {
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

        Ok((
            collect_opt_stmt!(stmts, WhenStmt)?,
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
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct LeafListStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "when" statement.
    when: Option<WhenStmt>,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "type" statement.
    type_: TypeStmt,

    /// "units" statement.
    units: Option<UnitsStmt>,

    /// "must" statement.
    must: Vec<MustStmt>,

    /// "default" statement.
    default: Vec<DefaultStmt>,

    /// "config" statement.
    config: Option<ConfigStmt>,

    /// "min-elements" statement.
    min_elements: Option<MinElementsStmt>,

    /// "max-elements" statement.
    max_elements: Option<MaxElementsStmt>,

    /// "ordered-by" statement.
    ordered_by: Option<OrderedByStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for LeafListStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Option<WhenStmt>,
        Vec<IfFeatureStmt>,
        TypeStmt,
        Option<UnitsStmt>,
        Vec<MustStmt>,
        Vec<DefaultStmt>,
        Option<ConfigStmt>,
        Option<MinElementsStmt>,
        Option<MaxElementsStmt>,
        Option<OrderedByStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::LeafListStmt(LeafListStmt {
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

        Ok((
            collect_opt_stmt!(stmts, WhenStmt)?,
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
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ListStmt {
    /// "Identifier-arg".
    arg: Identifier,

    /// "when" statement.
    when: Option<WhenStmt>,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "must" statement.
    must: Vec<MustStmt>,

    /// "key" statement.
    key: Option<KeyStmt>,

    /// "unique" statement.
    unique: Vec<UniqueStmt>,

    /// "config" statement.
    config: Option<ConfigStmt>,

    /// "min-elements" statement.
    min_elements: Option<MinElementsStmt>,

    /// "max-elements" statement.
    max_elements: Option<MaxElementsStmt>,

    /// "ordered-by" statement.
    ordered_by: Option<OrderedByStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "typedef" / "grouping" statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// "data-def" statement.
    data_def: DataDefStmt,

    /// "action" statement.
    action: Vec<ActionStmt>,

    /// "notification" statement.
    notification: Vec<NotificationStmt>,
}

impl ListStmt {
    pub fn typedef(&self) -> &Vec<TypedefStmt> {
        &self.typedef_or_grouping.typedef()
    }

    pub fn grouping(&self) -> &Vec<GroupingStmt> {
        &self.typedef_or_grouping.grouping()
    }

    pub fn container(&self) -> &Vec<ContainerStmt> {
        &self.data_def.container()
    }

    pub fn leaf(&self) -> &Vec<LeafStmt> {
        &self.data_def.leaf()
    }

    pub fn leaf_list(&self) -> &Vec<LeafListStmt> {
        &self.data_def.leaf_list()
    }

    pub fn list(&self) -> &Vec<ListStmt> {
        &self.data_def.list()
    }

    pub fn choice(&self) -> &Vec<ChoiceStmt> {
        &self.data_def.choice()
    }

    pub fn anydata(&self) -> &Vec<AnydataStmt> {
        &self.data_def.anydata()
    }

    pub fn anyxml(&self) -> &Vec<AnyxmlStmt> {
        &self.data_def.anyxml()
    }

    pub fn uses(&self) -> &Vec<UsesStmt> {
        &self.data_def.uses()
    }
}

impl Stmt for ListStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Option<WhenStmt>,
        Vec<IfFeatureStmt>,
        Vec<MustStmt>,
        Option<KeyStmt>,
        Vec<UniqueStmt>,
        Option<ConfigStmt>,
        Option<MinElementsStmt>,
        Option<MaxElementsStmt>,
        Option<OrderedByStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        TypedefOrGrouping,
        DataDefStmt,
        Vec<ActionStmt>,
        Vec<NotificationStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ListStmt(ListStmt {
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

        Ok((
            collect_opt_stmt!(stmts, WhenStmt)?,
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
                collect_vec_stmt!(stmts, GroupingStmt)?,
            )),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,
            )),
            collect_vec_stmt!(stmts, ActionStmt)?,
            collect_vec_stmt!(stmts, NotificationStmt)?,
        ))
    }
}

///
/// The "key" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct KeyStmt {
    /// "key-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::KeyStmt(KeyStmt { arg })
    }
}

///
/// The list's "unique" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct UniqueStmt {
    /// "unique-arg".
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::UniqueStmt(UniqueStmt { arg })
    }
}

///
/// The "choice" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ChoiceStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "when" statement.
    when: Option<WhenStmt>,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "default" statement.
    default: Option<DefaultStmt>,

    /// "config" statement.
    config: Option<ConfigStmt>,

    /// "mandatory" statement.
    mandatory: Option<MandatoryStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "short-case" / "case" statement.
    short_case_or_case: ShortCaseOrCaseStmt,
}

impl ChoiceStmt {
    pub fn choice(&self) -> &Vec<ChoiceStmt> {
        &self.short_case_or_case.choice()
    }

    pub fn container(&self) -> &Vec<ContainerStmt> {
        &self.short_case_or_case.container()
    }

    pub fn leaf(&self) -> &Vec<LeafStmt> {
        &self.short_case_or_case.leaf()
    }

    pub fn leaf_list(&self) -> &Vec<LeafListStmt> {
        &self.short_case_or_case.leaf_list()
    }

    pub fn list(&self) -> &Vec<ListStmt> {
        &self.short_case_or_case.list()
    }

    pub fn anydata(&self) -> &Vec<AnydataStmt> {
        &self.short_case_or_case.anydata()
    }

    pub fn anyxml(&self) -> &Vec<AnyxmlStmt> {
        &self.short_case_or_case.anyxml()
    }

    pub fn case(&self) -> &Vec<CaseStmt> {
        &self.short_case_or_case.case()
    }
}

impl Stmt for ChoiceStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "choice"
    }

    /// Sub Statements.
    type SubStmts = (
        Option<WhenStmt>,
        Vec<IfFeatureStmt>,
        Option<DefaultStmt>,
        Option<ConfigStmt>,
        Option<MandatoryStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        ShortCaseOrCaseStmt,
    );

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DefaultStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ConfigStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(MandatoryStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(ShortCaseOrCaseStmt::keywords)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ChoiceStmt(ChoiceStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ChoiceStmt(ChoiceStmt {
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

        Ok((
            collect_opt_stmt!(stmts, WhenStmt)?,
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
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct CaseStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "when" statement.
    when: Option<WhenStmt>,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "data-def" statement.
    data_def: DataDefStmt,
}

impl CaseStmt {
    pub fn container(&self) -> &Vec<ContainerStmt> {
        &self.data_def.container()
    }

    pub fn leaf(&self) -> &Vec<LeafStmt> {
        &self.data_def.leaf()
    }

    pub fn leaf_list(&self) -> &Vec<LeafListStmt> {
        &self.data_def.leaf_list()
    }

    pub fn list(&self) -> &Vec<ListStmt> {
        &self.data_def.list()
    }

    pub fn choice(&self) -> &Vec<ChoiceStmt> {
        &self.data_def.choice()
    }

    pub fn anydata(&self) -> &Vec<AnydataStmt> {
        &self.data_def.anydata()
    }

    pub fn anyxml(&self) -> &Vec<AnyxmlStmt> {
        &self.data_def.anyxml()
    }

    pub fn uses(&self) -> &Vec<UsesStmt> {
        &self.data_def.uses()
    }
}

impl Stmt for CaseStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Return statement keyword.
    fn keyword() -> Keyword {
        "case"
    }

    /// Sub Statements.
    type SubStmts = (
        Option<WhenStmt>,
        Vec<IfFeatureStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        DataDefStmt,
    );

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::CaseStmt(CaseStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::CaseStmt(CaseStmt {
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

        Ok((
            collect_opt_stmt!(stmts, WhenStmt)?,
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
                collect_vec_stmt!(stmts, UsesStmt)?,
            )),
        ))
    }
}

///
/// The "anydata" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct AnydataStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "when" statement.
    when: Option<WhenStmt>,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "must" statement.
    must: Vec<MustStmt>,

    /// "config" statement.
    config: Option<ConfigStmt>,

    /// "mandatory" statement.
    mandatory: Option<MandatoryStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for AnydataStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Option<WhenStmt>,
        Vec<IfFeatureStmt>,
        Vec<MustStmt>,
        Option<ConfigStmt>,
        Option<MandatoryStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::AnydataStmt(AnydataStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::AnydataStmt(AnydataStmt {
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

        Ok((
            collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, MandatoryStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "anyxml" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct AnyxmlStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "when" statement.
    when: Option<WhenStmt>,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "must" statement.
    must: Vec<MustStmt>,

    /// "config" statement.
    config: Option<ConfigStmt>,

    /// "mandatory" statement.
    mandatory: Option<MandatoryStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for AnyxmlStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Option<WhenStmt>,
        Vec<IfFeatureStmt>,
        Vec<MustStmt>,
        Option<ConfigStmt>,
        Option<MandatoryStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::AnyxmlStmt(AnyxmlStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::AnyxmlStmt(AnyxmlStmt {
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

        Ok((
            collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, MandatoryStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "uses" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct UsesStmt {
    /// "identifier-ref-arg".
    arg: IdentifierRef,

    /// "when" statement.
    when: Option<WhenStmt>,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "refine" statement.
    refine: Vec<RefineStmt>,

    /// "uses" statement.
    uses_augment: Vec<AugmentStmt>,
}

impl Stmt for UsesStmt {
    /// Arg type.
    type Arg = IdentifierRef;

    /// Sub Statements.
    type SubStmts = (
        Option<WhenStmt>,
        Vec<IfFeatureStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        Vec<RefineStmt>,
        Vec<AugmentStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(RefineStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(AugmentStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::UsesStmt(UsesStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::UsesStmt(UsesStmt {
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

        Ok((
            collect_opt_stmt!(stmts, WhenStmt)?,
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            collect_vec_stmt!(stmts, RefineStmt)?,
            collect_vec_stmt!(stmts, AugmentStmt)?,
        ))
    }
}

///
/// The "refine" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct RefineStmt {
    /// "refine-arg".
    arg: RefineArg,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "must" statement.
    must: Vec<MustStmt>,

    /// "presence" statement.
    presence: Option<PresenceStmt>,

    /// "default" statement.
    default: Vec<DefaultStmt>,

    /// "config" statement.
    config: Option<ConfigStmt>,

    /// "mandatory" statement.
    mandatory: Option<MandatoryStmt>,

    /// "min-elements" statement.
    min_elements: Option<MinElementsStmt>,

    /// "max-elements" statement.
    max_elements: Option<MaxElementsStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,
}

impl Stmt for RefineStmt {
    /// Arg type.
    type Arg = RefineArg;

    /// Sub Statements.
    type SubStmts = (
        Vec<IfFeatureStmt>,
        Vec<MustStmt>,
        Option<PresenceStmt>,
        Vec<DefaultStmt>,
        Option<ConfigStmt>,
        Option<MandatoryStmt>,
        Option<MinElementsStmt>,
        Option<MaxElementsStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
    );

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
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
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
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::RefineStmt(RefineStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::RefineStmt(RefineStmt {
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

        Ok((
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, PresenceStmt)?,
            collect_vec_stmt!(stmts, DefaultStmt)?,
            collect_opt_stmt!(stmts, ConfigStmt)?,
            collect_opt_stmt!(stmts, MandatoryStmt)?,
            collect_opt_stmt!(stmts, MinElementsStmt)?,
            collect_opt_stmt!(stmts, MaxElementsStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "augment" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct AugmentStmt {
    /// "augment-arg".
    arg: SchemaNodeid,

    /// "when" statement.
    when: Option<WhenStmt>,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "data-def" / "case" / "action" / "notification" statement.
    data_def_or_else: DataDefOrElse,
}

impl AugmentStmt {
    pub fn container(&self) -> &Vec<ContainerStmt> {
        &self.data_def_or_else.container()
    }

    pub fn leaf(&self) -> &Vec<LeafStmt> {
        &self.data_def_or_else.leaf()
    }

    pub fn leaf_list(&self) -> &Vec<LeafListStmt> {
        &self.data_def_or_else.leaf_list()
    }

    pub fn list(&self) -> &Vec<ListStmt> {
        &self.data_def_or_else.list()
    }

    pub fn choice(&self) -> &Vec<ChoiceStmt> {
        &self.data_def_or_else.choice()
    }

    pub fn anydata(&self) -> &Vec<AnydataStmt> {
        &self.data_def_or_else.anydata()
    }

    pub fn anyxml(&self) -> &Vec<AnyxmlStmt> {
        &self.data_def_or_else.anyxml()
    }

    pub fn uses(&self) -> &Vec<UsesStmt> {
        &self.data_def_or_else.uses()
    }

    pub fn case(&self) -> &Vec<CaseStmt> {
        &self.data_def_or_else.case()
    }
    pub fn action(&self) -> &Vec<ActionStmt> {
        &self.data_def_or_else.action()
    }
    pub fn notification(&self) -> &Vec<NotificationStmt> {
        &self.data_def_or_else.notification()
    }
}

impl Stmt for AugmentStmt {
    /// Arg type.
    type Arg = SchemaNodeid;

    /// Sub Statements.
    type SubStmts = (
        Option<WhenStmt>,
        Vec<IfFeatureStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        DataDefOrElse,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(WhenStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
            SubStmtDef::OneOrMore(SubStmtWith::Selection(DataDefOrElse::keywords)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::AugmentStmt(AugmentStmt {
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

        Ok((
            collect_opt_stmt!(stmts, WhenStmt)?,
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
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct WhenStmt {
    /// String.
    arg: String,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::WhenStmt(WhenStmt {
            arg,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::WhenStmt(WhenStmt {
            arg,
            description: substmts.0,
            reference: substmts.1,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
        ))
    }
}

///
/// The "rpc" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct RpcStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "typedef" / "grouping" statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// "input" statement.
    input: Option<InputStmt>,

    /// "output" statement.
    output: Option<OutputStmt>,
}

impl RpcStmt {
    pub fn typedef(&self) -> &Vec<TypedefStmt> {
        &self.typedef_or_grouping.typedef()
    }

    pub fn grouping(&self) -> &Vec<GroupingStmt> {
        &self.typedef_or_grouping.grouping()
    }
}

impl Stmt for RpcStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Vec<IfFeatureStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        TypedefOrGrouping,
        Option<InputStmt>,
        Option<OutputStmt>,
    );

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
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
            SubStmtDef::Optional(SubStmtWith::Stmt(InputStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(OutputStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::RpcStmt(RpcStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::RpcStmt(RpcStmt {
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

        Ok((
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
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
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct ActionStmt {
    /// "identifier" arg.
    arg: Identifier,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "typedef" / "grouping" statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// "input" statement.
    input: Option<InputStmt>,

    /// "output" statement.
    output: Option<OutputStmt>,
}

impl ActionStmt {
    pub fn typedef(&self) -> &Vec<TypedefStmt> {
        &self.typedef_or_grouping.typedef()
    }

    pub fn grouping(&self) -> &Vec<GroupingStmt> {
        &self.typedef_or_grouping.grouping()
    }
}

impl Stmt for ActionStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Vec<IfFeatureStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        TypedefOrGrouping,
        Option<InputStmt>,
        Option<OutputStmt>,
    );

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
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
            SubStmtDef::Optional(SubStmtWith::Stmt(InputStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(OutputStmt::keyword)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ActionStmt(ActionStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::ActionStmt(ActionStmt {
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

        Ok((
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
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
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct InputStmt {
    /// "must" statement.
    must: Vec<MustStmt>,

    /// "typedef" / "grouping" statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// "data-def" statement.
    data_def: DataDefStmt,
}

impl InputStmt {
    pub fn typedef(&self) -> &Vec<TypedefStmt> {
        &self.typedef_or_grouping.typedef()
    }

    pub fn grouping(&self) -> &Vec<GroupingStmt> {
        &self.typedef_or_grouping.grouping()
    }

    pub fn container(&self) -> &Vec<ContainerStmt> {
        &self.data_def.container()
    }

    pub fn leaf(&self) -> &Vec<LeafStmt> {
        &self.data_def.leaf()
    }

    pub fn leaf_list(&self) -> &Vec<LeafListStmt> {
        &self.data_def.leaf_list()
    }

    pub fn list(&self) -> &Vec<ListStmt> {
        &self.data_def.list()
    }

    pub fn choice(&self) -> &Vec<ChoiceStmt> {
        &self.data_def.choice()
    }

    pub fn anydata(&self) -> &Vec<AnydataStmt> {
        &self.data_def.anydata()
    }

    pub fn anyxml(&self) -> &Vec<AnyxmlStmt> {
        &self.data_def.anyxml()
    }

    pub fn uses(&self) -> &Vec<UsesStmt> {
        &self.data_def.uses()
    }
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
    #[cfg(not(feature = "cisco-nso-extensions"))]
    fn has_substmts() -> bool {
        true
    }

    /// Return true if this statement has sub-statements optionally.
    #[cfg(feature = "cisco-nso-extensions")]
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
            SubStmtDef::OneOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    #[cfg(feature = "cisco-nso-extensions")]
    fn new_with_arg(_arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::InputStmt(InputStmt {
            must: Vec::new(),
            typedef_or_grouping: TypedefOrGrouping::new(),
            data_def: DataDefStmt::new(),
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::InputStmt(InputStmt {
            must: substmts.0,
            typedef_or_grouping: substmts.1,
            data_def: substmts.2,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((
            collect_vec_stmt!(stmts, MustStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,
            )),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,
            )),
        ))
    }
}

///
/// The "output" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct OutputStmt {
    /// "must" statement.
    must: Vec<MustStmt>,

    /// "typedef" / "grouping" statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// "data-def" statement.
    data_def: DataDefStmt,
}

impl OutputStmt {
    pub fn typedef(&self) -> &Vec<TypedefStmt> {
        &self.typedef_or_grouping.typedef()
    }

    pub fn grouping(&self) -> &Vec<GroupingStmt> {
        &self.typedef_or_grouping.grouping()
    }

    pub fn container(&self) -> &Vec<ContainerStmt> {
        &self.data_def.container()
    }

    pub fn leaf(&self) -> &Vec<LeafStmt> {
        &self.data_def.leaf()
    }

    pub fn leaf_list(&self) -> &Vec<LeafListStmt> {
        &self.data_def.leaf_list()
    }

    pub fn list(&self) -> &Vec<ListStmt> {
        &self.data_def.list()
    }

    pub fn choice(&self) -> &Vec<ChoiceStmt> {
        &self.data_def.choice()
    }

    pub fn anydata(&self) -> &Vec<AnydataStmt> {
        &self.data_def.anydata()
    }

    pub fn anyxml(&self) -> &Vec<AnyxmlStmt> {
        &self.data_def.anyxml()
    }

    pub fn uses(&self) -> &Vec<UsesStmt> {
        &self.data_def.uses()
    }
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
    #[cfg(not(feature = "cisco-nso-extensions"))]
    fn has_substmts() -> bool {
        true
    }

    /// Return true if this statement has sub-statements optionally.
    #[cfg(feature = "cisco-nso-extensions")]
    fn opt_substmts() -> bool {
        true
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
            SubStmtDef::OneOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    #[cfg(feature = "cisco-nso-extensions")]
    fn new_with_arg(_arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::OutputStmt(OutputStmt {
            must: Vec::new(),
            typedef_or_grouping: TypedefOrGrouping::new(),
            data_def: DataDefStmt::new(),
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::OutputStmt(OutputStmt {
            must: substmts.0,
            typedef_or_grouping: substmts.1,
            data_def: substmts.2,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((
            collect_vec_stmt!(stmts, MustStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,
            )),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,
            )),
        ))
    }
}

///
/// The "notification" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct NotificationStmt {
    /// "identifier-arg".
    arg: Identifier,

    /// "if-feature" statement.
    if_feature: Vec<IfFeatureStmt>,

    /// "must" statement.
    must: Vec<MustStmt>,

    /// "status" statement.
    status: Option<StatusStmt>,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "typedef" / "grouping" statement.
    typedef_or_grouping: TypedefOrGrouping,

    /// "data-def" statement.
    data_def: DataDefStmt,
}

impl NotificationStmt {
    pub fn typedef(&self) -> &Vec<TypedefStmt> {
        &self.typedef_or_grouping.typedef()
    }

    pub fn grouping(&self) -> &Vec<GroupingStmt> {
        &self.typedef_or_grouping.grouping()
    }

    pub fn container(&self) -> &Vec<ContainerStmt> {
        &self.data_def.container()
    }

    pub fn leaf(&self) -> &Vec<LeafStmt> {
        &self.data_def.leaf()
    }

    pub fn leaf_list(&self) -> &Vec<LeafListStmt> {
        &self.data_def.leaf_list()
    }

    pub fn list(&self) -> &Vec<ListStmt> {
        &self.data_def.list()
    }

    pub fn choice(&self) -> &Vec<ChoiceStmt> {
        &self.data_def.choice()
    }

    pub fn anydata(&self) -> &Vec<AnydataStmt> {
        &self.data_def.anydata()
    }

    pub fn anyxml(&self) -> &Vec<AnyxmlStmt> {
        &self.data_def.anyxml()
    }

    pub fn uses(&self) -> &Vec<UsesStmt> {
        &self.data_def.uses()
    }
}

impl Stmt for NotificationStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (
        Vec<IfFeatureStmt>,
        Vec<MustStmt>,
        Option<StatusStmt>,
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        TypedefOrGrouping,
        DataDefStmt,
    );

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
        vec![
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(IfFeatureStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(StatusStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Selection(DataDefStmt::keywords)),
        ]
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::NotificationStmt(NotificationStmt {
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
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::NotificationStmt(NotificationStmt {
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

        Ok((
            collect_vec_stmt!(stmts, IfFeatureStmt)?,
            collect_vec_stmt!(stmts, MustStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            TypedefOrGrouping::new_with_substmts((
                collect_vec_stmt!(stmts, TypedefStmt)?,
                collect_vec_stmt!(stmts, GroupingStmt)?,
            )),
            DataDefStmt::new_with_substmts((
                collect_vec_stmt!(stmts, ContainerStmt)?,
                collect_vec_stmt!(stmts, LeafStmt)?,
                collect_vec_stmt!(stmts, LeafListStmt)?,
                collect_vec_stmt!(stmts, ListStmt)?,
                collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,
            )),
        ))
    }
}

///
/// The "deviation" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct DeviationStmt {
    /// "deviation-arg".
    arg: DeviationArg,

    /// "description" statement.
    description: Option<DescriptionStmt>,

    /// "reference" statement.
    reference: Option<ReferenceStmt>,

    /// "deviate" statement.
    deviate: Vec<DeviateStmt>,
}

impl Stmt for DeviationStmt {
    /// Arg type.
    type Arg = DeviationArg;

    /// Sub Statements.
    type SubStmts = (
        Option<DescriptionStmt>,
        Option<ReferenceStmt>,
        Vec<DeviateStmt>,
    );

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
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
            SubStmtDef::OneOrMore(SubStmtWith::Stmt(DeviateStmt::keyword)),
        ]
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> YangStmt
    where
        Self: Sized,
    {
        YangStmt::DeviationStmt(DeviationStmt {
            arg,
            description: substmts.0,
            reference: substmts.1,
            deviate: substmts.2,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

        Ok((
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?,
            collect_vec_stmt!(stmts, DeviateStmt)?,
        ))
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
    fn parse(parser: &mut Parser) -> Result<YangStmt, YangError>
    where
        Self::Arg: StmtArg,
        Self: Sized,
    {
        let arg = Self::Arg::parse_arg(parser)?;

        match &arg as &str {
            "add" => {
                let stmt = DeviateAddStmt::parse(parser)?;
                Ok(YangStmt::DeviateStmt(DeviateStmt::Add(stmt)))
            }
            "delete" => {
                let stmt = DeviateDeleteStmt::parse(parser)?;
                Ok(YangStmt::DeviateStmt(DeviateStmt::Delete(stmt)))
            }
            "replace" => {
                let stmt = DeviateReplaceStmt::parse(parser)?;
                Ok(YangStmt::DeviateStmt(DeviateStmt::Replace(stmt)))
            }
            "not-supported" => {
                let token = parser.get_token()?;
                match token {
                    Token::StatementEnd => Ok(YangStmt::DeviateStmt(DeviateStmt::NotSupported)),
                    _ => Err(YangError::UnexpectedToken(format!(
                        "'{}' after 'not-supported'",
                        token.to_string()
                    ))),
                }
            }
            _ => Err(YangError::UnexpectedToken(arg.to_string())),
        }
    }
}

///
/// The "deviate add" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct DeviateAddStmt {
    /// "units" statement.
    units: Option<UnitsStmt>,

    /// "must" statement.
    must: Vec<MustStmt>,

    /// "unique" statement.
    unique: Vec<UniqueStmt>,

    /// "default" statement.
    default: Vec<DefaultStmt>,

    /// "config" statement.
    config: Option<ConfigStmt>,

    /// "mandatory" statement.
    mandatory: Option<MandatoryStmt>,

    /// "min-elements" statement.
    min_elements: Option<MinElementsStmt>,

    /// "max-elements" statement.
    max_elements: Option<MaxElementsStmt>,
}

impl Compound for DeviateAddStmt {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(UnitsStmt::keyword)),
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
        let token = parser.get_token()?;
        match token {
            Token::BlockBegin => {
                let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

                let token = parser.get_token()?;
                match token {
                    Token::BlockEnd => Ok(DeviateAddStmt {
                        units: collect_opt_stmt!(stmts, UnitsStmt)?,
                        must: collect_vec_stmt!(stmts, MustStmt)?,
                        unique: collect_vec_stmt!(stmts, UniqueStmt)?,
                        default: collect_vec_stmt!(stmts, DefaultStmt)?,
                        config: collect_opt_stmt!(stmts, ConfigStmt)?,
                        mandatory: collect_opt_stmt!(stmts, MandatoryStmt)?,
                        min_elements: collect_opt_stmt!(stmts, MinElementsStmt)?,
                        max_elements: collect_opt_stmt!(stmts, MaxElementsStmt)?,
                    }),
                    _ => Err(YangError::UnexpectedToken(token.to_string())),
                }
            }
            Token::StatementEnd => Ok(DeviateAddStmt {
                units: None,
                must: Vec::new(),
                unique: Vec::new(),
                default: Vec::new(),
                config: None,
                mandatory: None,
                min_elements: None,
                max_elements: None,
            }),
            _ => Err(YangError::UnexpectedToken(token.to_string())),
        }
    }
}

///
/// The "deviate delete" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct DeviateDeleteStmt {
    /// "units" statement.
    units: Option<UnitsStmt>,

    /// "must" statement.
    must: Vec<MustStmt>,

    /// "unique" statement.
    unique: Vec<UniqueStmt>,

    /// "default" statement.
    default: Vec<DefaultStmt>,
}

impl Compound for DeviateDeleteStmt {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(UnitsStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(MustStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(UniqueStmt::keyword)),
            SubStmtDef::ZeroOrMore(SubStmtWith::Stmt(DefaultStmt::keyword)),
        ]
    }
}

impl DeviateDeleteStmt {
    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<DeviateDeleteStmt, YangError> {
        let token = parser.get_token()?;
        match token {
            Token::BlockBegin => {
                let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;
                let token = parser.get_token()?;

                match token {
                    Token::BlockEnd => Ok(DeviateDeleteStmt {
                        units: collect_opt_stmt!(stmts, UnitsStmt)?,
                        must: collect_vec_stmt!(stmts, MustStmt)?,
                        unique: collect_vec_stmt!(stmts, UniqueStmt)?,
                        default: collect_vec_stmt!(stmts, DefaultStmt)?,
                    }),
                    _ => Err(YangError::UnexpectedToken(token.to_string())),
                }
            }
            Token::StatementEnd => Ok(DeviateDeleteStmt {
                units: None,
                must: Vec::new(),
                unique: Vec::new(),
                default: Vec::new(),
            }),
            _ => Err(YangError::UnexpectedToken(token.to_string())),
        }
    }
}

///
/// The "deviate replace" Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct DeviateReplaceStmt {
    /// "type" statement.
    type_: Option<TypeStmt>,

    /// "units" statement.
    units: Option<UnitsStmt>,

    /// "default" statement.
    default: Option<DefaultStmt>,

    /// "config" statement.
    config: Option<ConfigStmt>,

    /// "mandatory" statement.
    mandatory: Option<MandatoryStmt>,

    /// "min-elements" statement.
    min_elements: Option<MinElementsStmt>,

    /// "max-elements" statement.
    max_elements: Option<MaxElementsStmt>,
}

impl Compound for DeviateReplaceStmt {
    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(TypeStmt::keyword)),
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
        let token = parser.get_token()?;
        match token {
            Token::BlockBegin => {
                let mut stmts = SubStmtUtil::parse_substmts(parser, Self::substmts_def())?;

                let token = parser.get_token()?;
                match token {
                    Token::BlockEnd => Ok(DeviateReplaceStmt {
                        type_: collect_opt_stmt!(stmts, TypeStmt)?,
                        units: collect_opt_stmt!(stmts, UnitsStmt)?,
                        default: collect_opt_stmt!(stmts, DefaultStmt)?,
                        config: collect_opt_stmt!(stmts, ConfigStmt)?,
                        mandatory: collect_opt_stmt!(stmts, MandatoryStmt)?,
                        min_elements: collect_opt_stmt!(stmts, MinElementsStmt)?,
                        max_elements: collect_opt_stmt!(stmts, MaxElementsStmt)?,
                    }),
                    _ => Err(YangError::UnexpectedToken(token.to_string())),
                }
            }
            Token::StatementEnd => Ok(DeviateReplaceStmt {
                type_: None,
                units: None,
                default: None,
                config: None,
                mandatory: None,
                min_elements: None,
                max_elements: None,
            }),
            _ => Err(YangError::UnexpectedToken(token.to_string())),
        }
    }
}

///
/// Unknown Statement.
///
#[derive(Debug, Clone, PartialEq, Getters)]
pub struct UnknownStmt {
    /// Keyword
    keyword: UnknownStmtKeyword,

    /// Optional arg.
    arg: Option<String>,

    /// YANG statement.
    yang: Vec<YangStmt>,
}

impl UnknownStmt {
    /// Parse a statement and return the object wrapped in enum.
    pub fn parse(parser: &mut Parser, keyword: &str) -> Result<YangStmt, YangError>
    where
        Self: Sized,
    {
        let keyword = UnknownStmtKeyword::from_str(keyword)
            .map_err(|e| YangError::ArgumentParseError(e.str))?;

        let token = parser.get_token()?;
        let arg = match token {
            Token::Identifier(s) | Token::QuotedString(s) => Some(s),
            Token::StatementEnd => {
                parser.save_token(token);
                None
            }
            _ => return Err(YangError::UnexpectedToken(token.to_string())),
        };

        let token = parser.get_token()?;
        let mut yang = Vec::new();
        match token {
            Token::StatementEnd => {}
            Token::BlockBegin => loop {
                let token = parser.get_token()?;

                match token {
                    Token::BlockEnd => break,
                    Token::StatementEnd => {}
                    Token::QuotedString(ref keyword) | Token::Identifier(ref keyword) => {
                        let stmt = SubStmtUtil::call_stmt_parser(parser, keyword as &str)?;
                        yang.push(stmt);
                    }
                    Token::EndOfInput => return Err(YangError::UnexpectedEof),
                    _ => return Err(YangError::UnexpectedToken(token.to_string())),
                }
            },
            _ => return Err(YangError::UnexpectedToken(token.to_string())),
        }

        Ok(YangStmt::UnknownStmt(UnknownStmt { keyword, arg, yang }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    pub fn test_import_stmt() {
        // Assuming keyword is already parsed, and arg and body are given to stmt parser.
        let s = r#"openconfig-inet-types {
                       prefix oc-inet;
                       revision-date 2017-07-06;
                   }"#;

        let mut parser = Parser::new(s.to_string());
        match ImportStmt::parse(&mut parser) {
            Ok(yang) => match yang {
                YangStmt::ImportStmt(stmt) => {
                    assert_eq!(
                        stmt.arg(),
                        &Identifier::from_str("openconfig-inet-types").unwrap()
                    );
                    assert_eq!(
                        stmt.prefix(),
                        &PrefixStmt {
                            arg: Identifier::from_str("oc-inet").unwrap()
                        }
                    );
                    assert_eq!(
                        stmt.revision_date(),
                        &Some(RevisionDateStmt {
                            arg: DateArg::from_str("2017-07-06").unwrap()
                        })
                    );
                    assert_eq!(stmt.description(), &None);
                    assert_eq!(stmt.reference(), &None);
                }
                _ => panic!("Unexpected stmt {:?}", yang),
            },
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    #[test]
    pub fn test_include_stmt() {
        // Assuming keyword is already parsed, and arg and body are given to stmt parser.
        let s = r#"openconfig-inet-types {
                       prefix oc-inet;
                       revision-date 2017-07-06;
                   }"#;

        let mut parser = Parser::new(s.to_string());
        match IncludeStmt::parse(&mut parser) {
            Ok(yang) => panic!("{:?}", yang),
            Err(err) => assert_eq!(err.to_string(), "Unexpected token Identifier '\"prefix\"'"),
        }

        let s = r#"openconfig-inet-types {
                       revision-date 2017-07-06;
                   }"#;

        let mut parser = Parser::new(s.to_string());
        match IncludeStmt::parse(&mut parser) {
            Ok(yang) => match yang {
                YangStmt::IncludeStmt(stmt) => {
                    assert_eq!(
                        stmt.arg(),
                        &Identifier::from_str("openconfig-inet-types").unwrap()
                    );
                    assert_eq!(
                        stmt.revision_date(),
                        &Some(RevisionDateStmt {
                            arg: DateArg::from_str("2017-07-06").unwrap()
                        })
                    );
                    assert_eq!(stmt.description(), &None);
                    assert_eq!(stmt.reference(), &None);
                }
                _ => panic!("Unexpected stmt {:?}", yang),
            },
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    #[test]
    pub fn test_extension_stmt() {
        // Assuming keyword is already parsed, and arg and body are given to stmt parser.
        let s = r#"openconfig-version {
    argument "semver" {
      yin-element false;
    }
    description
      "The OpenConfig version number for the module. This is
      ...";
    }"#;

        let mut parser = Parser::new(s.to_string());
        match ExtensionStmt::parse(&mut parser) {
            Ok(yang) => match yang {
                YangStmt::ExtensionStmt(stmt) => {
                    assert_eq!(
                        stmt.arg(),
                        &Identifier::from_str("openconfig-version").unwrap()
                    );
                    match stmt.argument() {
                        Some(argument_stmt) => {
                            assert_eq!(
                                argument_stmt.arg(),
                                &Identifier::from_str("semver").unwrap()
                            );
                            match argument_stmt.yin_element() {
                                Some(yin_element_stmt) => {
                                    assert_eq!(
                                        yin_element_stmt.arg(),
                                        &YinElementArg::from_str("false").unwrap()
                                    );
                                }
                                None => panic!("No yin-element-stmt"),
                            }
                        }
                        None => panic!("No argument-stmt"),
                    }
                    assert_eq!(stmt.status(), &None);
                    match stmt.description() {
                        Some(description_stmt) => {
                            assert_eq!(
                                description_stmt.arg(),
                                "The OpenConfig version number for the module. This is\n..."
                            );
                        }
                        None => panic!("No description-stmt"),
                    }
                    assert_eq!(stmt.reference(), &None);
                }
                _ => panic!("Unexpected stmt {:?}", yang),
            },
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    #[test]
    pub fn test_identity_stmt() {
        // Assuming keyword is already parsed, and arg and body are given to stmt parser.
        let s = r#"SFP {
    base TRANSCEIVER_FORM_FACTOR_TYPE;
    description
      "Small form-factor pluggable transceiver supporting up to
      10 Gb/s signal";
    }"#;
        let mut parser = Parser::new(s.to_string());
        match IdentityStmt::parse(&mut parser) {
            Ok(yang) => match yang {
                YangStmt::IdentityStmt(stmt) => {
                    assert_eq!(stmt.arg(), &Identifier::from_str("SFP").unwrap());
                    assert_eq!(stmt.if_feature(), &vec![]);
                    assert_eq!(
                        stmt.base(),
                        &vec![BaseStmt {
                            arg: IdentifierRef::from_str("TRANSCEIVER_FORM_FACTOR_TYPE").unwrap()
                        }]
                    );
                    assert_eq!(stmt.status(), &None);
                    assert_eq!(stmt.description(), &Some(DescriptionStmt { arg: String::from("Small form-factor pluggable transceiver supporting up to\n10 Gb/s signal") }));
                    assert_eq!(stmt.reference(), &None);
                }
                _ => panic!("Unexpected stmt {:?}", yang),
            },
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    //    #[test]
    //    pub fn test_feature_stmt() {
    //        // TBD
    //    }

    #[test]
    pub fn test_typedef_stmt() {
        let s = r#"zero-based-counter32 {
    type yang:counter32;
    default "0";
    description
     "The zero-based-counter32 type represents a counter32
      that has the defined 'initial' value zero....";
    reference
      "RFC 4502: Remote Network Monitoring Management Information
                 Base Version 2";
    }"#;

        let mut parser = Parser::new(s.to_string());
        match TypedefStmt::parse(&mut parser) {
            Ok(yang) => match yang {
                YangStmt::TypedefStmt(stmt) => {
                    assert_eq!(
                        stmt.arg(),
                        &Identifier::from_str("zero-based-counter32").unwrap()
                    );
                    assert_eq!(
                        stmt.type_(),
                        &TypeStmt {
                            arg: IdentifierRef::from_str("yang:counter32").unwrap(),
                            type_body: None
                        }
                    );
                    assert_eq!(stmt.units(), &None);
                    assert_eq!(
                        stmt.default(),
                        &Some(DefaultStmt {
                            arg: String::from("0")
                        })
                    );
                    assert_eq!(stmt.description(), &Some(DescriptionStmt { arg: String::from("The zero-based-counter32 type represents a counter32\nthat has the defined 'initial' value zero....") }));
                    assert_eq!(stmt.reference(), &Some(ReferenceStmt { arg: String::from("RFC 4502: Remote Network Monitoring Management Information\n          Base Version 2") }));
                }
                _ => panic!("Unexpected stmt {:?}", yang),
            },
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    #[test]
    pub fn test_deviation_stmt() {
        // Assuming keyword is already parsed, and arg and body are given to stmt parser.

        // Deviation add.
        let s = r#""/oc-if:interfaces/oc-if:interface/oc-if:hold-time" +
            "/oc-if:config/oc-if:up" {
      deviate add;
  }"#;
        let mut parser = Parser::new(s.to_string());
        match DeviationStmt::parse(&mut parser) {
            Ok(yang) => match yang {
                YangStmt::DeviationStmt(stmt) => {
                    assert_eq!(stmt.arg(), &AbsoluteSchemaNodeid::from_str("/oc-if:interfaces/oc-if:interface/oc-if:hold-time/oc-if:config/oc-if:up").unwrap());
                    assert_eq!(stmt.description(), &None);
                    assert_eq!(stmt.reference(), &None);
                    assert_eq!(stmt.deviate().len(), 1);
                    let deviate_stmt = stmt.deviate().get(0).unwrap();
                    match deviate_stmt {
                        DeviateStmt::Add(deviate_add_stmt) => {
                            assert_eq!(deviate_add_stmt.units(), &None);
                            assert_eq!(deviate_add_stmt.must().len(), 0);
                            assert_eq!(deviate_add_stmt.unique().len(), 0);
                            assert_eq!(deviate_add_stmt.config(), &None);
                            assert_eq!(deviate_add_stmt.mandatory(), &None);
                            assert_eq!(deviate_add_stmt.min_elements(), &None);
                            assert_eq!(deviate_add_stmt.max_elements(), &None);
                        }
                        _ => panic!("Unexpected stmt {:?}", deviate_stmt),
                    }
                }
                _ => panic!("Unexpected stmt {:?}", yang),
            },
            Err(err) => panic!("{}", err.to_string()),
        }

        // Deviation delete.
        let s = r#""/oc-if:interfaces/oc-if:interface/oc-if:hold-time" +
            "/oc-if:config/oc-if:up" {
      deviate delete {
        default 0;
      }
      description
        "Hold-time 0 is not configurable on XE, use no dampening.";
  }"#;
        let mut parser = Parser::new(s.to_string());
        match DeviationStmt::parse(&mut parser) {
            Ok(yang) => match yang {
                YangStmt::DeviationStmt(stmt) => {
                    assert_eq!(stmt.arg(), &AbsoluteSchemaNodeid::from_str("/oc-if:interfaces/oc-if:interface/oc-if:hold-time/oc-if:config/oc-if:up").unwrap());
                    assert_eq!(
                        stmt.description(),
                        &Some(DescriptionStmt {
                            arg: String::from(
                                "Hold-time 0 is not configurable on XE, use no dampening."
                            )
                        })
                    );
                    assert_eq!(stmt.reference(), &None);
                    assert_eq!(stmt.deviate().len(), 1);
                    let deviate_stmt = stmt.deviate().get(0).unwrap();
                    match deviate_stmt {
                        DeviateStmt::Delete(deviate_delete_stmt) => {
                            assert_eq!(deviate_delete_stmt.units(), &None);
                            assert_eq!(deviate_delete_stmt.must().len(), 0);
                            assert_eq!(deviate_delete_stmt.unique().len(), 0);
                            assert_eq!(
                                deviate_delete_stmt.default(),
                                &vec![DefaultStmt {
                                    arg: String::from("0")
                                }]
                            );
                        }
                        _ => panic!("Unexpected stmt {:?}", deviate_stmt),
                    }
                }
                _ => panic!("Unexpected stmt {:?}", yang),
            },
            Err(err) => panic!("{}", err.to_string()),
        }

        // Deviation replace
        let s = r#""/oc-if:interfaces/oc-if:interface/oc-if:state" +
            "/oc-if:last-change" {
      deviate replace {
        type yang:date-and-time;
      }
      description
        "Change the type of the last-change flag to date-and-time";
  }"#;
        let mut parser = Parser::new(s.to_string());
        match DeviationStmt::parse(&mut parser) {
            Ok(yang) => match yang {
                YangStmt::DeviationStmt(stmt) => {
                    assert_eq!(
                        stmt.arg(),
                        &AbsoluteSchemaNodeid::from_str(
                            "/oc-if:interfaces/oc-if:interface/oc-if:state/oc-if:last-change"
                        )
                        .unwrap()
                    );
                    assert_eq!(
                        stmt.description(),
                        &Some(DescriptionStmt {
                            arg: String::from(
                                "Change the type of the last-change flag to date-and-time"
                            )
                        })
                    );
                    assert_eq!(stmt.reference(), &None);
                    assert_eq!(stmt.deviate().len(), 1);
                    let deviate_stmt = stmt.deviate().get(0).unwrap();
                    match deviate_stmt {
                        DeviateStmt::Replace(deviate_replace_stmt) => {
                            assert_eq!(
                                deviate_replace_stmt.type_(),
                                &Some(TypeStmt {
                                    arg: IdentifierRef::from_str("yang:date-and-time").unwrap(),
                                    type_body: None
                                })
                            );
                            assert_eq!(deviate_replace_stmt.units(), &None);
                            assert_eq!(deviate_replace_stmt.default(), &None);
                            assert_eq!(deviate_replace_stmt.config(), &None);
                            assert_eq!(deviate_replace_stmt.mandatory(), &None);
                            assert_eq!(deviate_replace_stmt.min_elements(), &None);
                            assert_eq!(deviate_replace_stmt.max_elements(), &None);
                        }
                        _ => panic!("Unexpected stmt {:?}", deviate_stmt),
                    }
                }
                _ => panic!("Unexpected stmt {:?}", yang),
            },
            Err(err) => panic!("{}", err.to_string()),
        }

        // Deviation not-supported
        let s = r#""/oc-if:interfaces/oc-if:interface/oc-vlan:routed-vlan" +
            "/oc-ip:ipv4/oc-ip:addresses/oc-ip:address/oc-ip:vrrp" {
      deviate not-supported;
      description
        "IPv4 VRRP not supported in 16.6.1.";
  }"#;
        let mut parser = Parser::new(s.to_string());
        match DeviationStmt::parse(&mut parser) {
            Ok(yang) => match yang {
                YangStmt::DeviationStmt(stmt) => {
                    assert_eq!(stmt.arg(), &AbsoluteSchemaNodeid::from_str("/oc-if:interfaces/oc-if:interface/oc-vlan:routed-vlan/oc-ip:ipv4/oc-ip:addresses/oc-ip:address/oc-ip:vrrp").unwrap());
                    assert_eq!(
                        stmt.description(),
                        &Some(DescriptionStmt {
                            arg: String::from("IPv4 VRRP not supported in 16.6.1.")
                        })
                    );
                    assert_eq!(stmt.reference(), &None);
                    assert_eq!(stmt.deviate().len(), 1);
                    let deviate_stmt = stmt.deviate().get(0).unwrap();
                    match deviate_stmt {
                        DeviateStmt::NotSupported => {}
                        _ => panic!("Unexpected stmt {:?}", deviate_stmt),
                    }
                }
                _ => panic!("Unexpected stmt {:?}", yang),
            },
            Err(err) => panic!("{}", err.to_string()),
        }
    }
}
