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

    /// Return statement keyword in &str.
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
        panic!();
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(_arg: Self::Arg) -> StmtType where Self: Sized {
        panic!();
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_arg: Self::Arg, _substmts: Self::SubStmts) -> StmtType where Self: Sized {
        panic!();
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
    arg: Identifier,

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
//            body: substmts.4,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let module_header = ModuleHeaderStmts::parse(parser)?;
        let linkage = LinkageStmts::parse(parser)?;
        let meta = MetaStmts::parse(parser)?;
        let revision = RevisionStmts::parse(parser)?;

        Ok((module_header, linkage, meta, revision))
    }
}

///
/// 7.2. The "submodule" Statement.
///
#[derive(Debug, Clone)]
pub struct SubmoduleStmt {
    /// Submodule identifier.
    arg: Identifier,

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
//            body: substmts.4,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let submodule_header = SubmoduleHeaderStmts::parse(parser)?;
        let linkage = LinkageStmts::parse(parser)?;
        let meta = MetaStmts::parse(parser)?;
        let revision = RevisionStmts::parse(parser)?;

        Ok((submodule_header, linkage, meta, revision))
    }
}

///
/// 7.1.2. The "yang-version" Statement.
///
#[derive(Debug, Clone)]
pub struct YangVersionStmt {
    arg: YangVersionArg,
}

impl Stmt for YangVersionStmt {
    /// Arg type.
    type Arg = YangVersionArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.1.5. The "import" Statement.
///
#[derive(Debug, Clone)]
pub struct ImportStmt {
    arg: Identifier,
    prefix: PrefixStmt,
    revision_date: Option<RevisionDateStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for ImportStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (PrefixStmt, Option<RevisionDateStmt>,Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 7.1.6. The "include" Statement.
///
#[derive(Debug, Clone)]
pub struct IncludeStmt {
    arg: Identifier,
    revision_date: Option<RevisionDateStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for IncludeStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<RevisionDateStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 7.1.3. The "namespace" Statement.
///
#[derive(Debug, Clone)]
pub struct NamespaceStmt {
    arg: Url,
}

impl Stmt for NamespaceStmt {
    /// Arg type.
    type Arg = Url;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "namespace"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::NamespaceStmt(NamespaceStmt { arg })
    }
}

///
/// 7.1.4. The "prefix" Statement.
///
#[derive(Debug, Clone)]
pub struct PrefixStmt {
    arg: Identifier,
}

impl Stmt for PrefixStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "prefix"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::PrefixStmt(PrefixStmt { arg })
    }
}

///
/// 7.2.2. The "belongs-to" Statement.
///
#[derive(Debug, Clone)]
pub struct BelongsToStmt {
    /// Identifier.
    arg: Identifier,

    /// Prefix statement.
    prefix: PrefixStmt,
}

impl Stmt for BelongsToStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (PrefixStmt,);

    /// Return statement keyword in &str.
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
/// 7.1.7. The "Organization" Statement.
///
#[derive(Debug, Clone)]
pub struct OrganizationStmt {
    arg: String,
}

impl Stmt for OrganizationStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "organization"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::OrganizationStmt(OrganizationStmt { arg })
    }
}

///
/// 7.1.8. The "contact" Statement.
///
#[derive(Debug, Clone)]
pub struct ContactStmt {
    arg: String,
}

impl Stmt for ContactStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "contact"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ContactStmt(ContactStmt { arg })
    }
}

///
/// 7.21.3. The "description" Statement.
/// 
#[derive(Debug, Clone)]
pub struct DescriptionStmt {
    arg: String,
}

impl Stmt for DescriptionStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "description"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::DescriptionStmt(DescriptionStmt { arg })
    }
}

///
/// 7.21.4. The "reference" Statement.
/// 
#[derive(Debug, Clone)]
pub struct ReferenceStmt {
    arg: String,
}

impl Stmt for ReferenceStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "reference"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ReferenceStmt(ReferenceStmt { arg })
    }
}

///
/// 7.3.3. The "units" Statement.
///
#[derive(Debug, Clone)]
pub struct UnitsStmt {
    arg: String,
}

impl Stmt for UnitsStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "units"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::UnitsStmt(UnitsStmt { arg })
    }
}

///
/// 7.1.9. The "revision" Statement.
///
#[derive(Debug, Clone)]
pub struct RevisionStmt {
    /// Revision date.
    arg: DateArg,

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
#[derive(Debug, Clone)]
pub struct RevisionDateStmt {
    arg: DateArg,
}

impl Stmt for RevisionDateStmt {
    /// Arg type.
    type Arg = DateArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "revision-date"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::RevisionDateStmt(RevisionDateStmt { arg })
    }
}

///
/// 7.19. The "extension" Statement.
///
#[derive(Debug, Clone)]
pub struct ExtensionStmt {
    arg: Identifier,
    argument: Option<ArgumentStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for ExtensionStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<ArgumentStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 7.19.2. The "argument" Statement.
///
#[derive(Debug, Clone)]
pub struct ArgumentStmt {
    arg: Identifier,
    yin_element: Option<YinElementStmt>,
}

impl Stmt for ArgumentStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<YinElementStmt>,);

    /// Return statement keyword in &str.
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
/// 7.19.2.2. The "yin-element" Statement.
///
#[derive(Debug, Clone)]
pub struct YinElementStmt {
    arg: YinElementArg,
}

impl Stmt for YinElementStmt {
    /// Arg type.
    type Arg = YinElementArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.18. The "identity" Statement.
///
#[derive(Debug, Clone)]
pub struct IdentityStmt {
    arg: Identifier,
    if_feature: Vec<IfFeatureStmt>,
    base: Vec<BaseStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for IdentityStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Vec<BaseStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 7.18.2. The "base" Statement.
///
#[derive(Debug, Clone)]
pub struct BaseStmt {
    arg: IdentifierRef,
}

impl Stmt for BaseStmt {
    /// Arg type.
    type Arg = IdentifierRef;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.20.1. The "feature" Statement.
///
#[derive(Debug, Clone)]
pub struct FeatureStmt {
    arg: Identifier,
    if_feature: Vec<IfFeatureStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for FeatureStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 7.20.2. The "if-feature" Statement.
///
#[derive(Debug, Clone)]
pub struct IfFeatureStmt {
    arg: IfFeatureExpr,
}

impl Stmt for IfFeatureStmt {
    /// Arg type.
    type Arg = IfFeatureExpr;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "if-feature"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::IfFeatureStmt(IfFeatureStmt { arg })
    }
}

///
/// 7.3. The "typedef" Statement.
///
#[derive(Debug, Clone)]
pub struct TypedefStmt {
    arg: Identifier,
    type_: TypeStmt,
    units: Option<UnitsStmt>,
    default: Option<DefaultStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for TypedefStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (TypeStmt, Option<UnitsStmt>, Option<DefaultStmt>,
                     Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 7.4. The "type" Statement.
///
#[derive(Debug, Clone)]
pub struct TypeStmt {
    arg: IdentifierRef,
    type_body: Option<TypeBodyStmts>,
}

impl Stmt for TypeStmt {
    /// Arg type.
    type Arg = IdentifierRef;

    /// Sub Statements.
    type SubStmts = Option<TypeBodyStmts>;

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "type"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        Ok(Some(TypeBodyStmts::parse(parser)?))
    }
}

///
/// 9.2.4. The "range" Statement.
///
#[derive(Debug, Clone)]
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

    /// Return statement keyword in &str.
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
/// 9.3.4. The "fraction-digits" Statement.
///
#[derive(Debug, Clone)]
pub struct FractionDigitsStmt {
    arg: FractionDigitsArg,
}

impl Stmt for FractionDigitsStmt {
    /// Arg type.
    type Arg = FractionDigitsArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 9.4.4. The "length" Statement.
///
#[derive(Debug, Clone)]
pub struct LengthStmt {
    arg: LengthArg,
    error_message: Option<ErrorMessageStmt>,
    error_app_tag: Option<ErrorAppTagStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for LengthStmt {
    /// Arg type.
    type Arg = LengthArg;

    /// Sub Statements.
    type SubStmts = (Option<ErrorMessageStmt>, Option<ErrorAppTagStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 9.4.6. The "pattern" Statement.
///
#[derive(Debug, Clone)]
pub struct PatternStmt {
    arg: String,
    modifier: Option<ModifierStmt>,
    error_message: Option<ErrorMessageStmt>,
    error_app_tag: Option<ErrorAppTagStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for PatternStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (Option<ModifierStmt>, Option<ErrorMessageStmt>, Option<ErrorAppTagStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 9.4.6. The "modifier" Statement.
///
#[derive(Debug, Clone)]
pub struct ModifierStmt {
    arg: ModifierArg,
}

impl Stmt for ModifierStmt {
    /// Arg type.
    type Arg = ModifierArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
#[derive(Debug, Clone)]
pub struct DefaultStmt {
    arg: String,
}

impl Stmt for DefaultStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 9.6.4. The "enum" Statement.
///
#[derive(Debug, Clone)]
pub struct EnumStmt {
    arg: String,
    if_feature: Vec<IfFeatureStmt>,
    value: Option<ValueStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for EnumStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Option<ValueStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 9.9.2. The "path" Statement.
///
#[derive(Debug, Clone)]
pub struct PathStmt {
    arg: PathArg,
}

impl Stmt for PathStmt {
    /// Arg type.
    type Arg = PathArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 9.9.3. The "require-instance" Statement.
///
#[derive(Debug, Clone)]
pub struct RequireInstanceStmt {
    arg: RequireInstanceArg,
}

impl Stmt for RequireInstanceStmt {
    /// Arg type.
    type Arg = RequireInstanceArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 9.7.4. The "bit" Statement.
///
#[derive(Debug, Clone)]
pub struct BitStmt {
    arg: Identifier,
    if_feature: Vec<IfFeatureStmt>,
    position: Option<PositionStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for BitStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Option<PositionStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 9.7.4.2. The "position" Statement.
///
#[derive(Debug, Clone)]
pub struct PositionStmt {
    arg: PositionValueArg,
}

impl Stmt for PositionStmt {
    /// Arg type.
    type Arg = PositionValueArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.21.2. The "status" Statement.
///
#[derive(Debug, Clone)]
pub struct StatusStmt {
    arg: StatusArg,
}

impl Stmt for StatusStmt {
    /// Arg type.
    type Arg = StatusArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.21.1. The "config" Statement.
///
#[derive(Debug, Clone)]
pub struct ConfigStmt {
    arg: ConfigArg,
}

impl Stmt for ConfigStmt {
    /// Arg type.
    type Arg = ConfigArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
#[derive(Debug, Clone)]
pub struct MandatoryStmt {
    arg: MandatoryArg,
}

impl Stmt for MandatoryStmt {
    /// Arg type.
    type Arg = MandatoryArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.5.5. The "presence" Statement.
///
#[derive(Debug, Clone)]
pub struct PresenceStmt {
    arg: String,
}

impl Stmt for PresenceStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.7.7. The "ordered-by" Statement.
///
#[derive(Debug, Clone)]
pub struct OrderedByStmt {
    arg: OrderedByArg,
}

impl Stmt for OrderedByStmt {
    /// Arg type.
    type Arg = OrderedByArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.5.3. The "must" Statement.
///
#[derive(Debug, Clone)]
pub struct MustStmt {
    arg: String,
    error_message: Option<ErrorMessageStmt>,
    error_app_tag: Option<ErrorAppTagStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for MustStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (Option<ErrorMessageStmt>, Option<ErrorAppTagStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 7.5.4.1. The "error-message" Statement.
///
#[derive(Debug, Clone)]
pub struct ErrorMessageStmt {
    arg: String,
}

impl Stmt for ErrorMessageStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.5.4.2. The "error-app-tag" Statement.
///
#[derive(Debug, Clone)]
pub struct ErrorAppTagStmt {
    arg: String,
}

impl Stmt for ErrorAppTagStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.7.5. The "min-elements" Statement.
///
#[derive(Debug, Clone)]
pub struct MinElementsStmt {
    arg: MinValueArg,
}

impl Stmt for MinElementsStmt {
    /// Arg type.
    type Arg = MinValueArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "min-elements"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::MinElementsStmt(MinElementsStmt { arg })
    }
}

///
/// 7.7.6. The "max-elements" Statement.
///
#[derive(Debug, Clone)]
pub struct MaxElementsStmt {
    arg: MaxValueArg,
}

impl Stmt for MaxElementsStmt {
    /// Arg type.
    type Arg = MaxValueArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "max-elements"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::MaxElementsStmt(MaxElementsStmt { arg })
    }
}

///
/// 9.6.4.2. The "value" Statement.
///
#[derive(Debug, Clone)]
pub struct ValueStmt {
    arg: IntegerValue,
}

impl Stmt for ValueStmt {
    /// Arg type.
    type Arg = IntegerValue;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "value"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ValueStmt(ValueStmt { arg })
    }
}

///
/// 7.12. The "grouping" Statement.
///
#[derive(Debug, Clone)]
pub struct GroupingStmt {
    arg: Identifier,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
    typedef_or_grouping: TypedefOrGrouping,
    data_def: DataDefStmt,
    action: Vec<ActionStmt>,
    notification: Vec<NotificationStmt>,
}

impl Stmt for GroupingStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<StatusStmt>,Option<DescriptionStmt>, Option<ReferenceStmt>,
                     TypedefOrGrouping, DataDefStmt, Vec<ActionStmt>, Vec<NotificationStmt>);

    /// Return statement keyword in &str.
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
                // collect_vec_stmt!(stmts, ContainerStmt)?,
                // collect_vec_stmt!(stmts, LeafStmt)?,
                // collect_vec_stmt!(stmts, LeafListStmt)?,
                // collect_vec_stmt!(stmts, ListStmt)?,
                // collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
            collect_vec_stmt!(stmts, ActionStmt)?,
            collect_vec_stmt!(stmts, NotificationStmt)?,
        ))
    }
}


/*

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
    fn keyword() -> Keyword {
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
    fn keyword() -> Keyword {
        "leaf"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

*/

///
/// 7.7. The "leaf-list" Statement.
///
#[derive(Debug, Clone)]
pub struct LeafListStmt {
    arg: Identifier,
    when: Option<WhenStmt>,
    if_feature: Vec<IfFeatureStmt>,
    type_: TypeStmt,
    units: Option<UnitsStmt>,
    must: Vec<MustStmt>,
    default: Vec<DefaultStmt>,
    config: Option<ConfigStmt>,
    min_elements: Option<MinElementsStmt>,
    max_elements: Option<MaxElementsStmt>,
    ordered_by: Option<OrderedByStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for LeafListStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, TypeStmt, Option<UnitsStmt>, Vec<MustStmt>,
                     Vec<DefaultStmt>, Option<ConfigStmt>, Option<MinElementsStmt>, Option<MaxElementsStmt>,
                     Option<OrderedByStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 7.8. The "list" Statement.
///
#[derive(Debug, Clone)]
pub struct ListStmt {
    arg: Identifier,
    when: Option<WhenStmt>,
    if_feature: Vec<IfFeatureStmt>,
    must: Vec<MustStmt>,
    key: Option<KeyStmt>,
    unique: Vec<UniqueStmt>,
    config: Option<ConfigStmt>,
    min_elements: Option<MinElementsStmt>,
    max_elements: Option<MaxElementsStmt>,
    ordered_by: Option<OrderedByStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
    typedef_or_grouping: TypedefOrGrouping,
    data_def: DataDefStmt,
    action: Vec<ActionStmt>,
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

    /// Return statement keyword in &str.
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
                // collect_vec_stmt!(stmts, ContainerStmt)?,
                // collect_vec_stmt!(stmts, LeafStmt)?,
                // collect_vec_stmt!(stmts, LeafListStmt)?,
                // collect_vec_stmt!(stmts, ListStmt)?,
                // collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
            collect_vec_stmt!(stmts, ActionStmt)?,
            collect_vec_stmt!(stmts, NotificationStmt)?,
        ))
    }
}

///
/// 7.8.2. The list's "key" Statement.
///
#[derive(Debug, Clone)]
pub struct KeyStmt {
    arg: KeyArg,
}

impl Stmt for KeyStmt {
    /// Arg type.
    type Arg = KeyArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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
/// 7.8.3. The list's "unique" Statement.
///
#[derive(Debug, Clone)]
pub struct UniqueStmt {
    arg: UniqueArg,
}

impl Stmt for UniqueStmt {
    /// Arg type.
    type Arg = UniqueArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
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

/*

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
    fn keyword() -> Keyword {
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
    fn keyword() -> Keyword {
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
    fn keyword() -> Keyword {
        "case"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

*/

///
/// 7.10. The "anydata" Statement.
///
#[derive(Debug, Clone)]
pub struct AnydataStmt {
    arg: Identifier,
    when: Option<WhenStmt>,
    if_feature: Vec<IfFeatureStmt>,
    must: Vec<MustStmt>,
    config: Option<ConfigStmt>,
    mandatory: Option<MandatoryStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for AnydataStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Vec<MustStmt>, Option<ConfigStmt>,
                     Option<MandatoryStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 7.11. The "anyxml" Statement.
///
#[derive(Debug, Clone)]
pub struct AnyxmlStmt {
    arg: Identifier,
    when: Option<WhenStmt>,
    if_feature: Vec<IfFeatureStmt>,
    must: Vec<MustStmt>,
    config: Option<ConfigStmt>,
    mandatory: Option<MandatoryStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for AnyxmlStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Vec<MustStmt>, Option<ConfigStmt>,
                     Option<MandatoryStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
/// 7.13. The "uses" Statement.
///
#[derive(Debug, Clone)]
pub struct UsesStmt {
    arg: IdentifierRef,
    when: Option<WhenStmt>,
    if_feature: Vec<IfFeatureStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
    refine: Vec<RefineStmt>,
    uses_augment: Vec<UsesAugmentStmt>,
}

impl Stmt for UsesStmt {
    /// Arg type.
    type Arg = IdentifierRef;

    /// Sub Statements.
    type SubStmts = (Option<WhenStmt>, Vec<IfFeatureStmt>, Option<StatusStmt>, Option<DescriptionStmt>,
                     Option<ReferenceStmt>, Vec<RefineStmt>, Vec<UsesAugmentStmt>);

    /// Return statement keyword in &str.
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
/// 7.13.2. The "refine" Statement.
///
#[derive(Debug, Clone)]
pub struct RefineStmt {
    arg: String,
//    arg: RefineArg,
    if_feature: Vec<IfFeatureStmt>,
    must: Vec<MustStmt>,
    presence: Option<PresenceStmt>,
    default: Vec<DefaultStmt>,
    config: Option<ConfigStmt>,
    mandatory: Option<MandatoryStmt>,
    min_elements: Option<MinElementsStmt>,
    max_elements: Option<MaxElementsStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for RefineStmt {
    /// Arg type.
    type Arg = String;//RefineArg;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Vec<MustStmt>, Option<PresenceStmt>,
                     Vec<DefaultStmt>, Option<ConfigStmt>, Option<MandatoryStmt>,
                     Option<MinElementsStmt>, Option<MaxElementsStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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
///
///
#[derive(Debug, Clone)]
pub struct UsesAugmentStmt {
    arg: String,
}

impl Stmt for UsesAugmentStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> Keyword {
        "uses-augment"
    }
}

/*

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
    fn keyword() -> Keyword {
        "augment"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

*/
///
///
///
#[derive(Debug, Clone)]
pub struct WhenStmt {
    arg: String,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for WhenStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = (Option<DescriptionStmt>, Option<ReferenceStmt>);

    /// Return statement keyword in &str.
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

/*

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
    fn keyword() -> Keyword {
        "rpc"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

*/

///
/// 7.15. The "action" Statement.
///
#[derive(Debug, Clone)]
pub struct ActionStmt {
    arg: Identifier,
    if_feature: Vec<IfFeatureStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
    typedef_or_grouping: TypedefOrGrouping,
    input: Option<InputStmt>,
    output: Option<OutputStmt>,
}

impl Stmt for ActionStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Option<StatusStmt>, Option<DescriptionStmt>, Option<ReferenceStmt>,
                     TypedefOrGrouping, Option<InputStmt>, Option<OutputStmt>,
    );

    /// Return statement keyword in &str.
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
             SubStmtDef::Optional(SubStmtWith::Selection(TypedefOrGrouping::keywords)),
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
/// 7.14.2. The "input" Statement.
///
#[derive(Debug, Clone)]
pub struct InputStmt {
    must: Vec<MustStmt>,
    typedef_or_grouping: TypedefOrGrouping,
    data_def: DataDefStmt,
}

impl Stmt for InputStmt {
    /// Arg type.
    type Arg = NoArg;

    /// Sub Statements.
    type SubStmts = (Vec<MustStmt>, TypedefOrGrouping, DataDefStmt);

    /// Return statement keyword in &str.
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
                // collect_vec_stmt!(stmts, ContainerStmt)?,
                // collect_vec_stmt!(stmts, LeafStmt)?,
                // collect_vec_stmt!(stmts, LeafListStmt)?,
                // collect_vec_stmt!(stmts, ListStmt)?,
                // collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
        ))
    }
}

///
/// 7.14.3. The "output" Statement.
///
#[derive(Debug, Clone)]
pub struct OutputStmt {
    must: Vec<MustStmt>,
    typedef_or_grouping: TypedefOrGrouping,
    data_def: DataDefStmt,
}

impl Stmt for OutputStmt {
    /// Arg type.
    type Arg = NoArg;

    /// Sub Statements.
    type SubStmts = (Vec<MustStmt>, TypedefOrGrouping, DataDefStmt);

    /// Return statement keyword in &str.
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
                // collect_vec_stmt!(stmts, ContainerStmt)?,
                // collect_vec_stmt!(stmts, LeafStmt)?,
                // collect_vec_stmt!(stmts, LeafListStmt)?,
                // collect_vec_stmt!(stmts, ListStmt)?,
                // collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
        ))
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct NotificationStmt {
    arg: Identifier,
    if_feature: Vec<IfFeatureStmt>,
    must: Vec<MustStmt>,
    status: Option<StatusStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
    typedef_or_grouping: TypedefOrGrouping,
    data_def: DataDefStmt,
}

impl Stmt for NotificationStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Vec<IfFeatureStmt>, Vec<MustStmt>, Option<StatusStmt>, Option<DescriptionStmt>,
                     Option<ReferenceStmt>, TypedefOrGrouping, DataDefStmt);

    /// Return statement keyword in &str.
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
                // collect_vec_stmt!(stmts, ContainerStmt)?,
                // collect_vec_stmt!(stmts, LeafStmt)?,
                // collect_vec_stmt!(stmts, LeafListStmt)?,
                // collect_vec_stmt!(stmts, ListStmt)?,
                // collect_vec_stmt!(stmts, ChoiceStmt)?,
                collect_vec_stmt!(stmts, AnydataStmt)?,
                collect_vec_stmt!(stmts, AnyxmlStmt)?,
                collect_vec_stmt!(stmts, UsesStmt)?,)),
        ))
    }
}

/*

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
    fn keyword() -> Keyword {
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
    fn keyword() -> Keyword {
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
    fn keyword() -> Keyword {
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
    fn keyword() -> Keyword {
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
    fn keyword() -> Keyword {
        "deviate-replace"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        Err(YangError::PlaceHolder)
    }
}

TBD */


