//
// YANG - YANG struct
//  Copyright (C) 2021 Toshiaki Takada
//

use std::collections::HashMap;

use url::Url;

use super::core::*;
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

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(_arg: Self::Arg) -> StmtType where Self: Sized {
        panic!();
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(_arg: Self::Arg, _substmts: Self::SubStmts) -> StmtType where Self: Sized {
        panic!();
    }

    // Parse a statement arg.
//    fn parse_arg(parser: &mut Parser) -> Result<Self::Arg, YangError> where Self::Arg: StmtArg {
//        Self::Arg::parse_arg(parser)
//    }

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

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ModuleStmt(ModuleStmt {
            identifier_arg: arg,
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

    /// Return true if this statement has substatements.
    fn has_substmts() -> bool {
        true
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::SubmoduleStmt(SubmoduleStmt {
            identifier_arg: arg,
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
    yang_version_arg: YangVersionArg,
}

impl Stmt for YangVersionStmt {
    /// Arg type.
    type Arg = YangVersionArg;

    /// Sub Statements.
    type SubStmts = ();

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
            revision_date: substmts.1,
            description: substmts.2,
            reference: substmts.3,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("prefix", Repeat::new(Some(1), Some(1))),
            ("revision-date", Repeat::new(Some(0), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

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
    identifier_arg: Identifier,
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
            revision_date: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::IncludeStmt(IncludeStmt {
            identifier_arg: arg,
            revision_date: substmts.0,
            description: substmts.1,
            reference: substmts.2,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("revision-date", Repeat::new(Some(0), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

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
    uri_str: Url,
}

impl Stmt for NamespaceStmt {
    /// Arg type.
    type Arg = Url;

    /// Sub Statements.
    type SubStmts = ();

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
    type SubStmts = ();

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
    type SubStmts = ();

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
    type SubStmts = ();

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
    type SubStmts = ();

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
    type SubStmts = ();

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
    type SubStmts = ();

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
}

///
///
///
#[derive(Debug, Clone)]
pub struct RevisionDateStmt {
    revision_date: DateArg,
}

impl Stmt for RevisionDateStmt {
    /// Arg type.
    type Arg = DateArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "revision-date"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::RevisionDateStmt(RevisionDateStmt { revision_date: arg })
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ExtensionStmt {
    identifier_arg: Identifier,
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
    fn keyword() -> &'static str {
        "extension"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ExtensionStmt(ExtensionStmt {
            identifier_arg: arg,
            argument: None,
            status: None,
            description: None,
            reference: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ExtensionStmt(ExtensionStmt {
            identifier_arg: arg,
            argument: substmts.0,
            status: substmts.1,
            description: substmts.2,
            reference: substmts.3,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("argument", Repeat::new(Some(0), Some(1))),
            ("status", Repeat::new(Some(0), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok((collect_opt_stmt!(stmts, ArgumentStmt)?,
            collect_opt_stmt!(stmts, StatusStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ArgumentStmt {
    identifier_arg: Identifier,
    yin_element: Option<YinElementStmt>,
}

impl Stmt for ArgumentStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Sub Statements.
    type SubStmts = (Option<YinElementStmt>,);

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "argument"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ArgumentStmt(ArgumentStmt {
            identifier_arg: arg,
            yin_element: None,
        })
    }

    /// Constructor with tuple of substatements. Panic if it is not defined.
    fn new_with_substmts(arg: Self::Arg, substmts: Self::SubStmts) -> StmtType where Self: Sized {
        StmtType::ArgumentStmt(ArgumentStmt {
            identifier_arg: arg,
            yin_element: substmts.0,
        })
    }

    /// Parse substatements.
    fn parse_substmts(parser: &mut Parser) -> Result<Self::SubStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("yin-element", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok((collect_opt_stmt!(stmts, YinElementStmt)?,))
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct YinElementStmt {
    yin_element_arg: YinElementArg,
}

impl Stmt for YinElementStmt {
    /// Arg type.
    type Arg = YinElementArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "yin-element"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::YinElementStmt(YinElementStmt {
            yin_element_arg: arg,
        })
    }
}


/*  XXXX TBD

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

*/

///
///
///
#[derive(Debug, Clone)]
pub struct FractionDigitsStmt {
    fraction_digits_arg: FractionDigitsArg,
}

impl Stmt for FractionDigitsStmt {
    /// Arg type.
    type Arg = FractionDigitsArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "fraction-digits"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::FractionDigitsStmt(FractionDigitsStmt {
            fraction_digits_arg: arg,
        })
    }
}

/*

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

*/


///
///
///
#[derive(Debug, Clone)]
pub struct StatusStmt {
    status_arg: StatusArg,
}

impl Stmt for StatusStmt {
    /// Arg type.
    type Arg = StatusArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "status"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::StatusStmt(StatusStmt {
            status_arg: arg
        })
    }
}


///
///
///
#[derive(Debug, Clone)]
pub struct ConfigStmt {
    config_arg: ConfigArg,
}

impl Stmt for ConfigStmt {
    /// Arg type.
    type Arg = ConfigArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "config"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ConfigStmt(ConfigStmt {
            config_arg: arg
        })
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct MandatoryStmt {
    mandatory_arg: MandatoryArg,
}

impl Stmt for MandatoryStmt {
    /// Arg type.
    type Arg = MandatoryArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "mandatory"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::MandatoryStmt(MandatoryStmt {
            mandatory_arg: arg
        })
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct PresenseStmt {
    str: String,
}

impl Stmt for PresenseStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "presense"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::PresenseStmt(PresenseStmt {
            str: arg,
        })
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct OrderedByStmt {
    ordered_by_arg: OrderedByArg,
}

impl Stmt for OrderedByStmt {
    /// Arg type.
    type Arg = OrderedByArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "ordered-by"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::OrderedByStmt(OrderedByStmt {
            ordered_by_arg: arg,
        })
    }
}

///
///
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
    fn keyword() -> &'static str {
        "must"
    }

    /// Return true if this statement has sub-statements optionally.
    fn opt_substmts() -> bool {
        true
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
        let map: HashMap<&'static str, Repeat> = [
            ("error-message", Repeat::new(Some(0), Some(1))),
            ("error-app-tag", Repeat::new(Some(0), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;

        Ok((collect_opt_stmt!(stmts, ErrorMessageStmt)?,
            collect_opt_stmt!(stmts, ErrorAppTagStmt)?,
            collect_opt_stmt!(stmts, DescriptionStmt)?,
            collect_opt_stmt!(stmts, ReferenceStmt)?))
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ErrorMessageStmt {
    str: String,
}

impl Stmt for ErrorMessageStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "error-message"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ErrorMessageStmt(ErrorMessageStmt {
            str: arg,
        })
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct ErrorAppTagStmt {
    str: String,
}

impl Stmt for ErrorAppTagStmt {
    /// Arg type.
    type Arg = String;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "error-app-tag"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ErrorAppTagStmt(ErrorAppTagStmt {
            str: arg,
        })
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct MinElementsStmt {
    min_value_arg: MinValueArg,
}

impl Stmt for MinElementsStmt {
    /// Arg type.
    type Arg = MinValueArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "min-elements"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::MinElementsStmt(MinElementsStmt { min_value_arg: arg })
    }
}

///
///
///
#[derive(Debug, Clone)]
pub struct MaxElementsStmt {
    max_value_arg: MaxValueArg,
}

impl Stmt for MaxElementsStmt {
    /// Arg type.
    type Arg = MaxValueArg;

    /// Sub Statements.
    type SubStmts = ();

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "max-elements"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::MaxElementsStmt(MaxElementsStmt { max_value_arg: arg })
    }
}

///
///
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
    fn keyword() -> &'static str {
        "value"
    }

    /// Constructor with a single arg. Panic if it is not defined.
    fn new_with_arg(arg: Self::Arg) -> StmtType where Self: Sized {
        StmtType::ValueStmt(ValueStmt { arg })
    }
}


/*

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
