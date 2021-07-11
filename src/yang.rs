//
// YANG - YANG struct
//  Copyright (C) 2021 Toshiaki Takada
//

use std::fmt;
use std::any::Any;
use std::collections::HashMap;
use super::error::*;
use super::parser::*;

#[macro_use]
use crate::collect_a_stmt;
use crate::collect_vec_stmt;
use crate::collect_opt_stmt;

pub type StmtCollection = HashMap<String, Vec<StmtType>>;

/// Get a statement arg.
fn parse_arg(parser: &mut Parser) -> Result<String, YangError> {
    let (token, _) = parser.get_token()?;
    match token {
        // Statement argument.
        Token::Identifier(s) => Ok(s),
        Token::QuotedString(s) => Ok(s),
        Token::EndOfInput => Err(YangError::UnexpectedEof),
        // Unexpected Token.
        _ => Err(YangError::UnexpectedToken(parser.line())),
    }
}

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

/// Get a list of statements in any order.
pub fn parse_stmts(parser: &mut Parser, map: HashMap<&'static str, Repeat>) -> Result<StmtCollection, YangError> {
    let mut stmts: StmtCollection = HashMap::new();

    loop {
        let (token, pos) = parser.get_token()?;
println!("*** parse_stmts {:?}", token);
        match token {
            Token::Identifier(ref keyword) => {
                if map.contains_key(keyword as &str) {
                    let stmt = parser.parse_stmt(&keyword)?;
                    let v =  match stmts.get_mut(keyword as &str) {
                        Some(v) => v,
                        None => {
                            stmts.insert(keyword.to_string(), Vec::new());
                            stmts.get_mut(keyword as &str).unwrap()
                        }
                    };
                    v.push(stmt);
                } else {
                    parser.save_token(token, pos);
                    break;
                }
            }
            _ => {
                parser.save_token(token, pos);
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

#[derive(Debug, Clone)]
pub struct TBD {}

/// Yang Statement
pub enum StmtType {
    ModuleStmt(ModuleStmt),
    SubmoduleStmt(SubmoduleStmt),
    YangVersionStmt(YangVersionStmt),
    ImportStmt(ImportStmt),
    IncludeStmt(IncludeStmt),
    NamespaceStmt(NamespaceStmt),
    PrefixStmt(PrefixStmt),
    BelongsToStmt(TBD),
    OrganizationStmt(OrganizationStmt),
    ContactStmt(ContactStmt),
    DescriptionStmt(DescriptionStmt),
    ReferenceStmt(ReferenceStmt),
}

impl fmt::Debug for StmtType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match &*self {
            StmtType::ModuleStmt(stmt) => write!(f, "module {:?}", stmt),
            StmtType::SubmoduleStmt(stmt) => write!(f, "submodule {:?}", stmt),
            StmtType::YangVersionStmt(stmt) => write!(f, "yang-version {:?}", stmt),
            StmtType::ImportStmt(stmt) => write!(f, "import {:?}", stmt),
            StmtType::IncludeStmt(stmt) => write!(f, "include {:?}", stmt),
            StmtType::NamespaceStmt(stmt) => write!(f, "namespace {:?}", stmt),
            StmtType::PrefixStmt(stmt) => write!(f, "prefix {:?}", stmt),
            StmtType::BelongsToStmt(stmt) => write!(f, "belongs-to {:?}", stmt),
            StmtType::OrganizationStmt(stmt) => write!(f, "organization {:?}", stmt),
            StmtType::ContactStmt(stmt) => write!(f, "contact {:?}", stmt),
            StmtType::DescriptionStmt(stmt) => write!(f, "description {:?}", stmt),
            StmtType::ReferenceStmt(stmt) => write!(f, "reference {:?}", stmt),
        }
    }
}


/// YANG Statement trait for a single statement.
pub trait Stmt {

    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized;

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> where Self: Sized;
}

/*
/// YANG Statements trait for a collection of statements.
pub trait Stmts {

}
*/

// Yang "module" statement.
#[derive(Debug, Clone)]
pub struct ModuleStmt {
    // Module identifier.
    identifier: String,

    module_header: ModuleHeaderStmts,
    linkage: LinkageStmts,
//    meta: MetaStmts,
//    revision: RevisionStmts,
//    body: BodyStmts,
}

impl Stmt for ModuleStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "module"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;
        let (token, _) = parser.get_token()?;
        if let Token::BlockBegin = token {
            let module_header = ModuleHeaderStmts::parse(parser)?;
            let linkage = LinkageStmts::parse(parser)?;
            // meta-stmts
            // revision-stmts
            // body-stmts

            let stmt = ModuleStmt {
                identifier: arg,
                module_header,
                linkage,
            };

            let (token, _) = parser.get_token()?;
            if let Token::BlockEnd = token {
                println!("*** blockend??");
            } else {
                return Err(YangError::UnexpectedToken(parser.line()));
            }

            Ok(StmtType::ModuleStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct SubmoduleStmt {
    identifier: String,

//    submodule_header: SubmoduleHeaderStmts,
//    liknage: LinkageStmts,
//    meta: MetaStmts,
//    revision: RevisionStmts,
//    body: BodyStmts,
}

impl Stmt for SubmoduleStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "submodule"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = SubmoduleStmt {
            identifier: arg,
        };

        Ok(StmtType::SubmoduleStmt(stmt))
    }
}

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

        let mut stmts = parse_stmts(parser, map)?;
        let yang_version = collect_a_stmt!(stmts, YangVersionStmt)?;
        let namespace = collect_a_stmt!(stmts, NamespaceStmt)?;
        let prefix = collect_a_stmt!(stmts, PrefixStmt)?;

        let stmts = ModuleHeaderStmts {
            yang_version,
            namespace,
            prefix,
        };

        Ok(stmts)
    }
}

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

        let mut stmts = parse_stmts(parser, map)?;
        let import = collect_vec_stmt!(stmts, ImportStmt)?;
        let include = collect_vec_stmt!(stmts, IncludeStmt)?;

        let stmts = LinkageStmts {
            import,
            include,
        };

        Ok(stmts)
    }
}


pub struct SubmoduleHeaderStmts {
    yang_version: Box<YangVersionStmt>,
//    belong_to: BelongToStmt,
}

//#[derive(Copy, Clone)]
pub struct MetaStmts {
    organization: Option<OrganizationStmt>,
    contact: Option<ContactStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

#[derive(Debug, Clone)]
pub struct RevisionStmts {
//    revision: Vec<RevisionStmt>
}

#[derive(Debug, Clone)]
pub struct YangVersionStmt {
    yang_version_arg: String,
}

impl Stmt for YangVersionStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "yang-version"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        // TBD: check arg is "1.1"

        let stmt = YangVersionStmt {
            yang_version_arg: String::from("1.1"),
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::YangVersionStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    identifier_arg: String,
    prefix: PrefixStmt,
//    revision_date: Option<RevisionDateStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for ImportStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "import"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let map: HashMap<&'static str, Repeat> = [
            ("prefix", Repeat::new(Some(1), Some(1))),
//            ("revision-date", Repeat::new(Some(1), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmts(parser, map)?;
        let prefix = collect_a_stmt!(stmts, PrefixStmt)?;
        let description = collect_opt_stmt!(stmts, DescriptionStmt)?;
        let reference = collect_opt_stmt!(stmts, ReferenceStmt)?;

        let stmt = ImportStmt {
            identifier_arg: arg,
            prefix,
            description,
            reference,
        };

        let (token, _) = parser.get_token()?;

        if let Token::StatementEnd = token {
            Ok(StmtType::ImportStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct IncludeStmt {
    identifier_arg: String,
//    revision_date: Option<RevisionDateStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for IncludeStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "incluse"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let map: HashMap<&'static str, Repeat> = [
//            ("revision-date", Repeat::new(Some(1), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmts(parser, map)?;
        let description = collect_opt_stmt!(stmts, DescriptionStmt)?;
        let reference = collect_opt_stmt!(stmts, ReferenceStmt)?;

        let stmt = IncludeStmt {
            identifier_arg: arg,
            description,
            reference,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::IncludeStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct NamespaceStmt {
    uri_str: String,
}

impl Stmt for NamespaceStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "namespace"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = NamespaceStmt {
            uri_str: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::NamespaceStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct PrefixStmt {
    prefix_arg: String,
}

impl Stmt for PrefixStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "prefix"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = PrefixStmt {
            prefix_arg: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::PrefixStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct OrganizationStmt {
    string: String,
}

impl Stmt for OrganizationStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "organization"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = OrganizationStmt {
            string: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::OrganizationStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ContactStmt {
    string: String,
}

impl Stmt for ContactStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "contact"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = ContactStmt {
            string: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::ContactStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct DescriptionStmt {
    string: String,
}

impl Stmt for DescriptionStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "description"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = DescriptionStmt {
            string: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::DescriptionStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ReferenceStmt {
    string: String,
}

impl Stmt for ReferenceStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "reference"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = ReferenceStmt {
            string: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::ReferenceStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}
