//
// YANG - YANG struct
//  Copyright (C) 2021 Toshiaki Takada
//

use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;
use super::error::*;
use super::parser::*;

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
pub fn parse_stmts(parser: &mut Parser, map: HashMap<&'static str, Repeat>) -> Result<HashMap<String, Vec<Stmt>>, YangError> {
    let mut stmts: HashMap<String, Vec<Stmt>> = HashMap::new();

    loop {
        let (token, pos) = parser.get_token()?;
println!("*** parset_stmts {:?}", token);
        match token {
            Token::Identifier(ref keyword) => {
                if map.contains_key(keyword as &str) {
                    let stmt = parser.parse_stmt(&keyword)?;
                    let mut v =  match stmts.get_mut(keyword as &str) {
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

println!("*** {} {:?} {}", k, rep, n);

        if !rep.validate(n) {
            return Err(YangError::StatementMismatch(k));
        }
    }

    Ok(stmts)
}

pub fn collect_a_stmt(stmts: &mut HashMap<String, Vec<Stmt>>, keyword: &str) -> Result<Stmt, YangError> {
    let stmt = match stmts.get_mut(keyword) {
        Some(v) => match v.pop() {
            Some(stmt) => stmt,
            None => return Err(YangError::MissingStatement),
        },
        None => return Err(YangError::MissingStatement),
    };

    Ok(stmt)
}

pub fn collect_vec_stmt(stmts: &mut HashMap<String, Vec<Stmt>>, keyword: &str) -> Result<Vec<Stmt>, YangError> {
    let vec_stmt: Vec<_> = match stmts.get_mut(keyword) {
        Some(v) => v.drain(..).collect(),
        None => return Err(YangError::MissingStatement),
    };

    Ok(vec_stmt)
}

pub fn collect_opt_stmt(stmts: &mut HashMap<String, Vec<Stmt>>, keyword: &str) -> Result<Option<Stmt>, YangError> {
    let opt_stmt = match stmts.get_mut(keyword) {
        Some(v) => match v.pop() {
            Some(stmt) => Some(stmt),
            None => None,
        },
        None => return Err(YangError::MissingStatement),
    };

    Ok(opt_stmt)
}

pub struct TBD {

}

/// Yang Statement
pub enum Stmt {
    Module(ModuleStmt),
    Submodule(SubmoduleStmt),
    YangVersion(YangVersionStmt),
    Import(ImportStmt),
    Include(IncludeStmt),
    Namespace(NamespaceStmt),
    Prefix(PrefixStmt),
    BelongsTo(TBD),
    Organization(TBD),
    Contact(TBD),
    Description(DescriptionStmt),
    Reference(ReferenceStmt),
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match &*self {
            Stmt::Module(_stmt) => write!(f, "module"),
            Stmt::Submodule(_stmt) => write!(f, "submodule"),
            Stmt::YangVersion(_stmt) => write!(f, "yang-version"),
            Stmt::Import(_stmt) => write!(f, "import"),
            Stmt::Include(_stmt) => write!(f, "include"),
            Stmt::Namespace(_stmt) => write!(f, "namespace"),
            Stmt::Prefix(_stmt) => write!(f, "prefix"),
            Stmt::BelongsTo(_stmt) => write!(f, "belongs-to"),
            Stmt::Organization(_stmt) => write!(f, "organization"),
            Stmt::Contact(_stmt) => write!(f, "contact"),
            Stmt::Description(_stmt) => write!(f, "description"),
            Stmt::Reference(_stmt) => write!(f, "reference"),
        }
    }
}

/// YANG Statement trait for a single statement.
pub trait StmtParser {
    /// Parse statement body and return statement object.
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> where Self: Sized;
}

/// YANG Statements trait for a collection of statements.
pub trait Stmts {

}

// Yang "module" statement.
pub struct ModuleStmt {
    // Module identifier.
    identifier: String,

    module_header: ModuleHeaderStmts,
//    linkage: LinkageStmts,
//    meta: MetaStmts,
//    revision: RevisionStmts,
//    body: BodyStmts,
}

impl StmtParser for ModuleStmt {
    /// Parse and get module-stmt.
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;
        let (token, _) = parser.get_token()?;
        if let Token::BlockBegin = token {
            
            let module_header = ModuleHeaderStmts::parse(parser)?;
            // linkage-stmts
            // meta-stmts
            // revision-stmts
            // body-stmts

            let stmt = ModuleStmt {
                identifier: arg,
                module_header,
            };

            let (token, _) = parser.get_token()?;
            if let Token::BlockEnd = token {
                println!("*** blockend??");
            } else {
                return Err(YangError::UnexpectedToken(parser.line()));
            }

            Ok(Stmt::Module(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

pub struct SubmoduleStmt {
    identifier: String,

//    submodule_header: SubmoduleHeaderStmts,
//    liknage: LinkageStmts,
//    meta: MetaStmts,
//    revision: RevisionStmts,
//    body: BodyStmts,
}

impl StmtParser for SubmoduleStmt {
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = SubmoduleStmt {
            identifier: arg,
        };

        Ok(Stmt::Submodule(stmt))
    }
}

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
        let yang_version = collect_a_stmt(&mut stmts, "yang-version");
        let namespace = collect_a_stmt(&mut stmts, "namespace");
        let prefix = collect_a_stmt(&mut stmts, "prefix");

        let stmts = ModuleHeaderStmts {
            yang_version: if let Ok(Stmt::YangVersion(stmt)) = yang_version { stmt } else { panic!("") },
            namespace: if let Ok(Stmt::Namespace(stmt)) = namespace { stmt } else { panic!("") },
            prefix: if let Ok(Stmt::Prefix(stmt)) = prefix { stmt } else { panic!("") },
        };

        Ok(stmts)
    }
}

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
        let mut import_vec = collect_vec_stmt(&mut stmts, "import")?;
        let mut include_vec = collect_vec_stmt(&mut stmts, "include")?;
        let import = import_vec.drain(..).map(|i| if let Stmt::Import(stmt) = i { stmt } else { panic!("") }).collect();
        let include = include_vec.drain(..).map(|i| if let Stmt::Include(stmt) = i { stmt } else { panic!("") }).collect();

        let stmts = LinkageStmts {
            import,
            include,
        };

        Ok(stmts)
    }
}


pub struct SubmoduleHeaderStmts {
    yang_version: YangVersionStmt,
//    belong_to: BelongToStmt,
}

pub struct MetaStmts {
//    organization: Option<OrganizationStmt>,
//    contact: Option<ContactStmt>,
//    description: Option<DescriptionStmt>,
//    reference: Option<ReferenceStmt>,
}

pub struct RevisionStmts {
//    revision: Vec<RevisionStmt>
}

pub struct YangVersionStmt {
    yang_version_arg: String,
}

impl StmtParser for YangVersionStmt {
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        // TBD: check arg is "1.1"

        let stmt = YangVersionStmt {
            yang_version_arg: String::from("1.1"),
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(Stmt::YangVersion(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

pub struct ImportStmt {
    identifier_arg: String,
    prefix: PrefixStmt,
//    revision_date: Option<RevisionDateStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl StmtParser for ImportStmt {
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        let map: HashMap<&'static str, Repeat> = [
            ("prefix", Repeat::new(Some(1), Some(1))),
//            ("revision-date", Repeat::new(Some(1), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmts(parser, map)?;
        let prefix = collect_a_stmt(&mut stmts, "prefix");
        let description = collect_opt_stmt(&mut stmts, "description");
        let reference = collect_opt_stmt(&mut stmts, "reference");

        let stmt = ImportStmt {
            identifier_arg: arg,
            prefix: if let Ok(Stmt::Prefix(stmt)) = prefix { stmt } else { panic!("") },
            description: if let Ok(Some(Stmt::Description(stmt))) = description { Some(stmt) } else { panic!("") },
            reference: if let Ok(Some(Stmt::Reference(stmt))) = reference { Some(stmt) } else { panic!("") },
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(Stmt::Import(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

pub struct IncludeStmt {
    identifier_arg: String,
//    revision_date: Option<RevisionDateStmt>,
//    description: Option<DescriptionStmt>,
//    reference: Option<Reference>,
}

pub struct NamespaceStmt {
    uri_str: String,
}

impl StmtParser for NamespaceStmt {
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = NamespaceStmt {
            uri_str: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(Stmt::Namespace(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

pub struct PrefixStmt {
    prefix_arg: String,
}

impl StmtParser for PrefixStmt {
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = PrefixStmt {
            prefix_arg: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(Stmt::Prefix(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

pub struct DescriptionStmt {
    identifier_arg: String,
}

impl StmtParser for DescriptionStmt {
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = DescriptionStmt {
            identifier_arg: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(Stmt::Description(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

pub struct ReferenceStmt {
    arg: String,
}

impl StmtParser for ReferenceStmt {
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = ReferenceStmt {
            arg: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(Stmt::Reference(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}






pub struct BodyStmts {

}
