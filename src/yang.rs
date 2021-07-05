//
// YANG - YANG struct
//  Copyright (C) 2021 Toshiaki Takada
//

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

pub struct TBD {

}

/// Yang Statement
pub enum Stmt {
    Module(ModuleStmt),
    Submodule(SubmoduleStmt),
    YangVersion(YangVersionStmt),
    Import(TBD),
    Include(TBD),
    Namespace(NamespaceStmt),
    Prefix(PrefixStmt),
}

/// Single YANG file conists of a module or a submodule statement.
pub enum Yang {
    Module(ModuleStmt),
    Submodule(SubmoduleStmt),
}

/// YANG Statement trait for a single statement.
pub trait StmtParser {
    /// Parse statement body and return statement object.
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> where Self: Sized;
}

/// YANG Statements trait for a collection of statements.
pub trait Stmts {

}

pub struct ModuleStmt {
    identifier: String,

//    module_header: ModuleHeaderStmts,
//    linkage: LinkageStmts,
//    meta: MetaStmts,
//    revision: RevisionStmts,
//    body: BodyStmts,
}

impl ModuleStmt {
    pub fn new(arg: String) -> ModuleStmt {
        ModuleStmt {
            identifier: arg,
        }
    }

}

impl StmtParser for ModuleStmt {
    /// Parse and get module-stmt.
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        // module-header-stmts
        // linkage-stmts
        // meta-stmts
        // revision-stmts
        // body-stmts

        let stmt = ModuleStmt {
            identifier: arg,
        };

        Ok(Stmt::Module(stmt))
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
        let hs: HashSet<&'static str> = ["yang-version", "namespace", "prefix"].iter().cloned().collect();
        let mut yang_version: Option<YangVersionStmt> = None;
        let mut namespace: Option<NamespaceStmt> = None;
        let mut prefix: Option<PrefixStmt> = None;

        loop {
            let (token, pos) = parser.get_token()?;
            match token {
                Token::Identifier(ref keyword) => {
                    if hs.contains(&keyword as &str) {
                        let stmt = parser.parse_stmt(&keyword)?;
                        match stmt {
                            Stmt::YangVersion(stmt) => {
                                yang_version.replace(stmt);
                            }
                            Stmt::Namespace(stmt) => {
                                namespace.replace(stmt);
                            }
                            Stmt::Prefix(stmt) => {
                                prefix.replace(stmt);
                            }
                            _ => {}
                        }
                    } else {
                        parser.save_token(token, pos);
                    }
                }
                _ => {
                    if let None = yang_version {
                        return Err(YangError::MissingStatement);
                    }

                    if let None = namespace {
                        return Err(YangError::MissingStatement);
                    }

                    if let None = prefix {
                        return Err(YangError::MissingStatement);
                    }

                    parser.save_token(token, pos);
                    break;
                }
            }
        }

        let stmts = ModuleHeaderStmts {
            yang_version: yang_version.unwrap(),
            namespace: namespace.unwrap(),
            prefix: prefix.unwrap(),
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

pub struct LinkageStmts {
//    import: Vec<ImportStmt>,
//    include: Vec<IncludeStmt>,
}

pub struct RevisionStmts {
//    revision: Vec<RevisionStmt>
}

pub struct YangVersionStmt {
    yang_version_arg: String,
}

impl YangVersionStmt {
    pub fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = YangVersionStmt {
            yang_version_arg: String::from("1.1"),
        };

        Ok(Stmt::YangVersion(stmt))
    }
}

pub struct NamespaceStmt {
    uri_str: String,
}

impl NamespaceStmt {
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = NamespaceStmt {
            uri_str: arg,
        };

        Ok(Stmt::Namespace(stmt))
    }
}

pub struct PrefixStmt {
    prefix_arg: String,
}

impl PrefixStmt {
    fn parse(parser: &mut Parser) -> Result<Stmt, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = PrefixStmt {
            prefix_arg: arg,
        };

        Ok(Stmt::Prefix(stmt))
    }
}

pub struct BodyStmts {

}
