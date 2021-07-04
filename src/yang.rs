//
// YANG - YANG struct
//  Copyright (C) 2021 Toshiaki Takada
//

use std::collections::HashMap;
use super::error::*;
use super::parser::*;

/// Get a statement arg.
fn parse_arg(parser: &mut Parser) -> Result<String, YangError> {
    while parser.input_len() > 0 {
        let (token, _) = parser.get_token()?;
        match token {
            // Ignore comments and/or whitespaces.
            Token::Whitespace(_) |
            Token::Comment(_) => {}
            // Statement argument.
            Token::Identifier(s) => return Ok(s),
            // Unexpected Token.
            _ => return Err(YangError::UnexpectedToken(parser.line())),
        }
    }

    Err(YangError::UnexpectedEof)
}

/// Single YANG file conists of a module or a submodule statement.
pub enum Yang {
    Module(ModuleStmt),
    Submodule(SubmoduleStmt),
}

/// YANG Statement trait for a single statement.
pub trait Stmt {
    /// Parse statement body and return statement object.
    fn parse(parser: &mut Parser) -> Result<Box<Stmt>, YangError> where Self: Sized;
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

impl Stmt for ModuleStmt {
    /// Parse and get module-stmt.
    fn parse(parser: &mut Parser) -> Result<Box<Stmt>, YangError> {
        let arg = parse_arg(parser)?;

        // module-header-stmts
        // linkage-stmts
        // meta-stmts
        // revision-stmts
        // body-stmts

        let stmt = ModuleStmt {
            identifier: arg,
        };

        Ok(Box::new(stmt))
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

impl Stmt for SubmoduleStmt {
    fn parse(parser: &mut Parser) -> Result<Box<Stmt>, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = SubmoduleStmt {
            identifier: arg,
        };

        Ok(Box::new(stmt))
    }
}



pub struct ModuleHeaderStmts {
    yang_version: YangVersionStmts,
//    namespace: NamespaceStmt,
//    prefix: PrefixStmt,
}

pub struct SubmoduleHeaderStmts {
    yang_version: YangVersionStmts,
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

pub struct YangVersionStmts {
    yang_version_arg: String,
}

pub struct BodyStmts {

}
