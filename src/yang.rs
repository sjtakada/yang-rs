//
// YANG - YANG struct
//  Copyright (C) 2021 Toshiaki Takada
//

use std::collections::HashMap;
use super::error::*;
use super::parser::*;

/// Single YANG file conists of a module or a submodule statement.
pub enum Yang {
    Module(ModuleStmt),
    Submodule(SubmoduleStmt),
}

/// Yang Statement trait.
pub trait Stmt {
//    type Arg;

//    /// Parse statement arg.
//    fn parse_arg(parser: &mut Parser) -> Result<Self::Arg, YangError>/* where Self: Sized*/;

    /// Parse statement body and return statement object.
    fn parse(parser: &mut Parser) -> Result<Box<Stmt>, YangError> where Self: Sized;
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
    fn parse(parser: &mut Parser) -> Result<Box<Stmt>, YangError> {
//        let arg = ModuleStmt::parse_arg(parser)?;

        let arg = String::new();

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
//        let arg = SubmoduleStmt::parse_arg(parser)?;
        let arg = String::new();

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
