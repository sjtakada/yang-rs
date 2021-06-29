//
// YANG - YANG struct
//  Copyright (C) 2021 Toshiaki Takada
//

/// Single YANG file conists of a module or a submodule statement.
pub enum Yang {
    Module(ModuleStmt),
    Submodule(SubmoduleStmt),
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

pub struct SubmoduleStmt {
    identifier: String,

    submodule_header: SubmoduleHeaderStmts,
    liknage: LinkageStmts,
    meta: MetaStmts,
    revision: RevisionStmts,
    body: BodyStmts,
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
