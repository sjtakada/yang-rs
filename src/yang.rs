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
println!("*** parset_stmts {:?}", token);
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

println!("*** {} {:?} {}", k, rep, n);

        if !rep.validate(n) {
            return Err(YangError::StatementMismatch(k));
        }
    }

    Ok(stmts)
}

/*
pub fn collect_a_stmt<S: 'static + StmtParser>(stmts: &mut StmtCollection) -> Result<Box<dyn StmtParser>, YangError> {
    match stmts.get_mut(S::keyword()) {
        Some(v) => match v.pop() {
            Some(s) => {
//                let stmt = s.as_any().downcast::<S>().unwrap();
//                Ok(stmt)
                Ok(s)
            }
            None => Err(YangError::MissingStatement),
        },
        None => Err(YangError::MissingStatement),
    }
}

pub fn collect_vec_stmt<S: 'static + StmtParser>(stmts: &mut StmtCollection) -> Result<Vec<Box<S>>, YangError> {
    match stmts.get_mut(S::keyword()) {
        Some(v) => Ok(v.drain(..).map(|s| s.as_any().downcast::<S>().unwrap()).collect()),
        None => return Err(YangError::MissingStatement),
    }
}

pub fn collect_opt_stmt<S: 'static + StmtParser>(stmts: &mut StmtCollection) -> Result<Option<Box<S>>, YangError> {
    match stmts.get_mut(S::keyword()) {
        Some(v) => match v.pop() {
            Some(s) => {
                let stmt = s.as_any().downcast::<S>().unwrap();
                Ok(Some(stmt))
            }
            None => Ok(None),
        },
        None => Err(YangError::MissingStatement),
    }
}
*/

pub struct TBD {}

/// Yang Statement
pub enum StmtType {
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

impl fmt::Debug for StmtType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match &*self {
            StmtType::Module(_stmt) => write!(f, "module"),
            StmtType::Submodule(_stmt) => write!(f, "submodule"),
            StmtType::YangVersion(_stmt) => write!(f, "yang-version"),
            StmtType::Import(_stmt) => write!(f, "import"),
            StmtType::Include(_stmt) => write!(f, "include"),
            StmtType::Namespace(_stmt) => write!(f, "namespace"),
            StmtType::Prefix(_stmt) => write!(f, "prefix"),
            StmtType::BelongsTo(_stmt) => write!(f, "belongs-to"),
            StmtType::Organization(_stmt) => write!(f, "organization"),
            StmtType::Contact(_stmt) => write!(f, "contact"),
            StmtType::Description(_stmt) => write!(f, "description"),
            StmtType::Reference(_stmt) => write!(f, "reference"),
        }
    }
}


/// YANG Statement trait for a single statement.
pub trait StmtParser {

    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized;

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any>;

    /// Parse statement body and return statement object.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> where Self: Sized;
}

/*
/// YANG Statements trait for a collection of statements.
pub trait Stmts {

}
*/

// Yang "module" statement.
#[derive(Clone)]
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
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "module"
    }

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any> {
        Box::new(self)
    }

    /// Parse and get module-stmt.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
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

            Ok(StmtType::Module(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Clone)]
pub struct SubmoduleStmt {
    identifier: String,

//    submodule_header: SubmoduleHeaderStmts,
//    liknage: LinkageStmts,
//    meta: MetaStmts,
//    revision: RevisionStmts,
//    body: BodyStmts,
}

impl StmtParser for SubmoduleStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "submodule"
    }

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any> {
        Box::new(self)
    }

    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = SubmoduleStmt {
            identifier: arg,
        };

        Ok(StmtType::Submodule(stmt))
    }
}

#[derive(Clone)]
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
        let yang_version = collect_a_stmt!(YangVersion, YangVersionStmt, stmts)?;
        let namespace = collect_a_stmt!(Namespace, NamespaceStmt, stmts)?;
        let prefix = collect_a_stmt!(Prefix, PrefixStmt, stmts)?;

        let stmts = ModuleHeaderStmts {
            yang_version,
            namespace,
            prefix,
        };

        Ok(stmts)
    }
}

pub struct LinkageStmts {
    import: Vec<Box<ImportStmt>>,
    include: Vec<Box<IncludeStmt>>,
}

impl LinkageStmts {
    pub fn parse(parser: &mut Parser) -> Result<LinkageStmts, YangError> {
        let map: HashMap<&'static str, Repeat> = [
            ("import", Repeat::new(Some(0), None)),
            ("include", Repeat::new(Some(0), None)),
        ].iter().cloned().collect();

        Err(YangError::UnexpectedToken(parser.line()))
/*
        let mut stmts = parse_stmts(parser, map)?;
        let import = collect_vec_stmt::<ImportStmt>(&mut stmts)?;
        let include = collect_vec_stmt::<IncludeStmt>(&mut stmts)?;

        let stmts = LinkageStmts {
            import,
            include,
        };

        Ok(stmts)
*/
    }
}


pub struct SubmoduleHeaderStmts {
    yang_version: Box<YangVersionStmt>,
//    belong_to: BelongToStmt,
}

//#[derive(Copy, Clone)]
pub struct MetaStmts {
//    organization: Option<OrganizationStmt>,
//    contact: Option<ContactStmt>,
//    description: Option<DescriptionStmt>,
//    reference: Option<ReferenceStmt>,
}

#[derive(Clone)]
pub struct RevisionStmts {
//    revision: Vec<RevisionStmt>
}

#[derive(Clone)]
pub struct YangVersionStmt {
    yang_version_arg: String,
}

impl StmtParser for YangVersionStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "yang-version"
    }

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any> {
        Box::new(self)
    }

    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        // TBD: check arg is "1.1"

        let stmt = YangVersionStmt {
            yang_version_arg: String::from("1.1"),
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::YangVersion(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Clone)]
pub struct ImportStmt {
    identifier_arg: String,
    prefix: Box<PrefixStmt>,
//    revision_date: Option<RevisionDateStmt>,
    description: Option<Box<DescriptionStmt>>,
    reference: Option<Box<ReferenceStmt>>,
}

impl StmtParser for ImportStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "import"
    }

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any> {
        Box::new(self)
    }

    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let map: HashMap<&'static str, Repeat> = [
            ("prefix", Repeat::new(Some(1), Some(1))),
//            ("revision-date", Repeat::new(Some(1), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

/*
        let mut stmts = parse_stmts(parser, map)?;
        let prefix = collect_a_stmt::<PrefixStmt>(&mut stmts)?;
        let description = collect_opt_stmt::<DescriptionStmt>(&mut stmts)?;
        let reference = collect_opt_stmt::<ReferenceStmt>(&mut stmts)?;

        let stmt = ImportStmt {
            identifier_arg: arg,
            prefix,
            description,
            reference,
        };
*/
        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
//            Ok(StmtType::Import(stmt))
            Err(YangError::UnexpectedToken(parser.line()))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Clone)]
pub struct IncludeStmt {
    identifier_arg: String,
//    revision_date: Option<RevisionDateStmt>,
    description: Option<Box<DescriptionStmt>>,
    reference: Option<Box<ReferenceStmt>>,
}

impl StmtParser for IncludeStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "incluse"
    }

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any> {
        Box::new(self)
    }

    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let map: HashMap<&'static str, Repeat> = [
//            ("revision-date", Repeat::new(Some(1), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

/*
        let mut stmts = parse_stmts(parser, map)?;
        let description = collect_opt_stmt::<DescriptionStmt>(&mut stmts)?;
        let reference = collect_opt_stmt::<ReferenceStmt>(&mut stmts)?;

        let stmt = IncludeStmt {
            identifier_arg: arg,
            description,
            reference,
        };
*/

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
//            Ok(StmtType::Include(stmt))
            Err(YangError::UnexpectedToken(parser.line()))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Clone)]
pub struct NamespaceStmt {
    uri_str: String,
}

impl StmtParser for NamespaceStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "namespace"
    }

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any> {
        Box::new(self)
    }

    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = NamespaceStmt {
            uri_str: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::Namespace(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Clone)]
pub struct PrefixStmt {
    prefix_arg: String,
}

impl StmtParser for PrefixStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "prefix"
    }

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any> {
        Box::new(self)
    }

    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = PrefixStmt {
            prefix_arg: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::Prefix(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Clone)]
pub struct DescriptionStmt {
    identifier_arg: String,
}

impl StmtParser for DescriptionStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "description"
    }

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any> {
        Box::new(self)
    }

    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = DescriptionStmt {
            identifier_arg: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::Description(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

#[derive(Clone)]
pub struct ReferenceStmt {
    arg: String,
}

impl StmtParser for ReferenceStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        "reference"
    }

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any> {
        Box::new(self)
    }

    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = parse_arg(parser)?;

        let stmt = ReferenceStmt {
            arg: arg,
        };

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::Reference(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}


/*
impl StmtParser for DummyStmt {
    /// Return statement keyword in &str.
    fn keyword() -> &'static str where Self: Sized {
        ""
    }

    /// Return Any for downcasing.
    fn as_any(self) -> Box<dyn Any> {
        Box::new(self)
    }

    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
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

*/
