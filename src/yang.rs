//
// YANG - YANG struct
//  Copyright (C) 2021 Toshiaki Takada
//

use std::fmt;
use std::collections::HashMap;

use url::Url;

use super::error::*;
use super::parser::*;

#[macro_use]
use crate::collect_a_stmt;
use crate::collect_vec_stmt;
use crate::collect_opt_stmt;

/// TBD
///
///   yang-string         = *yang-char
///
///   ;; any Unicode or ISO/IEC 10646 character, including tab, carriage
///   ;; return, and line feed but excluding the other C0 control
///   ;; characters, the surrogate blocks, and the noncharacters
///   yang-char = %x09 / %x0A / %x0D / %x20-D7FF /
///                               ; exclude surrogate blocks %xD800-DFFF
///              %xE000-FDCF /    ; exclude noncharacters %xFDD0-FDEF
///              %xFDF0-FFFD /    ; exclude noncharacters %xFFFE-FFFF
///              %x10000-1FFFD /  ; exclude noncharacters %x1FFFE-1FFFF
///              %x20000-2FFFD /  ; exclude noncharacters %x2FFFE-2FFFF
///              %x30000-3FFFD /  ; exclude noncharacters %x3FFFE-3FFFF
///              %x40000-4FFFD /  ; exclude noncharacters %x4FFFE-4FFFF
///              %x50000-5FFFD /  ; exclude noncharacters %x5FFFE-5FFFF
///              %x60000-6FFFD /  ; exclude noncharacters %x6FFFE-6FFFF
///              %x70000-7FFFD /  ; exclude noncharacters %x7FFFE-7FFFF
///              %x80000-8FFFD /  ; exclude noncharacters %x8FFFE-8FFFF
///              %x90000-9FFFD /  ; exclude noncharacters %x9FFFE-9FFFF
///              %xA0000-AFFFD /  ; exclude noncharacters %xAFFFE-AFFFF
///              %xB0000-BFFFD /  ; exclude noncharacters %xBFFFE-BFFFF
///              %xC0000-CFFFD /  ; exclude noncharacters %xCFFFE-CFFFF
///              %xD0000-DFFFD /  ; exclude noncharacters %xDFFFE-DFFFF
///              %xE0000-EFFFD /  ; exclude noncharacters %xEFFFE-EFFFF
///              %xF0000-FFFFD /  ; exclude noncharacters %xFFFFE-FFFFF
///              %x100000-10FFFD  ; exclude noncharacters %x10FFFE-10FFFF
/// 
/// YANG string, quoted or unquoted.
fn parse_string(parser: &mut Parser) -> Result<String, YangError> {
    let (token, _) = parser.get_token()?;
    match token {
        // Statement argument.
        Token::Identifier(s) |
        Token::QuotedString(s) => Ok(s),
        // 
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


///
/// Trait for statement arg.
///
pub trait StmtArg {
    /// Parse token and return StmtArg if it is valid.
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> where Self: Sized;

    /// Get argment into string.
    fn get_arg(&self) -> String;
}

/// Yang Identifier.
#[derive(Debug, Clone)]
pub struct Identifier {
    str: String,
}

impl StmtArg for Identifier {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let str = parse_string(parser)?;

        if !str.starts_with(|c: char| c.is_alphabetic() || c == '_') {
            return Err(YangError::InvalidIdentifier);
        }

        if str.len() > 1 {
            if let Some(_) = &str[1..].find(|c: char| !c.is_alphabetic() && !c.is_digit(10) && c != '_' && c != '-' && c != '.') {
                return Err(YangError::InvalidIdentifier);
            }
        }
        Ok(Identifier { str })
    }

    fn get_arg(&self) -> String {
        self.str.clone()
    }
}

/// Yang String.
impl StmtArg for String {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        Ok(parse_string(parser)?)
    }

    fn get_arg(&self) -> String {
        self.clone()
    }
}

/// URL string.
impl StmtArg for Url {
    fn parse_arg(parser: &mut Parser) -> Result<Self, YangError> {
        let s = parse_string(parser)?;

        match Url::parse(&s) {
            Ok(url) => Ok(url),
            Err(err) => Err(YangError::ArgumentParseError(err.to_string())),
        }
    }

    fn get_arg(&self) -> String {
        self.to_string()
    }
}


/// Collection of statements in HashMap.
type StmtCollection = HashMap<String, Vec<StmtType>>;

/// Statement Parser callback type.
type StmtParserFn = fn(&mut Parser) -> Result<StmtType, YangError>;

/// Statement Parser initialization.
lazy_static! {
    static ref STMT_PARSER: HashMap<&'static str, StmtParserFn> = {
        let mut m = HashMap::new();

        m.insert("module", ModuleStmt::parse as StmtParserFn);
        m.insert("submodule", SubmoduleStmt::parse as StmtParserFn);
        m.insert("yang-version", YangVersionStmt::parse as StmtParserFn);
        m.insert("import", ImportStmt::parse as StmtParserFn);
        m.insert("include", IncludeStmt::parse as StmtParserFn);
        m.insert("namespace", NamespaceStmt::parse as StmtParserFn);
        m.insert("prefix", PrefixStmt::parse as StmtParserFn);
        m.insert("organization", OrganizationStmt::parse as StmtParserFn);
        m.insert("contact", ContactStmt::parse as StmtParserFn);
        m.insert("description", DescriptionStmt::parse as StmtParserFn);
        m.insert("reference", ReferenceStmt::parse as StmtParserFn);

        m
    };
}

/// Parse a single statement.
fn call_stmt_parser(parser: &mut Parser, keyword: &str) -> Result<StmtType, YangError> {
    let f = STMT_PARSER.get(keyword).unwrap();
    f(parser)
}

/// Get a list of statements in any order.
pub fn parse_stmt_collection(parser: &mut Parser, map: HashMap<&'static str, Repeat>) -> Result<StmtCollection, YangError> {
    let mut stmts: StmtCollection = HashMap::new();

    loop {
        let (token, pos) = parser.get_token()?;
println!("*** parse_stmts {:?}", token);
        match token {
            Token::Identifier(ref keyword) => {
                if map.contains_key(keyword as &str) {
                    let stmt = call_stmt_parser(parser, &keyword)?;
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
println!("*** rep {:?}", rep);
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


///
/// Trait for a single YANG statement.
///
pub trait Stmt {
    /// Arg type.
    type Arg;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str;

    /// Parse a statement arg.
    fn parse_arg(parser: &mut Parser) -> Result<Self::Arg, YangError> where Self::Arg: StmtArg {
        Self::Arg::parse_arg(parser)
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError>;
}

/*
/// YANG Statements trait for a collection of statements.
pub trait Stmts {

}
*/

///
/// 7.1. The "module" Statement.
///
/// Yang "module" statement.
#[derive(Debug, Clone)]
pub struct ModuleStmt {
    /// Module identifier.
    identifier_arg: Identifier,

    ///
    module_header: ModuleHeaderStmts,

    ///
    linkage: LinkageStmts,

    ///
    meta: MetaStmts,

//    revision: RevisionStmts,
//    body: BodyStmts,
}

impl Stmt for ModuleStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "module"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let identifier_arg = ModuleStmt::parse_arg(parser)?;
        let (token, _) = parser.get_token()?;
        if let Token::BlockBegin = token {
            let module_header = ModuleHeaderStmts::parse(parser)?;
            let linkage = LinkageStmts::parse(parser)?;
            let meta = MetaStmts::parse(parser)?;
            // revision-stmts
            // body-stmts

            let stmt = ModuleStmt {
                identifier_arg,
                module_header,
                linkage,
                meta,
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

///
/// 7.2. The "submodule" Statement.
///
#[derive(Debug, Clone)]
pub struct SubmoduleStmt {
    identifier_arg: Identifier,

//    submodule_header: SubmoduleHeaderStmts,
//    liknage: LinkageStmts,
//    meta: MetaStmts,
//    revision: RevisionStmts,
//    body: BodyStmts,
}

impl Stmt for SubmoduleStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "submodule"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let identifier_arg = SubmoduleStmt::parse_arg(parser)?;

        let stmt = SubmoduleStmt {
            identifier_arg,
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

        let mut stmts = parse_stmt_collection(parser, map)?;
        let yang_version = collect_a_stmt!(stmts, YangVersionStmt)?;
        let namespace = collect_a_stmt!(stmts, NamespaceStmt)?;
        let prefix = collect_a_stmt!(stmts, PrefixStmt)?;

        Ok(ModuleHeaderStmts {
            yang_version,
            namespace,
            prefix,
        })
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

        let mut stmts = parse_stmt_collection(parser, map)?;
        let import = collect_vec_stmt!(stmts, ImportStmt)?;
        let include = collect_vec_stmt!(stmts, IncludeStmt)?;

        Ok(LinkageStmts {
            import,
            include,
        })
    }
}


pub struct SubmoduleHeaderStmts {
    yang_version: Box<YangVersionStmt>,
//    belong_to: BelongToStmt,
}

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
        let organization = collect_opt_stmt!(stmts, OrganizationStmt)?;
        let contact = collect_opt_stmt!(stmts, ContactStmt)?;
        let description = collect_opt_stmt!(stmts, DescriptionStmt)?;
        let reference = collect_opt_stmt!(stmts, ReferenceStmt)?;

        Ok(MetaStmts {
            organization,
            contact,
            description,
            reference,
        })
  }
}

#[derive(Debug, Clone)]
pub struct RevisionStmts {
//    revision: Vec<RevisionStmt>
}

///
/// 7.1.2. The "yang-version" Statement.
///
#[derive(Debug, Clone)]
pub struct YangVersionStmt {
    yang_version_arg: String,
}

impl Stmt for YangVersionStmt {
    /// Arg type.
    type Arg = String;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "yang-version"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let arg = YangVersionStmt::parse_arg(parser)?;

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

///
/// 7.1.5. The "import" Statement.
///
#[derive(Debug, Clone)]
pub struct ImportStmt {
    identifier_arg: Identifier,
    prefix: PrefixStmt,
//    revision_date: Option<RevisionDateStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for ImportStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "import"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let identifier_arg = ImportStmt::parse_arg(parser)?;

        let map: HashMap<&'static str, Repeat> = [
            ("prefix", Repeat::new(Some(1), Some(1))),
//            ("revision-date", Repeat::new(Some(1), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let (token, _) = parser.get_token()?;
        if let Token::BlockBegin = token {
            let mut stmts = parse_stmt_collection(parser, map)?;
            let prefix = collect_a_stmt!(stmts, PrefixStmt)?;
            let description = collect_opt_stmt!(stmts, DescriptionStmt)?;
            let reference = collect_opt_stmt!(stmts, ReferenceStmt)?;

            let (token, _) = parser.get_token()?;

            if let Token::BlockEnd = token {
                let stmt = ImportStmt {
                    identifier_arg,
                    prefix,
                    description,
                    reference,
                };

                Ok(StmtType::ImportStmt(stmt))
            } else {
                Err(YangError::UnexpectedToken(parser.line()))
            }
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

///
/// 7.1.6. The "include" Statement.
///
#[derive(Debug, Clone)]
pub struct IncludeStmt {
    identifier_arg: Identifier,
//    revision_date: Option<RevisionDateStmt>,
    description: Option<DescriptionStmt>,
    reference: Option<ReferenceStmt>,
}

impl Stmt for IncludeStmt {
    /// Arg type.
    type Arg = Identifier;

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "incluse"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let identifier_arg = IncludeStmt::parse_arg(parser)?;

        let map: HashMap<&'static str, Repeat> = [
//            ("revision-date", Repeat::new(Some(1), Some(1))),
            ("description", Repeat::new(Some(0), Some(1))),
            ("reference", Repeat::new(Some(0), Some(1))),
        ].iter().cloned().collect();

        let mut stmts = parse_stmt_collection(parser, map)?;
        let description = collect_opt_stmt!(stmts, DescriptionStmt)?;
        let reference = collect_opt_stmt!(stmts, ReferenceStmt)?;

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            let stmt = IncludeStmt {
                identifier_arg,
                description,
                reference,
            };

            Ok(StmtType::IncludeStmt(stmt))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
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

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "namespace"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let uri_str = NamespaceStmt::parse_arg(parser)?;
        let (token, _) = parser.get_token()?;

        if let Token::StatementEnd = token {
            Ok(StmtType::NamespaceStmt(
                NamespaceStmt {
                    uri_str,
                }
            ))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
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

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "prefix"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let prefix_arg_str = PrefixStmt::parse_arg(parser)?;

        let (token, _) = parser.get_token()?;
        if let Token::StatementEnd = token {
            Ok(StmtType::PrefixStmt(PrefixStmt { prefix_arg_str }))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
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

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "organization"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let string = OrganizationStmt::parse_arg(parser)?;
        let (token, _) = parser.get_token()?;

        if let Token::StatementEnd = token {
            Ok(StmtType::OrganizationStmt(OrganizationStmt { string }))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
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

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "contact"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let string = ContactStmt::parse_arg(parser)?;
        let (token, _) = parser.get_token()?;

        if let Token::StatementEnd = token {
            Ok(StmtType::ContactStmt(ContactStmt { string }))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
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

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "description"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let string = DescriptionStmt::parse_arg(parser)?;
        let (token, _) = parser.get_token()?;

        if let Token::StatementEnd = token {
            Ok(StmtType::DescriptionStmt(DescriptionStmt { string }))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
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

    /// Return statement keyword in &str.
    fn keyword() -> &'static str {
        "reference"
    }

    /// Parse a statement and return the object wrapped in enum.
    fn parse(parser: &mut Parser) -> Result<StmtType, YangError> {
        let string = ReferenceStmt::parse_arg(parser)?;
        let (token, _) = parser.get_token()?;

        if let Token::StatementEnd = token {
            Ok(StmtType::ReferenceStmt(ReferenceStmt { string }))
        } else {
            Err(YangError::UnexpectedToken(parser.line()))
        }
    }
}

