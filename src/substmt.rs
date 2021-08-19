//
// YANG - YANG sub statement
//  Copyright (C) 2021 Toshiaki Takada
//

use std::collections::HashMap;

use super::core::*;
//use super::stmt::*;
//use super::arg::*;
use super::parser::*;
use super::error::*;

pub type StmtKeywordFn = fn() -> Keyword;
pub type CompoundKeywordFn = fn() -> Vec<Keyword>;

#[derive(Debug, Clone)]
pub enum SubStmtWith {
    Stmt(StmtKeywordFn),
    Compound(CompoundKeywordFn),
}

#[derive(Debug, Clone)]
pub enum SubStmtDef {
    // stmt
    HaveOne(SubStmtWith),
    // [stmt]
    Optional(SubStmtWith),
    // *stmt
    ZeroOrMore(SubStmtWith),
    // 1*stmt
    OneOrMore(SubStmtWith),
}

pub trait Compound {
    /// Return a list of statements keyword.
    fn keywords() -> Vec<Keyword>;

    /// Return if the compound is anonymous.
    fn anonymous() -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct RepeatCount {
    pub count: usize,
    pub min: usize,
    pub max: usize,
}

///
/// Substatements Utilities.
///
pub struct SubStmtUtil;

impl SubStmtUtil {
    // Parse a single statement.
    fn call_stmt_parser(parser: &mut Parser, keyword: &str) -> Result<StmtType, YangError> {
        let f = STMT_PARSER.get(keyword).unwrap();
        f(parser)
    }

    pub fn parse_substmts(parser: &mut Parser, def: Vec<SubStmtDef>) -> Result<StmtCollection, YangError> {
        // TBD: want to cache this definition somewhere.
        // Keyword to index.
        let mut k2i = HashMap::new();
        // Index to Repeat.
        let mut i2rep = HashMap::new();

        let mut i = 0;
        for s in def {
            let (rep, ssw) = match s {
                SubStmtDef::HaveOne(ssw) => (RepeatCount { count: 0, min: 1, max: 1 }, ssw),
                SubStmtDef::Optional(ssw) => (RepeatCount { count: 0, min: 0, max: 1 }, ssw),
                SubStmtDef::ZeroOrMore(ssw) => (RepeatCount { count: 0, min: 0, max: usize::MAX}, ssw),
                SubStmtDef::OneOrMore(ssw) => (RepeatCount { count: 0, min: 1, max: usize::MAX}, ssw),
            };
            i2rep.insert(i, rep);

            match ssw {
                SubStmtWith::Stmt(func) => {
                    k2i.insert(func(), i);
                }
                SubStmtWith::Compound(func) => {
                    for k in func() {
                        k2i.insert(k, i);
                    }
                }
            }

            i += 1;
        }

        let mut stmts: StmtCollection = HashMap::new();

        loop {
            let token = parser.get_token()?;
println!("*** [DEBUG] parse_substmts_default {:?}", token);
            match token {
                Token::Identifier(ref keyword) => {
                    if k2i.contains_key(keyword as &str) {
                        if let Some(rep) = i2rep.get_mut(k2i.get(keyword as &str).unwrap()) {
                            let stmt = Self::call_stmt_parser(parser, &keyword)?;
                            let v =  match stmts.get_mut(keyword as &str) {
                                Some(v) => v,
                                None => {
                                    stmts.insert(keyword.to_string(), Vec::new());
                                    stmts.get_mut(keyword as &str).unwrap()
                                }
                            };
                            v.push(stmt);
                            rep.count += 1;
                            // TODO: maybe we should validate number.
                        } else {
                            break;
                        }
                    } else {
                        parser.save_token(token);
                        break;
                    }
                }
                _ => {
                    parser.save_token(token);
                    break;
                }
            }
        }

        Ok(stmts)
    }
}

/*
pub struct Grouping {
}

impl Grouping {
    pub fn keyword() -> Keyword {
        "grouping"
    }
}

pub struct TypedefStmt {
}

impl TypedefStmt {
    pub fn keyword() -> Keyword {
        "typedef"
    }
}

pub struct TypedefAndGrouping {

}

impl Compound for TypedefAndGrouping {
    fn keywords() -> Vec<Keyword> {
        vec![TypedefStmt::keyword(), Grouping::keyword()]
    }
}


pub fn parse_tmp() {
    let vec = [
        SubStmtDef::HaveOne(SubStmtWith::Stmt(PrefixStmt::keyword)),
        SubStmtDef::Optional(SubStmtWith::Stmt(RevisionDateStmt::keyword)),
        SubStmtDef::Optional(SubStmtWith::Stmt(DescriptionStmt::keyword)),
        SubStmtDef::Optional(SubStmtWith::Stmt(ReferenceStmt::keyword)),
        SubStmtDef::ZeroOrMore(SubStmtWith::Compound(TypedefAndGrouping::keywords)),
    ];
}

pub fn expand_def(vec: Vec<SubStmtDef>) {
    let mut k2i = HashMap::new();
    let mut i2rep = HashMap::new();

    let mut i = 0;
    for s in vec {
        let (rep, ssw) = match s {
            SubStmtDef::HaveOne(ssw) => ((0, 1, 1), ssw),
            SubStmtDef::Optional(ssw) => ((0, 0, 1), ssw),
            SubStmtDef::ZeroOrMore(ssw) => ((0, 0, usize::MAX), ssw),
            SubStmtDef::OneOrMore(ssw) => ((0, 1, usize::MAX), ssw),
        };
        i2rep.insert(i, rep);

        match ssw {
            SubStmtWith::Stmt(func) => {
                k2i.insert(func(), i);
            }
            SubStmtWith::Compound(func) => {
                for k in func() {
                    k2i.insert(k, i);
                }
            }
        }

        i += 0;
    }
}
*/
