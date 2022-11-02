//
// YANG - YANG sub statement
//  Copyright (C) 2021 Toshiaki Takada
//

use std::collections::HashMap;

use super::core::*;
use super::error::*;
use super::parser::*;
use super::stmt::UnknownStmt;

pub type StmtKeywordFn = fn() -> Keyword;
pub type SelectionKeywordFn = fn() -> Vec<Keyword>;

#[derive(Debug, Clone)]
pub enum SubStmtWith {
    Stmt(StmtKeywordFn),
    Selection(SelectionKeywordFn),
}

#[derive(Debug, Clone)]
pub enum SubStmtDef {
    // stmt
    HasOne(SubStmtWith),
    // [stmt]
    Optional(SubStmtWith),
    // *stmt
    ZeroOrMore(SubStmtWith),
    // 1*stmt
    OneOrMore(SubStmtWith),
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
    pub fn call_stmt_parser(parser: &mut Parser, keyword: &str) -> Result<YangStmt, YangError> {
        match STMT_PARSER.get(keyword) {
            Some(f) => f(parser),
            None => UnknownStmt::parse(parser, keyword),
        }
    }

    pub fn parse_substmts(
        parser: &mut Parser,
        def: Vec<SubStmtDef>,
    ) -> Result<StmtCollection, YangError> {
        // TBD: want to cache this definition somewhere.
        // Keyword to index.
        let mut k2i = HashMap::new();
        // Index to Repeat.
        let mut i2rep = HashMap::new();

        let mut i = 0;
        for s in def {
            let (rep, ssw) = match s {
                SubStmtDef::HasOne(ssw) => (
                    RepeatCount {
                        count: 0,
                        min: 1,
                        max: 1,
                    },
                    ssw,
                ),
                SubStmtDef::Optional(ssw) => (
                    RepeatCount {
                        count: 0,
                        min: 0,
                        max: 1,
                    },
                    ssw,
                ),
                SubStmtDef::ZeroOrMore(ssw) => (
                    RepeatCount {
                        count: 0,
                        min: 0,
                        max: usize::MAX,
                    },
                    ssw,
                ),
                SubStmtDef::OneOrMore(ssw) => (
                    RepeatCount {
                        count: 0,
                        min: 1,
                        max: usize::MAX,
                    },
                    ssw,
                ),
            };
            i2rep.insert(i, rep);

            match ssw {
                SubStmtWith::Stmt(func) => {
                    k2i.insert(func(), i);
                }
                SubStmtWith::Selection(func) => {
                    for k in func() {
                        k2i.insert(k, i);
                    }
                }
            }

            i += 1;
        }

        let mut stmts: StmtCollection = HashMap::new();
        let mut unknown_stmts: StmtCollection = HashMap::new();

        loop {
            let token = parser.get_token()?;

            if parser.config().debug() {
                println!("*** [DEBUG] parse_substmts_default {:?}", token);
            }
            match token {
                Token::Identifier(ref keyword) => {
                    if k2i.contains_key(keyword as &str) {
                        if let Some(rep) = i2rep.get_mut(k2i.get(keyword as &str).unwrap()) {
                            let stmt = Self::call_stmt_parser(parser, &keyword)?;
                            stmts
                                .entry(keyword.clone())
                                .or_insert(Vec::new())
                                .push(stmt);
                            rep.count += 1;
                            // maybe we should validate number.
                        } else {
                            break;
                        }
                    } else if !STMT_PARSER.contains_key(keyword as &str) {
                        // This could be "unknown" statement.
                        let stmt = UnknownStmt::parse(parser, keyword)?;
                        // Because we want to be able to find all unknown statements later,
                        // save them not with their actual keyword (that is still saved within the UnknownStmt)
                        // but instead using the special value "!unknown"
                        // In YANG, identifiers are not allowed to contain exclamation marks,
                        // so this should be collision free even if some new keywords are introduced
                        unknown_stmts
                            .entry(String::from(crate::stmt::UNKNOWN_STMT_KEY))
                            .or_insert(Vec::new())
                            .push(stmt);
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

        // Validation.
        for k in stmts.keys() {
            match k2i.get(k as &str) {
                Some(i) => {
                    let rep = i2rep.get(i).unwrap();
                    if rep.count < rep.min {
                        return Err(YangError::TooFewStatement(k.clone()));
                    }
                    if rep.max < rep.count {
                        return Err(YangError::TooManyStatements(k.clone()));
                    }
                }
                None => return Err(YangError::UnexpectedStatement(k.clone())),
            }
        }

        // Add unknown statements to validated statements
        stmts.extend(unknown_stmts.drain());

        if parser.config().debug() {
            println!("*** [DEBUG] end");
        }

        Ok(stmts)
    }
}
