//
// YANG - Yet Another Next Generation
//   Following RFC7950
//
//   Copyright (C) 2021 Toshiaki Takada
//

///
/// YANG parser and library.
/// Conformed to RFC7950 "The YANG 1.1 Data Modeling Language".
///
///

pub mod core;
pub mod error;
pub mod parser;
pub mod arg;
pub mod stmt;

#[macro_use]
extern crate lazy_static;

#[macro_export]
macro_rules! collect_a_stmt {
    ($stmts:expr, $st:ident) => (
        match $stmts.get_mut(<$st>::keyword()) {
            Some(v) => match v.pop() {
                Some(en) => match en {
                    StmtType::$st(stmt) => Ok(stmt),
                    _ => Err(YangError::MissingStatement(<$st>::keyword())),
                }
                None => Err(YangError::MissingStatement(<$st>::keyword())),
            },
            None => Err(YangError::MissingStatement(<$st>::keyword())),
        }
    );
}

#[macro_export]
macro_rules! collect_vec_stmt {
    ($stmts:expr, $st:ident) => (
        match $stmts.get_mut(<$st>::keyword()) {
            Some(v) => {
                let w: Vec<_> = v.drain(..)
                    .map(|en| if let StmtType::$st(stmt) = en { stmt } else { panic!("Invalid Stmt"); })
                    .collect();
                Ok(w)
            }
            None => Ok(Vec::new()),
        }
    );
}

#[macro_export]
macro_rules! collect_opt_stmt {
    ($stmts:expr, $st:ident) => (
        match $stmts.get_mut(<$st>::keyword()) {
            Some(v) => match v.pop() {
                Some(en) => match en {
                    StmtType::$st(stmt) => Ok(Some(stmt)),
                    _ => Err(YangError::MissingStatement(<$st>::keyword())),
                }
                None => Ok(None),
            },
            None => Ok(None),
        }
    );
}


