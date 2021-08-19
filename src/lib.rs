//
// YANG - Yet Another Next Generation
//   The parser and libraries supporting RFC7950.
//
//   Copyright (C) 2021 Toshiaki Takada
//

pub mod core;
pub mod error;
pub mod parser;
pub mod arg;
pub mod stmt;
pub mod substmt;
pub mod compound;

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
                let mut error = false;
                let mut w = Vec::new();
                for en in v.drain(..) {
                    if let StmtType::$st(stmt) = en {
                        w.push(stmt)
                    } else {
                        error = true;
                    }
                }

                if error {
                    Err(YangError::PlaceHolder)
                } else {
                    Ok(w)
                }
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

#[macro_export]
macro_rules! parse_a_stmt {
    ($st:ident, $parser:ident) => {
        match <$st>::parse($parser)? {
            StmtType::$st(stmt) => Ok(stmt),
            _ => Err(YangError::UnexpectedStatement($parser.line())),
        }
    };
}
