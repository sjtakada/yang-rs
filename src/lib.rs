//
// YANG - Yet Another Next Generation
//   Following RFC7950
//
//   Copyright (C) 2021 Toshiaki Takada
//

pub mod error;
pub mod parser;
pub mod collate;
pub mod yang;


#[macro_export]
macro_rules! collect_a_stmt {
    ($stmts:expr, $st:ident) => (
        match $stmts.get_mut(<$st>::keyword()) {
            Some(v) => match v.pop() {
                Some(en) => match en {
                    StmtType::$st(stmt) => Ok(stmt),
                    _ => Err(YangError::MissingStatement),
                }
                None => Err(YangError::MissingStatement),
            },
            None => Err(YangError::MissingStatement),
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
            None => Err(YangError::MissingStatement),
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
                    _ => Err(YangError::MissingStatement),
                }
                None => Ok(None),
            },
            None => Err(YangError::MissingStatement),
        }
    );
}


