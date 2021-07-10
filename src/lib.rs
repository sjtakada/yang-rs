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
            Some(vec) => match vec.pop() {
                Some(t) => match t {
                    StmtType::$st(stmt) => Ok(stmt),
                    _ => Err(YangError::MissingStatement),
                }
                None => Err(YangError::MissingStatement),
            },
            None => Err(YangError::MissingStatement),
        }
    );
}

