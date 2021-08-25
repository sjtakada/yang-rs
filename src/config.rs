//
// YANG - Parser config.
//  Copyright (C) 2021 Toshiaki Takada
//

/// Parser config.
pub struct Config {
    /// Strict Yang Verson check (default: false).
    yang_version_check: bool,

    /// Strict sub statement count check (default: true).
    sub_stmts_count_check: bool,
}

impl Config {
    pub fn new() -> Config {
        Config {
            yang_version_check: false,
            sub_stmts_count_check: true,
        }
    }

    pub fn yang_version_check(&self) -> bool {
        self.yang_version_check
    }

    pub fn sub_stmts_count_check(&self) -> bool {
        self.sub_stmts_count_check
    }
}
