//
// YANG - Parser config.
//  Copyright (C) 2021 Toshiaki Takada
//

/// Parser config.
pub struct Config {
    /// Strict Yang Verson check (default: false).
    yang_version: Option<String>,

    /// Strict sub statement count check (default: true).
    sub_stmts_count_check: bool,
}

impl Config {
    pub fn new() -> Config {
        Config {
            yang_version: None,
            sub_stmts_count_check: true,
        }
    }

    pub fn set_yang_version(&mut self, yang_version: Option<String>) {
        self.yang_version = yang_version;
    }

    pub fn yang_version(&self) -> Option<&str> {
        match &self.yang_version {
            Some(s) => Some(s),
            None => None
        }
    }

    pub fn sub_stmts_count_check(&self) -> bool {
        self.sub_stmts_count_check
    }

}
