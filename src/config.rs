//
// YANG - Parser config.
//  Copyright (C) 2021 Toshiaki Takada
//

/// Parser config.
pub struct Config {
    /// Run parser in debug mode.
    debug: bool,

    /// Strict Yang Verson check (default: false).
    yang_version: Option<String>,

    /// Strict sub statement count check (default: true).
    sub_stmts_count_check: bool,
}

impl Config {
    pub fn new() -> Config {
        Config {
            debug: false,
            yang_version: None,
            sub_stmts_count_check: true,
        }
    }

    pub fn set_debug(&mut self, debug: bool) {
        self.debug = debug;
    }

    pub fn debug(&self) -> bool {
        self.debug
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
