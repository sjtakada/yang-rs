//
// YANG - main
//  Copyright (C) 2021 Toshiaki Takada
//

use std::env;
use getopts::Options;
use yang_rs::parser;
use yang_rs::config::Config;

const YANG_RS_VERSION: &str = "0.1.0";

/// Show help.                                                                                                                  
fn print_help(program: &str, opts: Options) {
    let brief = format!("Usage: {} [options]", program);
    print!("{}", opts.usage(&brief));
}

/// Version.
fn print_version(program: &str) {
    println!("{} version {}", program, YANG_RS_VERSION);
    println!("");
}

fn main() {
    // Command line arguments.                                                                                                  
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optopt("y", "yang-version", "Set explicit yang version", "YANG-VERSION");
    opts.optflag("h", "help", "Display this help and exit");
    opts.optflag("v", "version", "Print program version");

    let matches = match opts.parse(&args[1..]) {
        Ok(matches) => matches,
        Err(_) => {
            println!("Invalid option");
            print_help(&program, opts);
            return;
        }
    };

    if matches.opt_present("h") {
        print_help(&program, opts);
        return;
    }

    if matches.opt_present("v") {
        print_version(&program);
        return;
    }

    let mut config = Config::new();
    config.set_yang_version(matches.opt_str("y"));

    if !matches.free.is_empty() {
        let file = matches.free[0].clone();
        if let Err(e) = parser::parse_file(&file, config) {
            println!("Error: {:?}", e.to_string());
        }
    } else {
        print_help(&program, opts);
    }
}
