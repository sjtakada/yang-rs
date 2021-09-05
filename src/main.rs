//
// YANG - main
//  Copyright (C) 2021 Toshiaki Takada
//

use std::env;
use std::fs::File;
use std::io::prelude::*;

use getopts::Options;

use yang_rs::config::Config;
use yang_rs::parser::Parser;

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

/// Open and parse a YANG file.
pub fn parse_file(filename: &str, config: Config) -> std::io::Result<()> {
    let mut f = File::open(filename)?;
    let mut s = String::new();

    f.read_to_string(&mut s)?;

    let yang = Parser::parse_yang_from_string(s, config)?;
    println!("{:?}", yang);
    Ok(())
}

/// Main.
fn main() {
    // Command line arguments.
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optopt(
        "y",
        "yang-version",
        "Set explicit yang version",
        "YANG-VERSION",
    );
    opts.optflag("h", "help", "Display this help and exit");
    opts.optflag("v", "version", "Print program version");
    opts.optflag("d", "debug", "Run parser in debug mode");

    let matches = match opts.parse(&args[1..]) {
        Ok(matches) => matches,
        Err(_) => {
            println!("Invalid option");
            print_help(&program, opts);
            std::process::exit(1);
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
    if matches.opt_present("d") {
        config.set_debug(true);
    }

    if !matches.free.is_empty() {
        let file = matches.free[0].clone();
        if let Err(e) = parse_file(&file, config) {
            println!("Error: {:?}", e.to_string());
            std::process::exit(1);
        }
        // Successful.
    } else {
        print_help(&program, opts);
        std::process::exit(1);
    }
}
