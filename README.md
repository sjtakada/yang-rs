# yang-rs

Rust YANG parser. Supporting RFC7950.

## Usage
Parse and dump a YANG file.
```
cargo run YANG-FILE
```

Parse YANG string and get parsed Rust object.
```
use yang_rs::parser::Parser;
use yang_rs::config::Config;
use yang_rs::core::YangStmt;

let mut f = File::open(filename)?;
let mut s = String::new();

f.read_to_string(&mut s)?;

let yang = Parser::parse_yang_from_string(s, config)?;
match yang {
    YangStmt::ModuleStmt(module) => {
        println!("arg {:?}", module.arg());
        println!("arg {:?}", module.module_header().yang_version());
    }
    _ => {}
}

```
