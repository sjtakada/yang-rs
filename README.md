# yang-rs

Rust YANG parser. Supporting RFC7950.

## Usage
Parse and dump a YANG file.
```
cargo run YANG-FILE
```

Code example to parse YANG string and get parsed Rust object.
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

## Feature 'cisco-nso-extensions'

Enables parsing a few nonstandard constructs Cisco allows and uses with Network Service Orchestrator (NSO):

- Empty `input` / `output` statements
- `deref` in `leafref` `path` substatements ([see RFC errata 5617](https://www.rfc-editor.org/errata/eid5617))
