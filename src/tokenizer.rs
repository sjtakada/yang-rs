//
// YANG.rs - Tokenizer.
//   Copyright (C) 2021 Toshiaki Takada
//

use std::fs;
//use std::fs::File;
//use std::io::BufReader;
//use std::io::Read;
//use std::error::Error;

use pest::Parser;
use pest::iterators::Pairs;

use super::error::*;

#[derive(Parser)]
#[grammar = "yang.pest"]
pub struct YangParser<'a> {

    /// Filename for yang module.
    filename: String,

    /// Tokenized pairs.
    tokens: Option<Pairs<'a, Rule>>,
}

impl<'a> YangParser<'a> {

    /// Create YangParser.
    pub fn new() -> YangParser::<'a> {
        YangParser::<'a> {
            filename: String::new(),
            tokens: None,
        }
    }

    /// Initialize parser.
    pub fn init(&mut self, filename: &str) {
        self.filename = String::from(filename);
        self.tokens = None;
    }

    /// Open a file and parse with YangParser
    pub fn tokenize(input: &str) -> Result<Pairs<Rule>, YangParseError> {
        match YangParser::parse(Rule::yang, input) {
            Ok(mut result) => Ok(result),
            Err(err) => Err(YangParseError::InvalidStringLiteral(err.to_string()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_yang_1() {
        let unparsed = fs::read_to_string("yang/ietf-inet-types.yang").expect("cannot read file");

        let result = YangParser::tokenize(&unparsed);
        match result {
            Err(_) => panic!("ng"),
            _ => {}
        }
    }
}
