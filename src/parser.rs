//
// YANG - Parser
//  Copyright (C) 2021 Toshiaki Takada
//

use std::cell::Cell;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

use super::error::*;

// YANG Token type.
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Whitespace(String),
    Comment(String),
    PlusSign,
    BlockBegin,
    BlockEnd,
    String(String),
    Identifier(String),
    StatementEnd,
}

// Parser.
pub struct Parser {
    /// Input string.
    input: String,
    /// Cursor positionin bytes from the beginning.
    pos: Cell<usize>,
    /// Line number at cursor.
    line: Cell<usize>,
}

impl Parser {
    /// Constructor.
    pub fn new(s: String) -> Parser {
        Parser {
            input: s,
            pos: Cell::new(0),
            line: Cell::new(0),
        }
    }

    /// Get input string at current position.
    pub fn input(&self) -> &str {
        &self.input[self.pos.get()..]
    }

    /// Return remaining input length.
    pub fn input_len(&self) -> usize {
        self.input.len() - self.pos.get()
    }

    /// Return parser cusor position.
    pub fn pos(&self) -> usize {
        self.pos.get()
    }

    /// Set cursor position.
    pub fn pos_set(&mut self, pos: usize) {
        self.pos.set(self.pos.get() + pos);
    }

    /// Return line number.
    pub fn line(&self) -> usize {
        self.line.get()
    }

    /// Add len to line number.
    pub fn line_add(&self, len: usize) {
        self.line.set(self.line.get() + len);
    }

    /// Get single token and position.
    pub fn get_token(&mut self) -> Result<(Token, usize), YangError> {
        let input = &self.input();
        let token: Token;
        let mut pos: usize = 0;

        if input.starts_with(char::is_whitespace) {
            pos = match input.find(|c: char| !c.is_whitespace()) {
                Some(pos) => pos,
                None => input.len(),
            };

            let l = &input[..pos];
            let v: Vec<&str> = l.matches("\n").collect();
            self.line_add(v.len());

            token = Token::Whitespace(String::from(l));
        } else if input.starts_with("//") {
            pos = match input.find(|c: char| c == '\r' || c == '\n') {
                Some(pos) => pos,
                None => input.len(),
            };
            token = Token::Comment(String::from(&input[2..pos]));
        } else if input.starts_with("/*") {
            let l = &input[2..];
            pos = match l.find("*/") {
                Some(pos) => pos,
                None => return Err(YangError::InvalidComment),
            };
            token = Token::Comment(String::from(&l[..pos]));
            pos += 4;
        } else if input.starts_with('+') {
            pos = 1;
            token = Token::PlusSign;
        } else if input.starts_with('{') {
            pos = 1;
            token = Token::BlockBegin;
        } else if input.starts_with('}') {
            pos = 1;
            token = Token::BlockEnd;
        } else if input.starts_with(';') {
            pos = 1;
            token = Token::StatementEnd;
        } else if input.starts_with('"') {
            let l = &input[1..];

            let mut chars = l.chars();
            loop {
                let c = match chars.next() {
                    Some(c) => c,
                    None => return Err(YangError::InvalidString),
                };

                if c == '\\' {
                    let d = match chars.next() {
                        Some(d) => d,
                        None => return Err(YangError::InvalidString),
                    };
                    if d != 'n' && d != 't' && d != '"' && d != '\\' {
                        return Err(YangError::InvalidString);
                    } 
                    pos += 2;

                } else if c == '"' {
                    break;
                } else {
                    pos += c.len_utf8();
                }
            }

            token = Token::String(String::from(&l[..pos]));
            pos += 2;
        } else if input.starts_with("'") {
            let l = &input[1..];
            pos = match l.find("'") {
                Some(pos) => pos,
                None => return Err(YangError::InvalidString),
            };
            token = Token::String(String::from(&l[..pos]));
            pos += 2;
        } else {
            // 6.1.3. Quoting
            // An unquoted string is any sequence of characters that does not
            // contain any space, tab, carriage return, or line feed characters, a
            // single or double quote character, a semicolon (";"), braces ("{" or
            // "}"), or comment sequences ("//", "/*", or "*/").

            let mut l = &input[pos..];
            while l.len() > 0 {
                let c = l.chars().next().unwrap();

                if c.is_whitespace() || c == '"' || c == '\'' || c == '}' || c == '{' || c == ';' {
                    break;
                }

                if l.starts_with("//") || l.starts_with("/*") || l.starts_with("*/") {
                    break;
                }

                pos += c.len_utf8();
                l = &input[pos..];
            }

            token = Token::Identifier(String::from(&input[..pos]));
        }

        self.pos_set(pos);
        Ok((token, pos))
    }

    /// Recursively parse input and build structs.
    pub fn parse(&mut self) -> Result<(), YangError> {
        while self.input_len() > 0 {
            let (token, pos) = self.get_token()?;
            match token {
                Token::Whitespace(s) => {
                }
                Token::Comment(s) => {
                }
                Token::PlusSign => {
                }
                Token::BlockBegin => {
                }
                Token::BlockEnd => {
                }
                Token::String(s) => {
                }
                Token::Identifier(s) => {
                }
                Token::StatementEnd => {
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn test_get_token_1() {
        let s = "module { }";
        let mut parser = Parser::new(s.to_string());

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("module".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::BlockBegin);

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::BlockEnd);
    }

    #[test]
    pub fn test_get_token_2() {
        let s = "module; /* comment */ statement";
        let mut parser = Parser::new(s.to_string());

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("module".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::StatementEnd);

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Comment(" comment ".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("statement".to_string()));
    }

    #[test]
    pub fn test_get_token_3() {
        let s = "module // comment
";
        let mut parser = Parser::new(s.to_string());

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("module".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Comment(" comment".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace("\n".to_string()));
    }

    #[test]
    pub fn test_get_token_4() {
        let s = "/* comment // */ module";
        let mut parser = Parser::new(s.to_string());

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Comment(" comment // ".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("module".to_string()));
    }

    #[test]
    pub fn test_get_token_5() {
        let s = "// /* comment */ module";
        let mut parser = Parser::new(s.to_string());

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Comment(" /* comment */ module".to_string()));
    }

    #[test]
    pub fn test_get_token_6() {
        let s = r#" "string" "#;
        let mut parser = Parser::new(s.to_string());

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::String(String::from("string".to_string())));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace(" ".to_string()));
    }

    #[test]
    pub fn test_get_token_7() {
        let s = r#" '"string"' "#;
        let mut parser = Parser::new(s.to_string());

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::String(String::from(r#""string""#.to_string())));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Whitespace(" ".to_string()));
    }
}
