//
// YANG - Parser
//  Copyright (C) 2021 Toshiaki Takada
//

use std::cell::Cell;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::Error;
use std::io::ErrorKind;
use std::path::Path;

use super::error::*;
use super::yang::*;

pub type StmtParser = fn(&mut Parser) -> Result<Box<dyn Stmt>, YangError>;

/// Open and parse a YANG file.
pub fn parse_file(filename: &str) -> std::io::Result<()> {
    let mut f = File::open(filename)?;
    let mut s = String::new();
    let p = Path::new(filename)
        .file_stem()
        .ok_or(Error::new(ErrorKind::Other, "Invalid filename"))?;
    let n1 = p
        .to_str()
        .ok_or(Error::new(ErrorKind::Other, "Invalid filename"))?;
    let n2 = str::replace(n1, ".", "_");

    f.read_to_string(&mut s)?;
    let mut parser = Parser::new(s);

    match parser.parse_yang() {
        Ok(yang) => {
            // TBD
        }
        Err(err) => {
            println!(
                "Error: {:?} at line {}, pos {}",
                err,
                parser.line(),
                parser.pos()
            );
        }
    }

    Ok(())
}


// YANG Token type.
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Whitespace(String),
    Comment(String),
    PlusSign,
    BlockBegin,
    BlockEnd,
    QuotedString(String),
    Identifier(String),
    StatementEnd,
    EndOfInput,
}

// Parser.
pub struct Parser {
    /// Input string.
    input: String,

    /// Cursor position in bytes from the beginning.
    pos: Cell<usize>,

    /// Line number at cursor.
    line: Cell<usize>,

    /// Saved token.
    saved: Cell<Option<(Token, usize)>>,

    /// Statement parser callbacks.
    parse_stmt: HashMap<&'static str, StmtParser>,
}

impl Parser {
    /// Constructor.
    pub fn new(s: String) -> Parser {
        Parser {
            input: s,
            pos: Cell::new(0),
            line: Cell::new(0),
            saved: Cell::new(None),
            parse_stmt: HashMap::new(),
        }
    }

    /// Init Stmt parsers.
    pub fn init_stmt_parsers(&mut self) {
        self.register("module", ModuleStmt::parse);
        self.register("submodule", SubmoduleStmt::parse);
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

    /// Save token to saved.
    pub fn save_token(&mut self, token: Token, pos: usize) {
        self.saved.replace(Some((token, pos)));
    }

    /// Load token from saved.
    pub fn load_token(&mut self) -> Option<(Token, usize)> {
        self.saved.replace(None)
    }

    /// Get a token to wrap get_single_token().
    pub fn get_token(&mut self) -> Result<(Token, usize), YangError> {
        let mut st = String::new();
        let mut concat_str = false;

        if let Some((token, pos)) = self.load_token() {
            return Ok((token, pos));
        }

        loop {
            if self.input_len() == 0 {
                if st.len() > 0 {
                    return Ok((Token::QuotedString(st), self.pos()));
                } else if concat_str {
                    return Err(YangError::UnexpectedEof);
                }

                return Ok((Token::EndOfInput, self.pos()));
            }

            let (token, pos) = self.get_single_token()?;
            match token {
                Token::Whitespace(s) |
                Token::Comment(s) => {}
                Token::QuotedString(s) => {
                    if st.len() == 0 || concat_str {
                        st.push_str(&s);
                        concat_str = false;
                    } else {
                        return Err(YangError::InvalidString);
                    }
                }
                Token::PlusSign => {
                    if concat_str {
                        return Err(YangError::InvalidString);
                    } else {
                        concat_str = true;
                    }
                }
                _ => {
                    if concat_str {
                        return Err(YangError::InvalidString);
                    }

                    if st.len() > 0 {
                        self.save_token(token, pos);
                        return Ok((Token::QuotedString(st), self.pos()));
                    }

                    return Ok((token, pos));
                }
            }
        }
    }

    /// Get single token and position.
    pub fn get_single_token(&mut self) -> Result<(Token, usize), YangError> {
        let input = &self.input();
        let token: Token;
        let mut pos: usize = 0;

        if input.starts_with(char::is_whitespace) {
            pos = match input.find(|c: char| !c.is_whitespace()) {
                Some(pos) => pos,
                None => input.len(),
            };

            let l = &input[..pos];
            let len = l.matches("\n").count();
            self.line_add(len);

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

            let len = l[..pos].matches("\n").count();
            self.line_add(len);

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

            let len = l[..pos].matches("\n").count();
            self.line_add(len);

            token = Token::QuotedString(String::from(&l[..pos]));
            pos += 2;
        } else if input.starts_with("'") {
            let l = &input[1..];
            pos = match l.find("'") {
                Some(pos) => pos,
                None => return Err(YangError::InvalidString),
            };
            token = Token::QuotedString(String::from(&l[..pos]));
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

    /// Entry point of YANG parser. It will return a module or submodule.
    pub fn parse_yang(&mut self) -> Result<Yang, YangError> {
        let mut keyword: Option<String> = None;

        // Find a statement keyword.
        while self.input_len() > 0 {
            let (token, pos) = self.get_token()?;
            match token {
                // Ignore.
                Token::Whitespace(_) |
                Token::Comment(_) => {}
                // Module or submodule.
                Token::Identifier(k) => {
                    keyword = Some(k.clone());
                    break;
                }
                _ => return Err(YangError::UnexpectedToken(self.line())),
            }
        }

        if keyword == None {
            return Err(YangError::UnexpectedEof);
        }

        let mut arg: Option<String> = None;
        
        // Find an arg.
        while self.input_len() > 0 {
            let (token, _) = self.get_token()?;
            match token {
                // Ignore.
                Token::Whitespace(_) |
                Token::Comment(_) => {}
                // Module or submodule.
                Token::Identifier(k) => {
                    arg = Some(k.clone());
                    break;
                }
                _ => return Err(YangError::UnexpectedToken(self.line())),
            }
        }

        if arg == None {
            return Err(YangError::UnexpectedEof);
        }

        if keyword == Some("module".to_string()) {
            match self.parse_module(arg.unwrap()) {
                Ok(module) => Ok(Yang::Module(module)),
                Err(_) => Err(YangError::UnexpectedToken(self.line())),
            }
        } else if keyword == Some("submodule".to_string()) {
//            let submodule = self.parse_module();
            Err(YangError::UnexpectedToken(self.line()))
        } else {
            Err(YangError::UnexpectedToken(self.line()))
        }
    }

    pub fn parse_module(&mut self, arg: String) -> Result<ModuleStmt, YangError> {
        let module = ModuleStmt::new(arg);
        let mut block: usize = 0;

        while self.input_len() > 0 {
            let (token, _) = self.get_token()?;
            match token {
                // Ignore.
                Token::Whitespace(_) |
                Token::Comment(_) => {}
                // Module or submodule.
                Token::BlockBegin => {
                    block += 1;
                    break;
                }
                _ => return Err(YangError::UnexpectedToken(self.line())),
            }
        }

        while self.input_len() > 0 {
            let (token, _) = self.get_token()?;
            match token {
                // Ignore.
                Token::Whitespace(_) |
                Token::Comment(_) => {}
                // Module or submodule.
                Token::BlockBegin => {
                    block += 1;
                }
                Token::BlockEnd => {
                    block -= 1;
                    if block == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }

        if block > 0 {
            return Err(YangError::UnexpectedEof);
        }

        println!("*** line {} pos {}", self.line(), self.pos());

        Ok(module)
    }

/*
    /// Recursively parse input and build structs.
    pub fn parse(&mut self) -> Result<(), YangError> {
        // start from yang-file rule.


        // get list of tokens toward the end of statement (; or {} ).
        // match any of statement per keyword to choose, statement parser.
        // if the statement include sub statemnts, call sub statement parser.

        while self.input_len() > 0 {
            let (token, _) = self.get_token()?;
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
                _ => {}
            }
        }

        Ok(())
    }
*/

    /// Register Stmt Parser.
    pub fn register(&mut self, keyword: &'static str, f: StmtParser) {
        self.parse_stmt.insert(keyword, f);
    }

    /// Call Stmt Parsrer
    pub fn parse_stmt(&mut self, keyword: &str) -> Result<Box<dyn Stmt>, YangError> {
        let f = self.parse_stmt.get(keyword).unwrap();
        f(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_get_token_1() {
        let s = "module { }";
        let mut parser = Parser::new(s.to_string());

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("module".to_string()));

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::BlockBegin);

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
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

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Comment(" comment ".to_string()));

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
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

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Comment(" comment".to_string()));

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace("\n".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_4() {
        let s = "/* comment // */ module";
        let mut parser = Parser::new(s.to_string());

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Comment(" comment // ".to_string()));

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("module".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_5() {
        let s = "// /* comment */ module";
        let mut parser = Parser::new(s.to_string());

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Comment(" /* comment */ module".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_6() {
        let s = r#" "string" "#;
        let mut parser = Parser::new(s.to_string());

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::QuotedString(String::from("string".to_string())));

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace(" ".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_7() {
        let s = r#" '"string"' "#;
        let mut parser = Parser::new(s.to_string());

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace(" ".to_string()));
        
        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::QuotedString(String::from(r#""string""#.to_string())));

//        let (token, _) = parser.get_token().unwrap();
//        assert_eq!(token, Token::Whitespace(" ".to_string()));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_8() {
        let s = r#" "Hello" + "World" { }"#;
        let mut parser = Parser::new(s.to_string());

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::QuotedString(String::from(r#"HelloWorld"#.to_string())));

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::BlockBegin);

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::BlockEnd);

        let (token, _) = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }
}
