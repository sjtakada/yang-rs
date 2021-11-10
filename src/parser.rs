//
// YANG - Parser
//  Copyright (C) 2021 Toshiaki Takada
//

use std::cell::Cell;
use std::io::Error;
use std::io::ErrorKind;

use super::config::Config;
use super::core::*;
use super::error::*;
use super::stmt::*;
use super::substmt::*;

use crate::collect_a_stmt;

/// 6.1.3. Quoting
///
///   If a double-quoted string contains a line break followed by space or
///   tab characters that are used to indent the text according to the
///   layout in the YANG file, this leading whitespace is stripped from the
///   string, up to and including the column of the starting double quote
///   character, or to the first non-whitespace character, whichever occurs
///   first.  Any tab character in a succeeding line that must be examined
///   for stripping is first converted into 8 space characters.
///
///   If a double-quoted string contains space or tab characters before a
///   line break, this trailing whitespace is stripped from the string.
///
fn trim_spaces(l: &str, indent: usize) -> String {
    let mut s = String::new();
    let mut chars = l.chars();

    'outer: while let Some(mut c) = chars.next() {
        let mut count = 0;
        loop {
            if c == ' ' {
                count += 1;
            } else if c == '\t' {
                count += 8;
            } else if c == '\n' {
                s.push('\n');
                continue 'outer;
            } else {
                break;
            }

            if let Some(d) = chars.next() {
                c = d;
            } else {
                return s;
            }
        }

        if count > indent {
            s.push_str(&(" ".repeat(count - indent)));
        }
        s.push(c);

        loop {
            if let Some(d) = chars.next() {
                if d == '\n' {
                    break;
                } else {
                    s.push(d);
                }
            } else {
                return s;
            }
        }

        while s.ends_with(|c: char| c == ' ' || c == '\t') {
            s.pop().unwrap();
        }

        s.push('\n');
    }

    s
}

/// YANG Token type.
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    /// Space, tab, carriage return and/or line feed.
    Whitespace(String),

    /// Single line or multi line comments.
    Comment(String),

    /// "+"
    PlusSign,

    /// "{".
    BlockBegin,

    /// "}".
    BlockEnd,

    /// Single quoted or double qouted string.
    QuotedString(String),

    /// Unquoted identifier.
    Identifier(String),

    /// ";".
    StatementEnd,

    /// End of Input.
    EndOfInput,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match &self {
            Token::Whitespace(_) => String::from("Whitespace"),
            Token::Comment(_) => String::from("Comment"),
            Token::PlusSign => String::from("PlusSign"),
            Token::BlockBegin => String::from("BlockBegin"),
            Token::BlockEnd => String::from("BlockEnd"),
            Token::QuotedString(s) => format!("QuotedString '{}'", s),
            Token::Identifier(s) => format!("Identifier '{:?}'", s),
            Token::StatementEnd => String::from("StatementEnd"),
            Token::EndOfInput => String::from("EndOfInput"),
        }
    }
}

/// Parser.
pub struct Parser {
    /// Config.
    config: Config,

    /// Input string.
    input: String,

    /// Cursor position in bytes from the beginning.
    pos: Cell<usize>,

    /// Line number at cursor.
    line: Cell<usize>,

    /// Chars from last line feed.
    column: Cell<usize>,

    /// Saved token.
    saved: Cell<Option<Token>>,
}

impl Parser {
    /// Constructor.
    pub fn new(s: String) -> Parser {
        Parser {
            config: Config::new(),
            input: s,
            pos: Cell::new(0),
            line: Cell::new(0),
            column: Cell::new(0),
            saved: Cell::new(None),
        }
    }

    /// Constructor with config.
    pub fn new_with_config(s: String, config: Config) -> Parser {
        Parser {
            config,
            input: s,
            pos: Cell::new(0),
            line: Cell::new(0),
            column: Cell::new(0),
            saved: Cell::new(None),
        }
    }

    /// Get config reference.
    pub fn config(&self) -> &Config {
        &self.config
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

    /// Move cursor position forward.
    pub fn pos_add(&mut self, pos: usize) {
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

    /// Add chars to column.
    pub fn column_add(&self, num: usize) {
        self.column.set(self.column.get() + num);
    }

    /// Set chars to column from last linefeed.
    pub fn column_set_from(&self, l: &str) {
        let rpos = l.rfind("\n").unwrap();
        self.column.set(l.len() - rpos - 1);
    }

    /// Save token to saved.
    pub fn save_token(&mut self, token: Token) {
        self.saved.replace(Some(token));
    }

    /// Load token from saved.
    pub fn load_token(&mut self) -> Option<Token> {
        self.saved.replace(None)
    }

    /// Get a token and save it.
    pub fn peek_token(&mut self) -> Result<Token, YangError> {
        let token = self.get_token()?;
        self.save_token(token.clone());
        Ok(token)
    }

    /// Get a token except whitespace and comment.
    pub fn get_token(&mut self) -> Result<Token, YangError> {
        let mut st = String::new();
        let mut concat_str = false;
        let mut string_parsed = false;

        if let Some(token) = self.load_token() {
            return Ok(token);
        }

        loop {
            if self.input_len() == 0 {
                if st.len() > 0 {
                    return Ok(Token::QuotedString(st));
                } else if concat_str {
                    return Err(YangError::UnexpectedEof);
                }

                return Ok(Token::EndOfInput);
            }

            let (token, _pos) = self.get_single_token()?;
            match token {
                Token::Whitespace(_) | Token::Comment(_) => {}
                Token::QuotedString(s) => {
                    if st.len() == 0 || concat_str {
                        st.push_str(&s);
                        concat_str = false;
                        string_parsed = true;
                    } else {
                        return Err(YangError::InvalidString(s));
                    }
                }
                Token::PlusSign => {
                    if concat_str {
                        return Err(YangError::InvalidString(st));
                    } else {
                        concat_str = true;
                    }
                }
                _ => {
                    if concat_str {
                        return Err(YangError::InvalidString(st));
                    }

                    if string_parsed {
                        self.save_token(token);
                        return Ok(Token::QuotedString(st));
                    }

                    return Ok(token);
                }
            }
        }
    }

    /// Get a single token and position.
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
            let line = l.matches("\n").count();
            if line > 0 {
                self.column_set_from(l);
                self.line_add(line);
            } else {
                self.column_add(l.len());
            }

            token = Token::Whitespace(String::from(l));
        } else if input.starts_with("//") {
            pos = match input.find(|c: char| c == '\r' || c == '\n') {
                Some(pos) => pos,
                None => input.len(),
            };
            token = Token::Comment(String::from(&input[2..pos]));
        } else if input.starts_with("/*") {
            let mut l = &input[2..];
            pos = match l.find("*/") {
                Some(pos) => pos,
                None => return Err(YangError::InvalidComment),
            };

            l = &l[..pos];

            let line = l.matches("\n").count();
            if line > 0 {
                self.column_set_from(l);
                self.line_add(line);
            } else {
                self.column_add(pos + 4);
            }

            token = Token::Comment(String::from(l));
            pos += 4;
        } else if input.starts_with('+') {
            pos = 1;
            self.column_add(1);
            token = Token::PlusSign;
        } else if input.starts_with('{') {
            pos = 1;
            self.column_add(1);
            token = Token::BlockBegin;
        } else if input.starts_with('}') {
            pos = 1;
            self.column_add(1);
            token = Token::BlockEnd;
        } else if input.starts_with(';') {
            pos = 1;
            self.column_add(1);
            token = Token::StatementEnd;
        } else if input.starts_with('"') {
            let mut l = &input[1..];

            let mut chars = l.chars();
            loop {
                let c = match chars.next() {
                    Some(c) => c,
                    None => {
                        return Err(YangError::InvalidString(
                            "String not terminated".to_string(),
                        ))
                    }
                };

                if c == '\\' {
                    let d = match chars.next() {
                        Some(d) => d,
                        None => {
                            return Err(YangError::InvalidString(
                                "String not terminated".to_string(),
                            ))
                        }
                    };
                    if d != 'n' && d != 't' && d != '"' && d != '\\' {
                        return Err(YangError::InvalidString(format!(
                            "backslash followed by invalid char '{}'",
                            d
                        )));
                    }
                    pos += 2;
                } else if c == '"' {
                    l = &l[..pos];
                    break;
                } else {
                    pos += c.len_utf8();
                }
            }

            let line = l[..pos].matches("\n").count();
            if line > 0 {
                let column = self.column.get() + 1;
                let s = trim_spaces(l, column);

                self.line_add(line);
                token = Token::QuotedString(s);
            } else {
                token = Token::QuotedString(String::from(&l[..pos]));
            }

            pos += 2;
        } else if input.starts_with("'") {
            let l = &input[1..];
            pos = match l.find("'") {
                Some(pos) => pos,
                None => {
                    return Err(YangError::InvalidString(
                        "String not terminated".to_string(),
                    ))
                }
            };

            let line = l[..pos].matches("\n").count();
            self.line_add(line);

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

        self.pos_add(pos);
        Ok((token, pos))
    }

    /// Return substatements definition.
    fn substmts_def() -> Vec<SubStmtDef> {
        vec![
            SubStmtDef::Optional(SubStmtWith::Stmt(ModuleStmt::keyword)),
            SubStmtDef::Optional(SubStmtWith::Stmt(SubmoduleStmt::keyword)),
        ]
    }

    /// Entry point of YANG parser.  An input and a config has to be set.
    /// It will return a module or submodule statement.
    pub fn parse_yang(&mut self) -> Result<YangStmt, YangError> {
        let mut stmts = SubStmtUtil::parse_substmts(self, Self::substmts_def())?;

        if stmts.contains_key("module") {
            let module = collect_a_stmt!(stmts, ModuleStmt)?;
            Ok(YangStmt::ModuleStmt(module))
        } else if stmts.contains_key("submodule") {
            let submodule = collect_a_stmt!(stmts, SubmoduleStmt)?;
            Ok(YangStmt::SubmoduleStmt(submodule))
        } else {
            Err(YangError::UnexpectedEof)
        }
    }

    /// Parse string as an input, and return YangStmt. Encapsulate YangError into io::Error.
    pub fn parse_yang_from_string(s: String, config: Config) -> Result<YangStmt, std::io::Error> {
        let mut parser = Parser::new_with_config(s, config);
        parser.parse_yang().map_err(|err| {
            Error::new(
                ErrorKind::Other,
                format!(
                    "YangError: {:?} at line {}, pos {}",
                    err,
                    parser.line(),
                    parser.pos()
                ),
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_get_token() {
        let s = "module { }";
        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("module".to_string()));

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::BlockBegin);

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::BlockEnd);
    }

    #[test]
    pub fn test_get_token_comment_1() {
        let s = "module; /* comment */ statement";
        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("module".to_string()));

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::StatementEnd);

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("statement".to_string()));
    }

    #[test]
    pub fn test_get_token_comment_2() {
        let s = "module // comment
";
        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("module".to_string()));

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_comment_3() {
        let s = "/* comment // */ module";
        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier("module".to_string()));

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_comment_4() {
        let s = "// /* comment */ module";
        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_string_1() {
        let s = r#" "string" "#;
        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(
            token,
            Token::QuotedString(String::from("string".to_string()))
        );

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_string_2() {
        let s = r#" '"string"' "#;
        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(
            token,
            Token::QuotedString(String::from(r#""string""#.to_string()))
        );

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_string_3() {
        let s = r#" "Hello" + "World" { }"#;
        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(
            token,
            Token::QuotedString(String::from(r#"HelloWorld"#.to_string()))
        );

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::BlockBegin);

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::BlockEnd);

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_string_4() {
        let s = r#" 'string1
 string2 ' "#;

        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(
            token,
            Token::QuotedString(String::from("string1\n string2 ".to_string()))
        );

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_string_5() {
        let s = r#"    "string1
     string2" "#;

        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(
            token,
            Token::QuotedString(String::from("string1\nstring2".to_string()))
        );

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_string_6() {
        let s = r#"/**/"string1

      string2   	 
	
 string3	" + "string4" "#;

        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(
            token,
            Token::QuotedString(String::from(
                "string1\n\n string2\n\nstring3	string4".to_string()
            ))
        );

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::EndOfInput);
    }

    #[test]
    pub fn test_get_token_empty() {
        let s = r#"identifier " ";"#;
        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier(String::from("identifier")));

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::QuotedString(String::from(" ")));

        let s = r#"identifier "";"#;
        let mut parser = Parser::new(s.to_string());

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::Identifier(String::from("identifier")));

        let token = parser.get_token().unwrap();
        assert_eq!(token, Token::QuotedString(String::from("")));
    }
}
