use std::{cmp::Ordering, fmt, fs, path::Path};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Star,
    Slash,
    Mod,
    Xor,
    Bang,
    Colon,
    BitAnd,
    BitOr,
    LogicalAnd,
    LogicalOr,
    DoubleDot,
    ShiftLeft,
    ShiftRight,
    Arrow,
    Tilde,

    Equal,
    DoubleEqual,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    StringLiteral,
    CharLiteral,
    IntLiteral,
    FloatLiteral,
    True,
    False,

    KeywordConst,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordFor,
    KeywordIn,
    KeywordFunc,
    KeywordReturn,
    KeywordBreak,
    KeywordContinue,
    KeywordExtern,
    KeywordExport,
    KeywordStruct,
    KeywordNew,
    KeywordAs,

    Indent,
    Dedent,
    Eof,
}

#[derive(Debug)]
pub struct ZernError {
    pub loc: Loc,
    pub message: String,
}

impl fmt::Display for ZernError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} \x1b[91mERROR\x1b[0m: {}", self.loc, self.message)
    }
}

impl std::error::Error for ZernError {}

macro_rules! error {
    ($loc:expr, $msg:expr) => {
        Err(ZernError {
            loc: $loc.clone(),
            message: $msg.into(),
        })
    };
}

pub(crate) use error;

#[derive(Debug, Clone, PartialEq)]
pub struct Loc {
    pub filename: String,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filename, self.line, self.column)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub loc: Loc,
}

pub struct Tokenizer {
    source: Vec<char>,
    tokens: Vec<Token>,
    indent_stack: Vec<usize>,
    current_indent: usize,
    start: usize,
    current: usize,
    loc: Loc,
}

impl Tokenizer {
    pub fn new(filename: String, source: String) -> Tokenizer {
        Tokenizer {
            source: source.chars().collect(),
            tokens: vec![],
            indent_stack: vec![0],
            current_indent: 0,
            start: 0,
            current: 0,
            loc: Loc {
                filename,
                line: 1,
                column: 1,
            },
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, ZernError> {
        while !self.eof() {
            self.start = self.current;
            self.scan_token()?;
        }
        self.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: String::new(),
            loc: self.loc.clone(),
        });

        Ok(self.tokens)
    }

    fn scan_token(&mut self) -> Result<(), ZernError> {
        match self.advance() {
            '(' => self.add_token(TokenType::LeftParen)?,
            ')' => self.add_token(TokenType::RightParen)?,
            '[' => self.add_token(TokenType::LeftBracket)?,
            ']' => self.add_token(TokenType::RightBracket)?,
            '+' => {
                if self.match_char('=') {
                    self.add_token(TokenType::PlusEqual)?
                } else {
                    self.add_token(TokenType::Plus)?
                }
            }
            '*' => self.add_token(TokenType::Star)?,
            ',' => self.add_token(TokenType::Comma)?,
            '%' => self.add_token(TokenType::Mod)?,
            '^' => self.add_token(TokenType::Xor)?,
            ':' => self.add_token(TokenType::Colon)?,
            '~' => self.add_token(TokenType::Tilde)?,
            '-' => {
                if self.match_char('=') {
                    self.add_token(TokenType::MinusEqual)?
                } else if self.match_char('>') {
                    self.add_token(TokenType::Arrow)?
                } else {
                    self.add_token(TokenType::Minus)?
                }
            }
            '.' => {
                if self.match_char('.') {
                    self.add_token(TokenType::DoubleDot)?
                } else {
                    return error!(self.loc, "expected '.' after '.'");
                }
            }
            '/' => {
                if self.match_char('/') {
                    while !self.eof() && self.peek() != '\n' {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash)?
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::LogicalAnd)?
                } else {
                    self.add_token(TokenType::BitAnd)?
                }
            }
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::LogicalOr)?
                } else {
                    self.add_token(TokenType::BitOr)?
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::NotEqual)?
                } else {
                    self.add_token(TokenType::Bang)?
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenType::DoubleEqual)?
                } else {
                    self.add_token(TokenType::Equal)?
                }
            }
            '>' => {
                if self.match_char('>') {
                    self.add_token(TokenType::ShiftRight)?
                } else if self.match_char('=') {
                    self.add_token(TokenType::GreaterEqual)?
                } else {
                    self.add_token(TokenType::Greater)?
                }
            }
            '<' => {
                if self.match_char('<') {
                    self.add_token(TokenType::ShiftLeft)?
                } else if self.match_char('=') {
                    self.add_token(TokenType::LessEqual)?
                } else {
                    self.add_token(TokenType::Less)?
                }
            }
            '\'' => {
                if self.eof() {
                    return error!(self.loc, "unterminated char literal");
                }
                _ = self.match_char('\\'); // if its an escape sequence skip \ and read one more
                if self.eof() {
                    return error!(self.loc, "unterminated char literal");
                }
                self.advance();
                if !self.match_char('\'') {
                    return error!(self.loc, "expected ' after char literal");
                }
                self.add_token(TokenType::CharLiteral)?
            }
            '"' => {
                let start_loc = self.loc.clone();

                while !self.eof() {
                    if self.peek() == '\\' {
                        self.advance();
                        if self.eof() {
                            return error!(
                                self.loc,
                                format!("unterminated string, started at {}", start_loc)
                            );
                        }
                    } else if self.peek() == '"' {
                        break;
                    } else if self.peek() == '\n' {
                        self.loc.line += 1;
                        self.loc.column = 0;
                    }
                    self.advance();
                }

                if self.eof() {
                    return error!(
                        self.loc,
                        format!("unterminated string, started at {}", start_loc)
                    );
                }

                self.advance();
                self.add_token(TokenType::StringLiteral)?
            }
            ' ' | '\r' => {}
            '\n' => {
                self.loc.line += 1;
                self.loc.column = 1;
                self.handle_indentation()?;
            }
            '0'..='9' => self.scan_number()?,
            'A'..='Z' | 'a'..='z' | '_' => self.scan_identifier()?,
            _ => return error!(self.loc, "unexpected character"),
        }
        Ok(())
    }

    fn handle_indentation(&mut self) -> Result<(), ZernError> {
        if self.peek() == '\n' {
            return Ok(());
        }
        let new_indent = self.count_indentation();

        match new_indent.cmp(&self.current_indent) {
            Ordering::Greater => {
                self.indent_stack.push(new_indent);
                self.tokens.push(Token {
                    token_type: TokenType::Indent,
                    lexeme: String::new(),
                    loc: self.loc.clone(),
                });
            }
            Ordering::Less => {
                while !self.indent_stack.is_empty()
                    && *self.indent_stack.last().unwrap() > new_indent
                {
                    self.indent_stack.pop();
                    self.tokens.push(Token {
                        token_type: TokenType::Dedent,
                        lexeme: String::new(),
                        loc: self.loc.clone(),
                    });
                }
                if self.indent_stack.is_empty() || *self.indent_stack.last().unwrap() != new_indent
                {
                    return error!(self.loc, "invalid indentation");
                }
            }
            Ordering::Equal => {}
        }

        self.current_indent = new_indent;
        Ok(())
    }

    fn count_indentation(&mut self) -> usize {
        let mut count = 0;

        while self.peek() == ' ' {
            count += 1;
            self.advance();
        }
        count
    }

    fn scan_number(&mut self) -> Result<(), ZernError> {
        let mut is_float = false;

        if self.source[self.current - 1] == '0' && self.match_char('x') {
            if !self.peek().is_ascii_hexdigit() {
                return error!(self.loc, "expected a digit after '0x'");
            }
            while self.peek().is_ascii_hexdigit() {
                self.advance();
            }
        } else {
            while self.peek().is_ascii_digit() {
                self.advance();
            }
            if self.current + 1 < self.source.len()
                && self.peek() == '.'
                && self.source[self.current + 1] != '.'
            {
                is_float = true;
                self.advance();
                while self.peek().is_ascii_digit() {
                    self.advance();
                }
            }
        }

        if is_float {
            self.add_token(TokenType::FloatLiteral)
        } else {
            self.add_token(TokenType::IntLiteral)
        }
    }

    fn scan_identifier(&mut self) -> Result<(), ZernError> {
        while self.peek().is_alphanumeric() || self.peek() == '_' || self.peek() == '.' {
            self.advance();
        }

        let lexeme: String = self.source[self.start..self.current].iter().collect();

        if lexeme == "include" {
            return self.scan_include();
        }

        self.add_token(match lexeme.as_str() {
            "const" => TokenType::KeywordConst,
            "if" => TokenType::KeywordIf,
            "else" => TokenType::KeywordElse,
            "while" => TokenType::KeywordWhile,
            "for" => TokenType::KeywordFor,
            "in" => TokenType::KeywordIn,
            "func" => TokenType::KeywordFunc,
            "return" => TokenType::KeywordReturn,
            "break" => TokenType::KeywordBreak,
            "continue" => TokenType::KeywordContinue,
            "extern" => TokenType::KeywordExtern,
            "export" => TokenType::KeywordExport,
            "struct" => TokenType::KeywordStruct,
            "new" => TokenType::KeywordNew,
            "as" => TokenType::KeywordAs,
            "true" => TokenType::True,
            "false" => TokenType::False,
            _ => TokenType::Identifier,
        })
    }

    fn scan_include(&mut self) -> Result<(), ZernError> {
        if !self.match_char(' ') {
            return error!(self.loc, "expected a space after 'include'");
        }

        if self.peek() != '"' {
            return error!(self.loc, "expected '\"' after 'include '");
        }
        self.advance();

        let path_start = self.current;
        while !self.eof() && self.peek() != '"' {
            self.advance();
        }

        if self.eof() {
            return error!(self.loc, "unterminated string after 'include '");
        }

        let path: String = self.source[path_start..self.current].iter().collect();
        self.advance(); // consume closing quote

        self.include_file(path)
    }

    // TODO: circular includes lead to "fatal runtime error: stack overflow, aborting"
    pub fn include_file(&mut self, path: String) -> Result<(), ZernError> {
        let source = match fs::read_to_string(&path) {
            Ok(x) => x,
            Err(_) => {
                return error!(self.loc, format!("failed to include {}", path));
            }
        };

        let filename = Path::new(&path).file_name().unwrap().to_str().unwrap();

        let tokenizer = Tokenizer::new(filename.to_owned(), source);
        self.tokens.extend(tokenizer.tokenize()?);
        self.tokens.pop(); // remove inner Eof

        Ok(())
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.eof() || self.peek() != expected {
            false
        } else {
            self.current += 1;
            self.loc.column += 1;
            true
        }
    }

    fn add_token(&mut self, token_type: TokenType) -> Result<(), ZernError> {
        let mut lexeme: String = self.source[self.start..self.current].iter().collect();

        if token_type == TokenType::CharLiteral || token_type == TokenType::StringLiteral {
            lexeme = self.unescape(&lexeme)?;
        }

        self.tokens.push(Token {
            token_type,
            lexeme,
            loc: self.loc.clone(),
        });
        Ok(())
    }

    fn unescape(&self, s: &str) -> Result<String, ZernError> {
        let mut result = String::with_capacity(s.len());
        let mut chars = s.chars();

        while let Some(c) = chars.next() {
            if c != '\\' {
                result.push(c);
                continue;
            }
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('0') => result.push('\0'),
                Some('\\') => result.push('\\'),
                Some('\'') => result.push('\''),
                Some('"') => result.push('"'),
                Some(c) => {
                    return error!(
                        self.loc.clone(),
                        format!("unknown escape sequence: \\{}", c)
                    );
                }
                None => return error!(self.loc.clone(), "unexpected end of escape sequence"),
            }
        }
        Ok(result)
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;
        self.loc.column += 1;
        c
    }

    fn peek(&self) -> char {
        if self.eof() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn eof(&self) -> bool {
        self.current >= self.source.len()
    }
}
