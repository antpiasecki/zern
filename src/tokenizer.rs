use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    Mod,
    Xor,
    Bang,
    Colon,
    And,
    Or,

    Equal,
    DoubleEqual,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    Eof,
}

#[derive(Debug)]
pub struct MotError {
    pub loc: Loc,
    pub message: String,
}

impl fmt::Display for MotError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} \x1b[91mERROR\x1b[0m: {}", self.loc, self.message)
    }
}

impl std::error::Error for MotError {}

macro_rules! error {
    ($loc:expr, $msg:expr) => {
        Err(MotError {
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
    start: usize,
    current: usize,
    loc: Loc,
}

impl Tokenizer {
    pub fn new(filename: String, source: String) -> Tokenizer {
        Tokenizer {
            source: source.chars().collect(),
            tokens: vec![],
            start: 0,
            current: 0,
            loc: Loc {
                filename,
                line: 1,
                column: 1,
            },
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, MotError> {
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

    fn scan_token(&mut self) -> Result<(), MotError> {
        match self.advance() {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            '+' => self.add_token(TokenType::Plus),
            '*' => self.add_token(TokenType::Star),
            ',' => self.add_token(TokenType::Comma),
            '-' => self.add_token(TokenType::Minus),
            '%' => self.add_token(TokenType::Mod),
            '^' => self.add_token(TokenType::Xor),
            ':' => self.add_token(TokenType::Colon),
            '/' => {
                if self.match_char('/') {
                    while !self.eof() && self.peek() != '\n' {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash)
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::And);
                } else {
                    return error!(self.loc, "expected '&' after '&'");
                }
            }
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::Or);
                } else {
                    return error!(self.loc, "expected '|' after '|'");
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::NotEqual)
                } else {
                    self.add_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenType::DoubleEqual)
                } else {
                    self.add_token(TokenType::Equal)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterEqual)
                } else {
                    self.add_token(TokenType::Greater)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessEqual)
                } else {
                    self.add_token(TokenType::Less)
                }
            }
            '"' => {
                while !self.eof() && self.peek() != '"' {
                    if self.peek() == '\n' {
                        self.loc.line += 1;
                        self.loc.column = 1;
                    }
                    self.advance();
                }

                if self.eof() {
                    return error!(self.loc, "unterminated string");
                }

                self.advance();
                self.add_token(TokenType::String);
            }
            ' ' | '\t' | '\r' => {}
            '\n' => {
                self.loc.line += 1;
                self.loc.column = 1;
            }
            '0'..='9' => self.scan_number(),
            'A'..='Z' | 'a'..='z' | '_' => self.scan_identifier(),
            _ => return error!(self.loc, "unexpected character"),
        }
        Ok(())
    }

    fn scan_number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.add_token(TokenType::Number);
    }

    fn scan_identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' || self.peek() == '.' {
            self.advance();
        }

        let lexeme: String = self.source[self.start..self.current].iter().collect();
        match lexeme.as_str() {
            _ => self.add_token(TokenType::Identifier),
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.eof() || self.peek() != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn add_token(&mut self, token_type: TokenType) {
        let lexeme: String = self.source[self.start..self.current].iter().collect();
        self.tokens.push(Token {
            token_type,
            lexeme,
            loc: self.loc.clone(),
        });
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
