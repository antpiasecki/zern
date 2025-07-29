use std::{cmp::Ordering, fmt};

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
    Pipe,
    DoubleDot,

    Equal,
    DoubleEqual,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Char,
    Number,
    True,
    False,

    KeywordLet,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordFor,
    KeywordIn,
    KeywordFunc,
    KeywordReturn,
    KeywordBreak,
    KeywordContinue,

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
            '.' => {
                if self.match_char('.') {
                    self.add_token(TokenType::DoubleDot)
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
                    self.add_token(TokenType::Slash)
                }
            }
            '&' => self.add_token(TokenType::And),
            '|' => {
                if self.match_char('>') {
                    self.add_token(TokenType::Pipe);
                } else {
                    self.add_token(TokenType::Or);
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
            // TODO: escape sequences
            '\'' => {
                self.advance();
                if !self.match_char('\'') {
                    return error!(self.loc, "expected ' after char literal");
                }
                self.add_token(TokenType::Char);
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
                self.handle_indentation()?;
            }
            '0'..='9' => self.scan_number(),
            'A'..='Z' | 'a'..='z' | '_' => self.scan_identifier(),
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

    fn scan_number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.'
            && (self.current + 1 >= self.source.len())
            && self.source[self.current + 1].is_ascii_digit()
        {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.add_token(TokenType::Number);
    }

    fn scan_identifier(&mut self) {
        while self.peek().is_alphanumeric()
            || self.peek() == '_'
            || self.peek() == '.'
            || self.peek() == '!'
        {
            self.advance();
        }

        let lexeme: String = self.source[self.start..self.current].iter().collect();
        self.add_token(match lexeme.as_str() {
            "let" => TokenType::KeywordLet,
            "if" => TokenType::KeywordIf,
            "else" => TokenType::KeywordElse,
            "while" => TokenType::KeywordWhile,
            "for" => TokenType::KeywordFor,
            "in" => TokenType::KeywordIn,
            "func" => TokenType::KeywordFunc,
            "return" => TokenType::KeywordReturn,
            "break" => TokenType::KeywordBreak,
            "continue" => TokenType::KeywordContinue,
            "true" => TokenType::True,
            "false" => TokenType::False,
            _ => TokenType::Identifier,
        })
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
