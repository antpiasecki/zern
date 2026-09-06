use std::{
    cmp::Ordering,
    collections::HashSet,
    fmt, fs,
    path::{Path, PathBuf},
    rc::Rc,
};

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
    At,

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
    KeywordVar,
    KeywordDefer,
    KeywordTrue,
    KeywordFalse,

    Indent,
    Dedent,
    Dollar,
    Eof,
}

#[derive(Debug)]
pub struct ZernError {
    pub loc: Loc,
    pub message: String,
}

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
    pub filename: Rc<str>,
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

impl Default for Loc {
    fn default() -> Self {
        Self {
            filename: "<unknown>".into(),
            line: 0,
            column: 0,
            length: 1,
        }
    }
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

pub struct Tokenizer<'a> {
    source: Vec<char>,
    tokens: &'a mut Vec<Token>,
    indent_stack: Vec<usize>,
    current_indent: usize,
    start: usize,
    current: usize,
    loc: Loc,
    start_loc: Loc,
    included_paths: &'a mut HashSet<PathBuf>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(
        tokens: &'a mut Vec<Token>,
        filename: String,
        source: String,
        included_paths: &'a mut HashSet<PathBuf>,
    ) -> Tokenizer<'a> {
        Tokenizer {
            source: source.chars().collect(),
            tokens,
            indent_stack: vec![0],
            current_indent: 0,
            start: 0,
            current: 0,
            loc: Loc {
                filename: Rc::from(filename.as_str()),
                line: 1,
                column: 1,
                length: 1,
            },
            start_loc: Loc::default(),
            included_paths,
        }
    }

    pub fn tokenize(mut self) -> Result<(), ZernError> {
        while !self.eof() {
            self.start = self.current;
            self.start_loc = self.loc.clone();
            self.scan_token()?;
        }
        self.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: String::new(),
            loc: self.loc.clone(),
        });

        Ok(())
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
            '$' => self.add_token(TokenType::Dollar)?,
            '@' => self.add_token(TokenType::At)?,
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
                while !self.eof() {
                    if self.peek() == '\\' {
                        self.advance();
                        if self.eof() {
                            return error!(self.loc, format!("unterminated string, started at {}", self.start_loc));
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
                    return error!(self.loc, format!("unterminated string, started at {}", self.start_loc));
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
                while !self.indent_stack.is_empty() && *self.indent_stack.last().unwrap() > new_indent {
                    self.indent_stack.pop();
                    self.tokens.push(Token {
                        token_type: TokenType::Dedent,
                        lexeme: String::new(),
                        loc: self.loc.clone(),
                    });
                }
                if self.indent_stack.is_empty() || *self.indent_stack.last().unwrap() != new_indent {
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
            if self.source[self.current - 1] == '0' && self.peek().is_ascii_digit() {
                return error!(self.loc, "octal literals are not allowed");
            }
            while self.peek().is_ascii_digit() {
                self.advance();
            }
            if self.current + 1 < self.source.len() && self.peek() == '.' && self.source[self.current + 1] != '.' {
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
            "var" => TokenType::KeywordVar,
            "defer" => TokenType::KeywordDefer,
            "true" => TokenType::KeywordTrue,
            "false" => TokenType::KeywordFalse,
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
            return error!(self.start_loc, "unterminated string after 'include'");
        }

        let path: String = self.source[path_start..self.current].iter().collect();
        self.advance(); // consume closing quote

        self.include_file(path)
    }

    fn include_file(&mut self, mut path: String) -> Result<(), ZernError> {
        if path.starts_with("$/") {
            path = find_std_path().join(&path[2..]).to_string_lossy().into_owned();
        }

        let base_dir = Path::new(self.loc.filename.as_ref()).parent().unwrap();
        let resolved_path = base_dir.join(&path);

        let Ok(canonical) = fs::canonicalize(&resolved_path) else {
            return error!(self.loc, format!("failed to resolve {}", path));
        };

        if !self.included_paths.insert(canonical.clone()) {
            return Ok(());
        }

        let Ok(meta) = std::fs::metadata(&canonical) else {
            return error!(self.loc, format!("failed to access {}", path));
        };
        if !meta.file_type().is_file() {
            return error!(
                self.loc,
                format!("refusing to read {} because it is not a regular file", path)
            );
        }

        let Ok(source) = fs::read_to_string(&canonical) else {
            return error!(self.loc, format!("failed to include {}", path));
        };

        let tokenizer = Tokenizer::new(
            self.tokens,
            canonical.to_string_lossy().into_owned(),
            source,
            &mut *self.included_paths,
        );
        tokenizer.tokenize()?;
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
            loc: Loc {
                filename: self.start_loc.filename.clone(),
                line: self.start_loc.line,
                column: self.start_loc.column,
                length: self.current - self.start,
            },
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
                    return error!(self.loc.clone(), format!("unknown escape sequence: \\{}", c));
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
        if self.eof() { '\0' } else { self.source[self.current] }
    }

    fn eof(&self) -> bool {
        self.current >= self.source.len()
    }
}

fn find_std_path() -> PathBuf {
    let path = std::env::current_exe().unwrap();

    for dir in path.ancestors() {
        let candidate = dir.join("std");
        if candidate.is_dir() {
            return candidate;
        }
    }
    panic!("could not find zern std directory");
}
