use std::error::Error;

use crate::tokenizer::{MotError, Token, TokenType, error};

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var {
        name: Token,
        var_type: Token,
        initializer: Expr,
    },
    Block(Vec<Stmt>),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Literal(Token),
    Unary {
        op: Token,
        right: Box<Expr>,
    },
    Variable(Token),
    Assign {
        name: Token,
        value: Box<Expr>,
    },
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse(mut self) -> Result<Vec<Stmt>, Box<dyn Error>> {
        let mut statements = vec![];
        while !self.eof() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, Box<dyn Error>> {
        // TODO: synchronization after parse error
        if self.match_token(&[TokenType::KeywordLet]) {
            self.let_declaration()
        } else {
            self.statement()
        }
    }

    fn let_declaration(&mut self) -> Result<Stmt, Box<dyn Error>> {
        let name = self.consume(TokenType::Identifier, "expected variable name")?;
        self.consume(TokenType::Colon, "expected ':' after variable name")?;
        let var_type = self.consume(TokenType::Identifier, "expected variable type")?;
        self.consume(TokenType::Equal, "expected '=' after variable type")?;
        let initializer = self.expression()?;
        Ok(Stmt::Var {
            name,
            var_type,
            initializer,
        })
    }

    fn block(&mut self) -> Result<Stmt, Box<dyn Error>> {
        self.consume(TokenType::Indent, "expected an indent")?;

        let mut statements = vec![];
        while !self.eof() && !self.match_token(&[TokenType::Dedent]) {
            statements.push(self.declaration()?);
        }

        Ok(Stmt::Block(statements))
    }

    fn statement(&mut self) -> Result<Stmt, Box<dyn Error>> {
        if self.match_token(&[TokenType::KeywordPrint]) {
            Ok(Stmt::Print(self.expression()?))
        } else if self.match_token(&[TokenType::KeywordIf]) {
            self.if_statement()
        } else if self.match_token(&[TokenType::KeywordWhile]) {
            self.while_statement()
        } else {
            Ok(Stmt::Expression(self.expression()?))
        }
    }

    fn if_statement(&mut self) -> Result<Stmt, Box<dyn Error>> {
        let condition = self.expression()?;
        let then_branch = self.block()?;
        let else_branch = if self.match_token(&[TokenType::KeywordElse]) {
            if self.match_token(&[TokenType::KeywordIf]) {
                Some(Box::new(self.if_statement()?))
            } else {
                Some(Box::new(self.block()?))
            }
        } else {
            None
        };
        Ok(Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    fn while_statement(&mut self) -> Result<Stmt, Box<dyn Error>> {
        let condition = self.expression()?;
        let body = self.block()?;
        Ok(Stmt::While {
            condition,
            body: Box::new(body),
        })
    }

    fn expression(&mut self) -> Result<Expr, Box<dyn Error>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, Box<dyn Error>> {
        let expr = self.logical_or()?;

        if self.match_token(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            return match expr {
                Expr::Variable(name) => Ok(Expr::Assign {
                    name,
                    value: Box::new(value),
                }),
                _ => return error!(equals.loc, "invalid assignment target"),
            };
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr, Box<dyn Error>> {
        let mut expr = self.logical_and()?;

        while self.match_token(&[TokenType::Or]) {
            let op = self.previous().clone();
            let right = self.logical_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, Box<dyn Error>> {
        let mut expr = self.equality()?;

        while self.match_token(&[TokenType::And]) {
            let op = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, Box<dyn Error>> {
        let mut expr = self.comparison()?;

        while self.match_token(&[TokenType::DoubleEqual, TokenType::NotEqual]) {
            let op = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Box<dyn Error>> {
        let mut expr = self.term()?;

        while self.match_token(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::LessEqual,
            TokenType::Less,
        ]) {
            let op = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Box<dyn Error>> {
        let mut expr = self.factor()?;

        while self.match_token(&[TokenType::Plus, TokenType::Minus, TokenType::Xor]) {
            let op = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Box<dyn Error>> {
        let mut expr = self.unary()?;

        while self.match_token(&[TokenType::Star, TokenType::Slash, TokenType::Mod]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Box<dyn Error>> {
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                op,
                right: Box::new(right),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, Box<dyn Error>> {
        if self.match_token(&[TokenType::Number, TokenType::String]) {
            Ok(Expr::Literal(self.previous().clone()))
        } else if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "expected ')' after expression")?;
            Ok(Expr::Grouping(Box::new(expr)))
        } else if self.match_token(&[TokenType::Identifier]) {
            Ok(Expr::Variable(self.previous().clone()))
        } else {
            error!(self.peek().loc, "expected expression")
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<Token, Box<dyn Error>> {
        if self.check(&token_type) {
            Ok(self.advance().clone())
        } else {
            error!(self.previous().loc, format!("{}", message))
        }
    }

    fn match_token(&mut self, token_types: &[TokenType]) -> bool {
        for x in token_types {
            if self.check(x) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.eof() {
            false
        } else {
            self.peek().token_type == *token_type
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.eof() {
            self.current += 1;
        }
        self.previous()
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn eof(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }
}
