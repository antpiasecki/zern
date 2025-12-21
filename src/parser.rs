use crate::tokenizer::{Token, TokenType, ZernError, error};

#[derive(Debug, Clone)]
pub struct Param {
    pub var_type: Token,
    pub var_name: Token,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    Let {
        name: Token,
        var_type: Token,
        initializer: Expr,
    },
    Block(Vec<Stmt>),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Box<Stmt>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    For {
        var: Token,
        start: Expr,
        end: Expr,
        body: Box<Stmt>,
    },
    Function {
        name: Token,
        params: Vec<Param>,
        return_type: Token,
        body: Box<Stmt>,
        exported: bool,
    },
    Return(Expr),
    Break,
    Continue,
    Extern(Token),
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
    Call {
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr>,
    },
    ArrayLiteral(Vec<Expr>),
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    AddrOf {
        op: Token,
        expr: Box<Expr>,
    },
}

// TODO: currently they are all just 8 byte values
static TYPES: [&str; 7] = ["Void", "U8", "I64", "String", "Bool", "Ptr", "Array"];

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    is_inside_function: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current: 0,
            is_inside_function: false,
        }
    }

    pub fn parse(mut self) -> Result<Vec<Stmt>, ZernError> {
        let mut statements = vec![];
        while !self.eof() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, ZernError> {
        if !self.is_inside_function {
            if self.match_token(&[TokenType::KeywordFunc]) {
                return self.func_declaration(false);
            }
            if self.match_token(&[TokenType::KeywordExport]) {
                self.consume(TokenType::KeywordFunc, "expected 'func' after 'export'")?;
                return self.func_declaration(true);
            }
            if self.match_token(&[TokenType::KeywordExtern]) {
                return self.extern_declaration();
            }
            return error!(
                self.peek().loc,
                "statements not allowed outside function body"
            );
        }

        if self.match_token(&[TokenType::KeywordLet]) {
            self.let_declaration()
        } else {
            self.statement()
        }
    }

    fn func_declaration(&mut self, exported: bool) -> Result<Stmt, ZernError> {
        let name = self.consume(TokenType::Identifier, "expected function name")?;
        self.consume(TokenType::LeftBracket, "expected '[' after function name")?;

        let mut params = vec![];
        if !self.check(&TokenType::RightBracket) {
            loop {
                let var_name = self.consume(TokenType::Identifier, "expected parameter name")?;
                self.consume(TokenType::Colon, "expected ':' after parameter name")?;

                let var_type = self.consume(TokenType::Identifier, "expected parameter type")?;
                if !TYPES.contains(&var_type.lexeme.as_str()) {
                    return error!(&name.loc, format!("unknown type: {}", var_type.lexeme));
                }

                params.push(Param { var_type, var_name });
                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightBracket, "expected ']' after arguments")?;
        self.consume(TokenType::Colon, "expected ':' after '['")?;
        let return_type = self.consume(TokenType::Identifier, "expected return type")?;
        if !TYPES.contains(&return_type.lexeme.as_str()) {
            return error!(&name.loc, format!("unknown type: {}", return_type.lexeme));
        }

        self.is_inside_function = true;
        let body = Box::new(self.block()?);
        self.is_inside_function = false;

        Ok(Stmt::Function {
            name,
            params,
            return_type,
            body,
            exported,
        })
    }

    fn let_declaration(&mut self) -> Result<Stmt, ZernError> {
        let name = self.consume(TokenType::Identifier, "expected variable name")?;
        self.consume(TokenType::Colon, "expected ':' after variable name")?;

        let var_type = self.consume(TokenType::Identifier, "expected variable type")?;
        if !TYPES.contains(&var_type.lexeme.as_str()) {
            return error!(&name.loc, format!("unknown type: {}", var_type.lexeme));
        }

        self.consume(TokenType::Equal, "expected '=' after variable type")?;
        let initializer = self.expression()?;
        Ok(Stmt::Let {
            name,
            var_type,
            initializer,
        })
    }

    fn extern_declaration(&mut self) -> Result<Stmt, ZernError> {
        Ok(Stmt::Extern(
            self.consume(TokenType::Identifier, "expected extern name")?,
        ))
    }

    fn block(&mut self) -> Result<Stmt, ZernError> {
        self.consume(TokenType::Indent, "expected an indent")?;

        let mut statements = vec![];
        while !self.eof() && !self.match_token(&[TokenType::Dedent]) {
            statements.push(self.declaration()?);
        }

        Ok(Stmt::Block(statements))
    }

    fn statement(&mut self) -> Result<Stmt, ZernError> {
        if self.match_token(&[TokenType::KeywordIf]) {
            self.if_statement()
        } else if self.match_token(&[TokenType::KeywordWhile]) {
            self.while_statement()
        } else if self.match_token(&[TokenType::KeywordFor]) {
            self.for_statement()
        } else if self.match_token(&[TokenType::KeywordReturn]) {
            Ok(Stmt::Return(self.expression()?))
        } else if self.match_token(&[TokenType::KeywordBreak]) {
            Ok(Stmt::Break)
        } else if self.match_token(&[TokenType::KeywordContinue]) {
            Ok(Stmt::Continue)
        } else {
            Ok(Stmt::Expression(self.expression()?))
        }
    }

    fn if_statement(&mut self) -> Result<Stmt, ZernError> {
        let condition = self.expression()?;
        let then_branch = self.block()?;
        let else_branch = if self.match_token(&[TokenType::KeywordElse]) {
            if self.match_token(&[TokenType::KeywordIf]) {
                Box::new(self.if_statement()?)
            } else {
                Box::new(self.block()?)
            }
        } else {
            Box::new(Stmt::Block(vec![]))
        };
        Ok(Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    fn while_statement(&mut self) -> Result<Stmt, ZernError> {
        let condition = self.expression()?;
        let body = self.block()?;
        Ok(Stmt::While {
            condition,
            body: Box::new(body),
        })
    }

    fn for_statement(&mut self) -> Result<Stmt, ZernError> {
        let var = self.consume(TokenType::Identifier, "expected variable name after 'for'")?;
        self.consume(TokenType::KeywordIn, "expected 'in' after variable name")?;
        let start = self.expression()?;
        self.consume(TokenType::DoubleDot, "expected '..' after the number")?;
        let end = self.expression()?;

        let body = self.block()?;
        Ok(Stmt::For {
            var,
            start,
            end,
            body: Box::new(body),
        })
    }

    fn expression(&mut self) -> Result<Expr, ZernError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ZernError> {
        let expr = self.pipe()?;

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

    fn pipe(&mut self) -> Result<Expr, ZernError> {
        let mut expr = self.or_and()?;

        while self.match_token(&[TokenType::Pipe]) {
            let pipe = self.previous().clone();
            let right = self.equality()?;

            match right {
                Expr::Call {
                    callee,
                    paren,
                    args,
                } => {
                    let mut new_args = args;
                    new_args.insert(0, expr);
                    expr = Expr::Call {
                        callee,
                        paren,
                        args: new_args,
                    }
                }
                _ => {
                    return error!(pipe.loc, "tried to pipe into a non-call expression");
                }
            };
        }

        Ok(expr)
    }

    fn or_and(&mut self) -> Result<Expr, ZernError> {
        let mut expr = self.equality()?;

        while self.match_token(&[TokenType::BitOr, TokenType::BitAnd]) {
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

    fn equality(&mut self) -> Result<Expr, ZernError> {
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

    fn comparison(&mut self) -> Result<Expr, ZernError> {
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

    fn term(&mut self) -> Result<Expr, ZernError> {
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

    fn factor(&mut self) -> Result<Expr, ZernError> {
        let mut expr = self.unary()?;

        while self.match_token(&[
            TokenType::Star,
            TokenType::Slash,
            TokenType::Mod,
            TokenType::ShiftLeft,
            TokenType::ShiftRight,
        ]) {
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

    fn unary(&mut self) -> Result<Expr, ZernError> {
        if self.match_token(&[TokenType::At]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::AddrOf {
                op,
                expr: Box::new(right),
            });
        }
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                op,
                right: Box::new(right),
            });
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, ZernError> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&[TokenType::LeftParen]) {
                let mut args = vec![];
                if !self.check(&TokenType::RightParen) {
                    loop {
                        args.push(self.expression()?);
                        if !self.match_token(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }

                let paren = self.consume(TokenType::RightParen, "expected ')' after arguments")?;

                expr = Expr::Call {
                    callee: Box::new(expr),
                    paren,
                    args,
                };
            } else if self.match_token(&[TokenType::LeftBracket]) {
                let index = self.expression()?;
                self.consume(TokenType::RightBracket, "expected ']' after index")?;
                expr = Expr::Index {
                    expr: Box::new(expr),
                    index: Box::new(index),
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ZernError> {
        if self.match_token(&[
            TokenType::Number,
            TokenType::Char,
            TokenType::String,
            TokenType::True,
            TokenType::False,
        ]) {
            Ok(Expr::Literal(self.previous().clone()))
        } else if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "expected ')' after expression")?;
            Ok(Expr::Grouping(Box::new(expr)))
        } else if self.match_token(&[TokenType::LeftBracket]) {
            let mut xs = vec![];
            if !self.check(&TokenType::RightBracket) {
                loop {
                    xs.push(self.expression()?);
                    if !self.match_token(&[TokenType::Comma]) {
                        break;
                    }
                }
            }
            self.consume(TokenType::RightBracket, "expected ']' after values")?;

            Ok(Expr::ArrayLiteral(xs))
        } else if self.match_token(&[TokenType::Identifier]) {
            Ok(Expr::Variable(self.previous().clone()))
        } else {
            error!(self.peek().loc, "expected expression")
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<Token, ZernError> {
        if self.check(&token_type) {
            self.current += 1;
            Ok(self.previous().clone())
        } else {
            error!(self.previous().loc, format!("{}", message))
        }
    }

    fn match_token(&mut self, token_types: &[TokenType]) -> bool {
        for x in token_types {
            if self.check(x) {
                self.current += 1;
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
