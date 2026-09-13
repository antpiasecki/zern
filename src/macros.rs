use crate::tokenizer::{Loc, Token, TokenType, ZernError, error};
use std::collections::HashMap;

struct TextMacro {
    params: Vec<String>,
    body: Vec<Token>,
}

pub struct MacroExpander {
    macros: HashMap<String, TextMacro>,
}

impl MacroExpander {
    pub fn new() -> Self {
        MacroExpander { macros: HashMap::new() }
    }

    pub fn expand(&mut self, tokens: Vec<Token>) -> Result<Vec<Token>, ZernError> {
        let mut current = self.collect_textmacros(tokens);
        for _ in 0..64 {
            let (next, changed) = self.expand_invocations(current);
            current = next;
            if !changed {
                return Ok(current);
            }
        }
        return error!(Loc::default(), "macro expansion limit exceeded");
    }

    fn collect_textmacros(&mut self, tokens: Vec<Token>) -> Vec<Token> {
        let mut out = Vec::new();
        let mut i = 0;
        while i < tokens.len() {
            if tokens[i].token_type == TokenType::KeywordTextmacro {
                i += 1;
                let name = tokens[i].lexeme.clone();
                i += 1;
                i += 1; // skip '('
                let mut params = Vec::new();
                while tokens[i].token_type != TokenType::RightParen {
                    if tokens[i].token_type == TokenType::Identifier {
                        params.push(tokens[i].lexeme.clone());
                    }
                    i += 1;
                }
                i += 1; // skip ')'
                let mut body = Vec::new();
                while tokens[i].token_type != TokenType::KeywordEndmacro {
                    body.push(tokens[i].clone());
                    i += 1;
                }
                i += 1; // skip 'endmacro'
                self.macros.insert(name, TextMacro { params, body });
            } else {
                out.push(tokens[i].clone());
                i += 1;
            }
        }
        out
    }

    fn expand_body(&self, body: &[Token], bindings: &HashMap<String, Vec<Token>>, loc: &Loc) -> Vec<Token> {
        let mut out: Vec<Token> = Vec::new();
        let mut i = 0;
        while i < body.len() {
            let piece = |lexeme: &str| match bindings.get(lexeme) {
                Some(arg) => arg.iter().map(|t| t.lexeme.clone()).collect::<Vec<_>>().join(""),
                None => lexeme.to_string(),
            };

            if body[i].token_type == TokenType::Hash {
                // glue previous emitted token with the next token
                if let Some(prev) = out.pop() {
                    let next = &body[i + 1];
                    let merged = format!("{}{}", prev.lexeme, piece(&next.lexeme));
                    out.push(Token {
                        token_type: if !&merged.is_empty() && merged.bytes().all(|b| b.is_ascii_digit()) {
                            TokenType::IntLiteral
                        } else {
                            TokenType::Identifier
                        },
                        lexeme: merged,
                        loc: loc.clone(),
                        orig_loc: Some(body[i + 1].loc.clone()),
                    });
                    i += 2; // skip '#' and the next token
                    continue;
                }
            }

            if body[i].token_type == TokenType::Identifier {
                if let Some(arg) = bindings.get(&body[i].lexeme) {
                    for t in arg {
                        out.push(Token {
                            token_type: t.token_type.clone(),
                            lexeme: t.lexeme.clone(),
                            loc: loc.clone(),
                            orig_loc: Some(t.loc.clone()),
                        });
                    }
                    i += 1;
                    continue;
                }
            }

            out.push(Token {
                token_type: body[i].token_type.clone(),
                lexeme: piece(&body[i].lexeme),
                loc: loc.clone(),
                orig_loc: Some(body[i].loc.clone()),
            });
            i += 1;
        }
        out
    }

    fn expand_invocations(&self, tokens: Vec<Token>) -> (Vec<Token>, bool) {
        let mut out = Vec::new();
        let mut changed = false;
        let mut i = 0;
        while i < tokens.len() {
            if let Some(def) = self.macros.get(&tokens[i].lexeme) {
                changed = true;
                let loc = tokens[i].loc.clone();
                i += 1;
                i += 1; // skip '('
                let mut args: Vec<Vec<Token>> = vec![Vec::new()];
                let mut depth = 0;

                while !(tokens[i].token_type == TokenType::RightParen && depth == 0) {
                    match tokens[i].token_type {
                        TokenType::LeftParen => {
                            depth += 1;
                            args.last_mut().unwrap().push(tokens[i].clone());
                        }
                        TokenType::RightParen => {
                            depth -= 1;
                            args.last_mut().unwrap().push(tokens[i].clone());
                        }
                        TokenType::Comma if depth == 0 => args.push(Vec::new()),
                        _ => args.last_mut().unwrap().push(tokens[i].clone()),
                    }
                    i += 1;
                }
                i += 1; // skip ')'
                let bindings: HashMap<String, Vec<Token>> = def.params.iter().cloned().zip(args.into_iter()).collect();
                out.extend(self.expand_body(&def.body, &bindings, &loc));
            } else {
                out.push(tokens[i].clone());
                i += 1;
            }
        }
        (out, changed)
    }
}
