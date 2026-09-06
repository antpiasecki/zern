use crate::tokenizer::{Token, TokenType};
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

    pub fn expand(&mut self, tokens: Vec<Token>) -> Vec<Token> {
        let stripped = self.collect_textmacros(tokens);
        self.expand_invocations(stripped)
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

    fn expand_body(&self, body: &[Token], bindings: &HashMap<String, String>) -> Vec<Token> {
        let mut out: Vec<Token> = Vec::new();
        let mut i = 0;
        while i < body.len() {
            let piece = |lexeme: &str| bindings.get(lexeme).cloned().unwrap_or_else(|| lexeme.to_string());

            if body[i].token_type == TokenType::Hash {
                // glue previous emitted token with the next token
                if let Some(prev) = out.pop() {
                    let next = &body[i + 1];
                    let merged = format!("{}{}", prev.lexeme, piece(&next.lexeme));
                    out.push(Token {
                        token_type: TokenType::Identifier,
                        lexeme: merged,
                        loc: prev.loc.clone(),
                    });
                    i += 2; // skip '#' and the next token
                    continue;
                }
            }

            out.push(Token {
                lexeme: piece(&body[i].lexeme),
                ..body[i].clone()
            });
            i += 1;
        }
        out
    }

    fn expand_invocations(&self, tokens: Vec<Token>) -> Vec<Token> {
        let mut out = Vec::new();
        let mut i = 0;
        while i < tokens.len() {
            if let Some(def) = self.macros.get(&tokens[i].lexeme) {
                i += 1;
                i += 1; // skip '('
                let mut args = vec![String::new()];
                while tokens[i].token_type != TokenType::RightParen {
                    if tokens[i].token_type == TokenType::Comma {
                        args.push(String::new());
                    } else {
                        args.last_mut().unwrap().push_str(&tokens[i].lexeme);
                    }
                    i += 1;
                }
                i += 1; // skip ')'
                let bindings: HashMap<String, String> = def.params.iter().cloned().zip(args.into_iter()).collect();
                out.extend(self.expand_body(&def.body, &bindings));
            } else {
                out.push(tokens[i].clone());
                i += 1;
            }
        }
        out
    }
}
