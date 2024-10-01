use super::{tokenizer::{Token, TokenKind, Tokenizer}, AstExpr, AstStatement, AstType, BinOpKind};

pub struct Parser<'src> {
    tokens: &'src Vec<Token<'src>>,
    current: usize,
    /// The program contains the result of parsing tokens into statements
    /// Gets unwrapped at the end of parsing
    program: Vec<Result<AstStatement, String>>,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: &'src Vec<Token<'src>>) -> Self {
        Self {
            tokens,
            current: 0,
            program: Vec::new(),
        }
    }

    fn peek_back(&self) -> Option<&Token<'src>> {
        self.tokens.get(self.current - 1)
    }

    fn peek(&self) -> Option<&Token<'src>> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) -> Option<&Token<'src>> {
        self.current += 1;
        self.tokens.get(self.current - 1)
    }

    /// Expects the next token to be of the given kind
    /// If it is, advances the parser and returns the token
    /// Otherwise, returns None
    fn expect(&mut self, kind: TokenKind) -> Option<&Token<'src>> {
        if let Some(token) = &self.tokens.get(self.current) {
            if token.kind == kind {
                self.advance();
                return Some(token);
            }
        }
        None
    }

    fn parse_statement(&mut self) -> Result<AstStatement, String> {
        let token = if let Some(token) = self.peek() {
            token
        } else {
            return Err(format!("Unexpected EOF at {}", self.peek_back().unwrap().loc));
        };
        match token.kind {
            TokenKind::Const => self.parse_var_decl(false),
            TokenKind::Let => self.parse_var_decl(true),
            TokenKind::Return => self.parse_return(),
            _ => Err(format!("Unexpected token at {} got {:?}", token.loc, token.lexeme)),
        }
    }

    fn parse_var_decl(&mut self, mutable: bool) -> Result<AstStatement, String> {
        // Consume the const/let token taking the location
        let start_loc = self.advance().unwrap().loc;
        // Expect an identifier
        let ident = if let Some(token) = self.expect(TokenKind::Ident) {
            if let Some(lexeme) = token.lexeme {
                lexeme
            } else {
                return Err(format!("Expected identifier at {} got {:?}", token.loc, token.lexeme));
            }
        } else {
            return Err(format!("Expected identifier at {} got {:?}", start_loc, self.peek().unwrap().lexeme));
        };
        // if there is a colon the type is explicitly declared
        let ty = if let Some(_) = self.expect(TokenKind::Colon) {
            match self.parse_type() {
                Ok(ty) => Some(ty),
                Err(e) => return Err(e),
            }
        } else {
            // if there is no colon the type is inferred
            None
        };
        // Expect an equal sign
        if let None = self.expect(TokenKind::Eq) {
            return Err(format!("Expected '=' at {} got {:?}", self.peek().unwrap().loc, self.peek().unwrap().lexeme));
        }
        // Parse the expression
        let expr = match self.parse_expr() {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };
        // Expect a semicolon
        if let None = self.expect(TokenKind::Semicolon) {
            return Err(format!("Expected ';' at {} got {:?}", self.peek().unwrap().loc, self.peek().unwrap().lexeme));
        }
        if mutable {
            Ok(AstStatement::LetDef { 
                name: ident.to_string(), 
                ty, 
                value: expr, 
                loc: start_loc
            })
        } else {
            Ok(AstStatement::ConstDef {
                name: ident.to_string(),
                ty,
                value: expr,
                loc: start_loc,
            })
        }
    }

    fn parse_type(&mut self) -> Result<AstType, String> {
        unimplemented!();
    }

    fn parse_expr(&mut self) -> Result<AstExpr, String> {
        unimplemented!();
    }

    fn parse_primary_expr(&mut self) -> Result<AstExpr, String> {
        if let Some(token) = self.advance() {
            let start_loc = token.loc;
            match token.kind {
                TokenKind::Ident => {
                    if let Some(lexeme) = token.lexeme {
                        Ok(AstExpr::Ident { name: lexeme.to_string(), loc: start_loc })
                    } else {
                        Err(format!("Expected identifier at {} got {:?}", start_loc, token.lexeme))
                    }
                }
                TokenKind::Number => {
                    if let Some(lexeme) = token.lexeme {
                        if let Ok(value) = lexeme.parse::<i64>() {
                            Ok(AstExpr::Int { value, loc: start_loc })
                        } else if let Ok(value) = lexeme.parse::<f64>() {
                            Ok(AstExpr::Float { value, loc: start_loc })
                        } else {
                            Err(format!("Invalid number literal at {} got {:?}", start_loc, token.lexeme))
                        }
                    } else {
                        Err(format!("Expected number literal at {} got {:?}", start_loc, token.lexeme))
                    }
                }
                TokenKind::Lparen => {
                    match self.parse_expr() {
                        Ok(expr) => {
                            if let None = self.expect(TokenKind::Rparen) {
                                return Err(format!("Expected ')' at {} got {:?}", self.peek().unwrap().loc, self.peek().unwrap().lexeme));
                            }
                            Ok(expr)
                        }
                        Err(e) => Err(e),
                    }
                }
                TokenKind::String => {
                    if let Some(lexeme) = token.lexeme {
                        Ok(AstExpr::String { value: lexeme.to_string(), loc: start_loc })
                    } else {
                        Err(format!("Expected string literal at {} got {:?}", start_loc, token.lexeme))
                    }
                }
                _ => Err(format!("Unexpected token at {} got {:?}", start_loc, token.lexeme)),
            }
        } else {
            Err(format!("Unexpected EOF at {}", self.peek_back().unwrap().loc))
        }
    }

    fn binop_prec(kind: TokenKind) -> u8 {
        match kind {
            TokenKind::AmperAmper | TokenKind::PipePipe => 2,
            TokenKind::EqEq | TokenKind::BangEq | TokenKind::Lt | TokenKind::Lte | TokenKind::Gt | TokenKind::Gte => 5,
            TokenKind::Pipe | TokenKind::Amper | TokenKind::Caret | TokenKind::LtLt | TokenKind::GtGt => 10,
            TokenKind::Plus | TokenKind::Minus => 15,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => 20,
            _ => 0,
        }
    }

    fn peek_binop(&mut self) -> Option<(Token<'_>, u8)> {
        let token = if let Some(token) = self.tokens.get(self.current) {
            token
        } else {
            return None;
        };
        let prec = Self::binop_prec(token.kind);
        if prec == 0 {
            return None;
        } else {
            self.current += 1;
            return Some((token.clone(), prec));
        }
    }

    fn parse_binop_expr(&mut self, lhs: &mut AstExpr, prec: u8) -> Result<AstExpr, String> {
        unimplemented!();
    }

    fn parse_return(&mut self) -> Result<AstStatement, String> {
        let start_loc = self.advance().unwrap().loc;
        match self.peek() {
            Some(token) if token.kind == TokenKind::Semicolon => {
                self.advance();
                Ok(AstStatement::Return { expr: None, loc: start_loc })
            }
            Some(_) => {
                match self.parse_expr() {
                    Ok(expr) => {
                        if let None = self.expect(TokenKind::Semicolon) {
                            return Err(format!("Expected ';' at {}", self.peek().unwrap().loc));
                        }
                        Ok(AstStatement::Return { expr: Some(expr), loc: start_loc })
                    }
                    Err(e) => Err(e),
                }
            }
            None => Err(format!("Unexpected EOF at {}", start_loc)),
        }
    }
}
