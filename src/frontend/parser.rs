use super::{tokenizer::{Token, TokenKind, Tokenizer}, AstExpr, AstStatement, AstType};

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
            _ => Err(format!("Unexpected token at {}", token.loc)),
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
                return Err(format!("Expected identifier at {}", token.loc));
            }
        } else {
            return Err(format!("Expected identifier at {}", start_loc));
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
            return Err(format!("Expected '=' at {}", self.peek().unwrap().loc));
        }
        // Parse the expression
        let expr = match self.parse_expr() {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };
        // Expect a semicolon
        if let None = self.expect(TokenKind::Semicolon) {
            return Err(format!("Expected ';' at {}", self.peek().unwrap().loc));
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
