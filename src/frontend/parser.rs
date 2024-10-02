use std::iter::Peekable;
use std::slice::Iter;

use super::{tokenizer::{Token, TokenKind, Tokenizer}, AstExpr, AstStatement, AstType, BinOpKind};

struct TokenIter<'src> {
    tokens: Peekable<Iter<'src, Token<'src>>>,
    prev: Option<&'src Token<'src>>,
}

impl<'src> From<&'src Vec<Token<'src>>> for TokenIter<'src> {
    fn from(tokens: &'src Vec<Token<'src>>) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
            prev: None,
        }
    }
}

fn peek_back<'src>(iter: &'src TokenIter) -> Option<&'src Token<'src>> {
    iter.prev
}

fn peek<'src>(iter: &'src mut TokenIter) -> Option<&'src Token<'src>> {
    iter.tokens.peek().copied()
}

fn advance<'src>(iter: &'src mut TokenIter) -> Option<&'src Token<'src>> {
    iter.prev = iter.tokens.next();
    iter.prev
}

fn expect<'src>(iter: &'src mut TokenIter, kind: TokenKind) -> Option<&'src Token<'src>> {
    if let Some(token) = peek(iter) {
        if token.kind == kind {
            return advance(iter);
        }
    }
    None
}

fn expect_or_err<'src>(iter: &'src mut TokenIter, kind: TokenKind) -> Result<&'src Token<'src>, &'src Token<'src>> {
    if let Some(token) = iter.tokens.peek() {
        if token.kind == kind {
            return Ok(iter.tokens.next().unwrap());
        } else {
            return Err(token);
        }
    } else {
        return Err(iter.prev.unwrap());
    }
}

fn parse_statement<'src>(iter: &'src mut TokenIter) -> Result<AstStatement, String> {
    let token = if let Some(token) = peek(iter) {
        token
    } else {
        return Err(format!("Unexpected EOF at {}", peek_back(iter).unwrap().loc));
    };
    match token.kind {
        TokenKind::Const => parse_var_decl(iter, false),
        TokenKind::Let => parse_var_decl(iter, true),
        TokenKind::Return => parse_return(iter),
        _ => Err(format!("Unexpected token {} at {}", token.lexeme.unwrap_or("None"), token.loc)),
    }
}

fn parse_var_decl<'src>(iter: &'src mut TokenIter, mutable: bool) -> Result<AstStatement, String> {
    let start_loc = advance(iter).unwrap().loc;
    let ident = match expect_or_err(iter, TokenKind::Ident) {
        Ok(token) => token.lexeme.unwrap().to_string(),
        Err(token) => return Err(format!("Expected identifier at {} got {:?}", token.loc, token.lexeme)),
    };
    let ty = if let Some(_) = expect(iter, TokenKind::Colon) {
        match parse_type(iter) {
            Ok(ty) => Some(ty),
            Err(e) => return Err(e),
        }
    } else {
        None
    };
    if let Err(token) = expect_or_err(iter, TokenKind::Eq) {
        return Err(format!("Expected '=' at {} got {:?}", token.loc, token.lexeme));
    }
    let expr = match parse_expr(iter) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
    };
    if let Err(token) = expect_or_err(iter, TokenKind::Semicolon) {
        return Err(format!("Expected ';' at {} got {:?}", token.loc, token.lexeme));
    }
    if mutable {
        Ok(AstStatement::LetDef { 
            name: ident, 
            ty, 
            value: expr, 
            loc: start_loc
        })
    } else {
        Ok(AstStatement::ConstDef {
            name: ident,
            ty,
            value: expr,
            loc: start_loc,
        })
    }
}

fn parse_return<'src>(iter: &'src mut TokenIter) -> Result<AstStatement, String> {
    let start_loc = advance(iter).unwrap().loc;
    match peek(iter) {
        Some(token) if token.kind == TokenKind::Semicolon => {
            advance(iter);
            Ok(AstStatement::Return { expr: None, loc: start_loc })
        }
        Some(_) => {
            match parse_expr(iter) {
                Ok(expr) => {
                    if let Err(token) = expect_or_err(iter, TokenKind::Semicolon) {
                        return Err(format!("Expected ';' at {} got {:?}", token.loc, token.lexeme));
                    }
                    Ok(AstStatement::Return { expr: Some(expr), loc: start_loc })
                }
                Err(e) => Err(e),
            }
        }
        None => Err(format!("Unexpected EOF at {}", start_loc)),
    }
}

fn parse_type<'src>(iter: &'src mut TokenIter) -> Result<AstType, String> {
    unimplemented!();
}

fn parse_expr<'src>(iter: &'src mut TokenIter) -> Result<AstExpr, String> {
    unimplemented!();
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

fn peek_binop<'src>(iter: &'src mut TokenIter) -> Option<(&'src Token<'src>, u8)> {
    if let Some(token) = peek(iter) {
        let prec = binop_prec(token.kind);
        if prec > 0 {
            return Some((token, prec));
        }
    }
    None
}

fn parse_primary_expr<'src>(iter: &'src mut TokenIter) -> Result<AstExpr, String> {
    let token = if let Some(token) = advance(iter) {
        token
    } else {
        return Err(format!("Unexpected EOF at {}", peek_back(iter).unwrap().loc));
    };
    let start_loc = token.loc;
    match token.kind {
        TokenKind::Ident => {
            if let Some(lexeme) = token.lexeme {
                let mut expr = AstExpr::Ident { name: lexeme.to_string(), loc: start_loc };
                return parse_postfix(iter, &mut expr)
            } else {
                Err(format!("Expected identifier at {} got {:?}", start_loc, token.lexeme))
            }
        }
        TokenKind::Number => {
            if let Some(lexeme) = token.lexeme {
                if let Ok(value) = lexeme.parse::<i64>() {
                    let mut expr = AstExpr::Int { value, loc: start_loc };
                    return parse_postfix(iter, &mut expr)
                } else if let Ok(value) = lexeme.parse::<f64>() {
                    let mut expr = AstExpr::Float { value, loc: start_loc };
                    return parse_postfix(iter, &mut expr)
                } else {
                    return Err(format!("Invalid number literal at {} got {:?}", start_loc, token.lexeme));
                }
            } else {
                Err(format!("Expected number literal at {} got {:?}", start_loc, token.lexeme))
            }
        }
        TokenKind::Lparen => {
            match parse_expr(iter) {
                Ok(expr) => {
                    if let Err(token) = expect_or_err(iter, TokenKind::Rparen) {
                        return Err(format!("Expected ')' at {} got {:?}", token.loc, token.lexeme));
                    }
                    Ok(expr)
                }
                Err(e) => Err(e),
            }
        }
        TokenKind::String => {
            if let Some(lexeme) = token.lexeme {
                let mut lhs = AstExpr::String { value: lexeme.to_string(), loc: start_loc };
                return parse_postfix(iter, &mut lhs)
            } else {
                Err(format!("Expected string literal at {} got {:?}", start_loc, token.lexeme))
            }
        }
        // TODO: implmeent all remaining primary expressions
        _ => Err(format!("Unexpected token at {} got {:?}", start_loc, token.lexeme)),
    }
}


/// Checks for postfix expressions like function calls, array indexing, etc. then passes expr to parse_binop_expr
fn parse_postfix<'src>(iter: &'src mut TokenIter, lhs: &mut AstExpr) -> Result<AstExpr, String> {
    // TODO: implement postfix expressions
    unimplemented!();
}

fn parse_binop_expr<'src>(iter: &'src mut TokenIter, lhs: &mut AstExpr, prec: u8) -> Result<AstExpr, String> {
    // TODO: implement binary operator parsing
    unimplemented!();
}