use std::collections::HashMap;

use super::{tokenizer::{Token, TokenKind, Tokenizer}, 
    AstExpr, AstStatement, AstType, BinOpKind, UnOpKind};

struct Parser<'src> {
    tokens: Vec<Token<'src>>,
    pos: usize,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: Vec<Token<'src>>) -> Self {
        Self {
            tokens,
            pos: 0,
        }
    }

    fn consume(&mut self) -> Token<'src> {
        let token = self.tokens[self.pos].clone();
        self.pos += 1;
        token
    }

    fn peek(&self) -> Token<'src> {
        self.tokens[self.pos].clone()
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }
}

fn expr(parser: &mut Parser, precedence: u8) -> AstExpr {
    let token = parser.consume();
    let mut lhs = prefix(parser, &token);
    // TODO: parse infix expressions
}

fn prefix(parser: &mut Parser, token: &Token) -> AstExpr {
    match token.kind {
        TokenKind::Ident => ident(parser, token),
        TokenKind::Minus => minus(parser, token),
        TokenKind::Bang => bang(parser, token),
        _ => panic!("Unexpected token: {:?} at {}", token.kind, token.loc),
    }
}

fn ident(parser: &mut Parser, token: &Token) -> AstExpr {
    let name = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    AstExpr::Ident {
        name,
        loc: token.loc,
    }
}

macro_rules! define_unary_op {
    ($func_id:ident, $op_kind:expr) => {
        fn $func_id(parser: &mut Parser, token: &Token) -> AstExpr {
            AstExpr::UnOp {
                kind: $op_kind,
                expr: Box::new(expr(parser, 0)),
                loc: token.loc,
            }
        }
    }
}

define_unary_op!(minus, UnOpKind::Neg);
define_unary_op!(bang, UnOpKind::Not);
define_unary_op!(not, UnOpKind::Not);
