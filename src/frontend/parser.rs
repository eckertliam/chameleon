use std::collections::HashMap;

use super::{tokenizer::{Token, TokenKind, Tokenizer}, 
    Expr, Stmt, Type, BinOpKind, UnOpKind};

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

// Expression parsing
// The expression parser is a simple Pratt parser 
// Inspired by https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/

// parse an expression with a given precedence
fn expr(parser: &mut Parser, precedence: u8) -> Expr {
    let token = parser.consume();
    let mut lhs = prefix(parser, &token);
    
    while get_prec(parser.peek().kind) > precedence {
        let token = parser.consume();
        lhs = infix(parser, lhs, &token);
    }

    lhs
}

fn prefix(parser: &mut Parser, token: &Token) -> Expr {
    match token.kind {
        TokenKind::Ident => ident(parser, token),
        TokenKind::Number => number(parser, token),
        TokenKind::String => string(parser, token),
        TokenKind::True => Expr::Bool {
            value: true,
            loc: token.loc,
        },
        TokenKind::False => Expr::Bool {
            value: false,
            loc: token.loc,
        },
        TokenKind::Lbracket => array(parser, token),
        TokenKind::Minus => prefix_minus(parser, token),
        TokenKind::Bang => prefix_bang(parser, token),
        TokenKind::Lbrace => block_expr(parser, token),
        _ => panic!("Unexpected token: {:?} at {}", token.kind, token.loc),
    }
}

// parse an identifier
fn ident(parser: &mut Parser, token: &Token) -> Expr {
    let name = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    Expr::Ident {
        name,
        loc: token.loc,
    }
}

// parse an array
fn array(parser: &mut Parser, token: &Token) -> Expr {
    let mut values = Vec::new();
    while !parser.is_at_end() && parser.peek().kind != TokenKind::Rbracket {
        values.push(expr(parser, 0));
        if parser.peek().kind == TokenKind::Comma {
            parser.consume();
        }
    }
    if parser.consume().kind != TokenKind::Rbracket {
        panic!("Expected ] at {}", parser.peek().loc);
    }
    let loc = token.loc;
    Expr::Array {
        values,
        loc,
    }
}

fn block_expr(parser: &mut Parser, token: &Token) -> Expr {
    let stmts = parse_block(parser, token);
    let loc = token.loc;
    Expr::Block { stmts, loc }
}

// parse a number
// return an Int or Float depending on what can be parsed
fn number(parser: &mut Parser, token: &Token) -> Expr {
    let value = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    if let Ok(int_value) = value.parse::<i64>() {
        Expr::Int {
            value: int_value,
            loc: token.loc,
        }
    } else if let Ok(float_value) = value.parse::<f64>() {
        Expr::Float {
            value: float_value,
            loc: token.loc,
        }
    } else {
        panic!("Expected a number at {} got {}", token.loc, value);
    }
}

fn string(parser: &mut Parser, token: &Token) -> Expr {
    let value = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    Expr::String {
        value,
        loc: token.loc,
    }
}

// utility macro for defining unary operators
macro_rules! define_unary_op {
    ($func_id:ident, $op_kind:expr) => {
        fn $func_id(parser: &mut Parser, token: &Token) -> Expr {
            Expr::UnOp {
                kind: $op_kind,
                expr: Box::new(expr(parser, 0)),
                loc: token.loc,
            }
        }
    }
}

define_unary_op!(prefix_minus, UnOpKind::Neg);
define_unary_op!(prefix_bang, UnOpKind::Not);
define_unary_op!(prefix_not, UnOpKind::Not);


// parse an infix expression
fn infix(parser: &mut Parser, lhs: Expr, token: &Token) -> Expr {
    match token.kind {
        TokenKind::Plus => infix_plus(parser, lhs, token),
        TokenKind::Minus => infix_minus(parser, lhs, token),
        TokenKind::Star => infix_star(parser, lhs, token),
        TokenKind::Slash => infix_slash(parser, lhs, token),
        TokenKind::Percent => infix_percent(parser, lhs, token),
        TokenKind::AmperAmper => infix_and(parser, lhs, token),
        TokenKind::PipePipe => infix_or(parser, lhs, token),
        TokenKind::Caret => infix_xor(parser, lhs, token),
        TokenKind::Amper => infix_bit_and(parser, lhs, token),
        TokenKind::Pipe => infix_bit_or(parser, lhs, token),
        TokenKind::LtLt => infix_shl(parser, lhs, token),
        TokenKind::GtGt => infix_shr(parser, lhs, token),
        TokenKind::EqEq => infix_eq(parser, lhs, token),
        TokenKind::BangEq => infix_ne(parser, lhs, token),
        TokenKind::Lt => infix_lt(parser, lhs, token),
        TokenKind::Lte => infix_lte(parser, lhs, token),
        TokenKind::Gt => infix_gt(parser, lhs, token),
        TokenKind::Gte => infix_gte(parser, lhs, token),
        TokenKind::Lparen => infix_call(parser, lhs, token),
        TokenKind::Lbracket => infix_index(parser, lhs, token),
        _ => panic!("Unexpected token: {:?} at {}", token.kind, token.loc),
    }
}

macro_rules! define_binary_op {
    ($func_id:ident, $op_kind:expr) => {
        fn $func_id(parser: &mut Parser, lhs: Expr, token: &Token) -> Expr {
            Expr::BinOp {
                kind: $op_kind,
                lhs: Box::new(lhs),
                rhs: Box::new(expr(parser, get_prec(token.kind))),
                loc: token.loc,
            }
        }
    };
}

define_binary_op!(infix_plus, BinOpKind::Add);
define_binary_op!(infix_minus, BinOpKind::Sub);
define_binary_op!(infix_star, BinOpKind::Mul);
define_binary_op!(infix_slash, BinOpKind::Div);
define_binary_op!(infix_percent, BinOpKind::Mod);
define_binary_op!(infix_and, BinOpKind::And);
define_binary_op!(infix_or, BinOpKind::Or);
define_binary_op!(infix_xor, BinOpKind::Xor);
define_binary_op!(infix_bit_or, BinOpKind::BitOr);
define_binary_op!(infix_bit_and, BinOpKind::BitAnd);
define_binary_op!(infix_shl, BinOpKind::Shl);
define_binary_op!(infix_shr, BinOpKind::Shr);
define_binary_op!(infix_eq, BinOpKind::Eq);
define_binary_op!(infix_ne, BinOpKind::Ne);
define_binary_op!(infix_lt, BinOpKind::Lt);
define_binary_op!(infix_lte, BinOpKind::Le);
define_binary_op!(infix_gt, BinOpKind::Gt);
define_binary_op!(infix_gte, BinOpKind::Ge);

fn infix_call(parser: &mut Parser, lhs: Expr, token: &Token) -> Expr {
    // parse arguments
    let mut args = Vec::new();
    while !parser.is_at_end() && parser.peek().kind != TokenKind::Rparen {
        args.push(expr(parser, 0));
        if parser.peek().kind == TokenKind::Comma {
            parser.consume();
        }
    }

    // check for )
    if parser.consume().kind != TokenKind::Rparen {
        panic!("Expected ) at {}", parser.peek().loc);
    }

    let loc = lhs.loc();
    Expr::Call {
        callee: Box::new(lhs),
        args,
        loc,
    }
}

fn infix_index(parser: &mut Parser, lhs: Expr, token: &Token) -> Expr {
    let index = expr(parser, 0);
    if parser.consume().kind != TokenKind::Rbracket {
        panic!("Expected ] at {}", parser.peek().loc);
    }
    let loc = lhs.loc();
    Expr::Index {
        array: Box::new(lhs),
        index: Box::new(index),
        loc,
    }
}

// returns the precedence of the operator
fn get_prec(kind: TokenKind) -> u8 {
    match kind {
        TokenKind::PipePipe => 1,
        TokenKind::AmperAmper => 2,
        TokenKind::Pipe => 3,
        TokenKind::Caret => 4,
        TokenKind::Amper => 5,
        TokenKind::EqEq | TokenKind::BangEq => 6,
        TokenKind::Lt | TokenKind::Lte | TokenKind::Gt | TokenKind::Gte => 7,
        TokenKind::LtLt | TokenKind::GtGt => 8,
        TokenKind::Plus | TokenKind::Minus => 9,
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => 10,
        // ( and [ for function calls and indexing
        TokenKind::Lparen | TokenKind::Lbracket => 21,
        _ => 0,
    }
}

// Statement parsing 
// Statements are parsed with simple recursive descent

// parse a top level statement
fn stmt(parser: &mut Parser) -> Stmt {
    let token = parser.consume();
    match token.kind {
        TokenKind::Return => return_stmt(parser, token),
        TokenKind::Let => var_decl(parser, token, true),
        TokenKind::Const => var_decl(parser, token, false),
        TokenKind::Lbrace => block_stmt(parser, token),
        TokenKind::If => if_stmt(parser, token),
        _ => Stmt::Expr(expr(parser, 0)),
    }
}

fn return_stmt(parser: &mut Parser, token: Token) -> Stmt {
    let expr = if parser.peek().kind != TokenKind::Semicolon {
        Some(expr(parser, 0))
    } else {
        None
    };
    // peek ahead semicolons are optional but if its there we consume it
    if parser.peek().kind == TokenKind::Semicolon {
        parser.consume();
    }

    Stmt::Return {
        expr,
        loc: token.loc,
    }
}

fn var_decl(parser: &mut Parser, token: Token, mutable: bool) -> Stmt {
    let name = if let Token { kind: TokenKind::Ident, lexeme: Some(lexeme), .. } = parser.consume() {
        lexeme.to_string()
    } else {
        panic!("Expected identifier at {}", token.loc);
    };

    // TODO: add type annotation parsing
    let ty = None;

    let value = expr(parser, 0);

    // expect semicolon
    if parser.consume().kind != TokenKind::Semicolon {
        panic!("Expected semicolon at {}", parser.peek().loc);
    }

    if mutable {
        Stmt::LetDef {
            name,
            ty,
            value,
            loc: token.loc,
        }
    } else {
        Stmt::ConstDef {
            name,
            ty,
            value,
            loc: token.loc,
        }
    }
}

fn parse_block(parser: &mut Parser, token: &Token) -> Vec<Stmt> {
    let mut stmts = Vec::new();
    while parser.peek().kind != TokenKind::Rbrace && !parser.is_at_end() {
        stmts.push(stmt(parser));
    }
    if parser.consume().kind != TokenKind::Rbrace {
        panic!("Expected }} at {}", parser.peek().loc);
    }
    stmts
}

fn block_stmt(parser: &mut Parser, token: Token) -> Stmt {
    let stmts = parse_block(parser, &token);
    Stmt::Block { stmts, loc: token.loc }
}

fn if_stmt(parser: &mut Parser, token: Token) -> Stmt {
    let cond = expr(parser, 0);
    let lbrace = parser.consume();
    if lbrace.kind != TokenKind::Lbrace {
        panic!("Expected {{ at {}", lbrace.loc);
    }
    let then_block = Box::new(block_stmt(parser, lbrace));
    let else_block = if parser.peek().kind == TokenKind::Else {
        parser.consume();
        // check for an else if
        if parser.peek().kind == TokenKind::If {
            // consume the if token
            let token = parser.consume();
            // recursively parse the if statement
            let if_stmt = Box::new(if_stmt(parser, token));
            Some(if_stmt)
        } else {
            // consume the lbrace
            let lbrace = parser.consume();
            if lbrace.kind != TokenKind::Lbrace {
                panic!("Expected {{ at {}", lbrace.loc);
            }
            // parse the else block
            let else_block = Box::new(block_stmt(parser, lbrace));
            Some(else_block)
        }
    } else {
        None
    };
    Stmt::If {
        cond,
        then_block,
        else_block,
        loc: token.loc,
    }
}

// TODO: add parsing for type annotations
// TODO: add parsing for function definitions
// TODO: add parsing for struct definitions
// TODO: add parsing for enum definitions
// TODO: add parsing for alias definitions
