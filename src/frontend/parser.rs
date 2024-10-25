use std::fmt;

use super::{ast::*, tokenizer::{Token, TokenKind}, Loc, Tokenizer};

#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedToken {
        at: Loc,
        data: String,
    },
    ExpectedToken {
        expected: TokenKind,
        at: Loc,
        data: String,
    },
    UnexpectedEOF {
        at: Loc,
    },
    ExpectedValidNumber {
        at: Loc,
        data: String,
    },
    GenericParamError(String),
    DuplicateDefinition {
        name: String,
        original: Loc,
        new: Loc,
    },
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken { at, data } => write!(f, "Unexpected token: {} at {}", data, at),
            Self::ExpectedToken { expected, at, data } => write!(f, "Expected {} at {} but got {}", expected.as_str(), at, data),
            Self::UnexpectedEOF { at } => write!(f, "Unexpected EOF at {}", at),
            Self::ExpectedValidNumber { at, data } => write!(f, "Expected a valid number at {} but got {}", at, data),
            Self::GenericParamError(e) => write!(f, "{}", e),
            Self::DuplicateDefinition { name, original, new } => write!(f, "Duplicate definition: {} at {} and {}", name, original, new),
        }
    }
}

impl std::error::Error for ParserError {}

struct Parser<'src> {
    tokens: &'src Vec<Token<'src>>,
    pos: usize,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: &'src Vec<Token<'src>>) -> Self {
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
        self.pos >= self.tokens.len() || self.peek().kind == TokenKind::Eof
    }
}

/// attempt to unwrap a token's lexeme with a given kind if it does not have a lexeme or is not the expected kind return an error
fn unwrap_lexeme_with_kind<'src>(token: &Token<'src>, expected_kind: TokenKind) -> Result<String, ParserError> {
    if let Token { kind, lexeme: Some(lexeme), .. } = token {
        if *kind == expected_kind {
            Ok(lexeme.to_string())
        } else {
            Err(ParserError::ExpectedToken { expected: expected_kind, at: token.loc, data: token.error_data() })
        }
    } else {
        Err(ParserError::ExpectedToken { expected: expected_kind, at: token.loc, data: token.error_data() })
    }
}

/// attempt to consume a token with a given kind, if it does not match the expected kind return an error
fn expect_token<'src>(parser: &mut Parser<'src>, kind: TokenKind) -> Result<Token<'src>, ParserError> {
    let token = parser.consume();
    if token.kind != kind {
        return Err(ParserError::ExpectedToken { expected: kind, at: token.loc, data: token.error_data() });
    }
    Ok(token)
}

// Expression parsing
// The expression parser is a simple Pratt parser 
// Inspired by https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/

/// parse an expression with a given precedence
fn expr<'src>(parser: &mut Parser<'src>, token: &Token<'src>, precedence: u8) -> Result<Expr, ParserError> {
    let mut lhs = prefix(parser, token)?;

    while get_prec(parser.peek().kind) > precedence {
        let infix_token = parser.consume();
        lhs = match infix(parser, lhs, &infix_token) {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };
    }

    Ok(lhs)
}

fn prefix<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Expr, ParserError> {
    match token.kind {
        TokenKind::Ident => ident(parser, token),
        TokenKind::Number => number(parser, token),
        TokenKind::String => string(parser, token),
        TokenKind::True => Ok(Expr::Bool {
            value: true,
            loc: token.loc,
        }),
        TokenKind::False => Ok(Expr::Bool {
            value: false,
            loc: token.loc,
        }),
        TokenKind::Lbracket => array(parser, token),
        TokenKind::Minus => prefix_minus(parser, token),
        TokenKind::Bang => prefix_bang(parser, token),
        TokenKind::Lbrace => block_expr(parser, token),
        _ => Err(ParserError::UnexpectedToken { at: token.loc, data: token.error_data() }),
    }
}

/// parse an identifier
fn ident<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Expr, ParserError> {
    let name = unwrap_lexeme_with_kind(token, TokenKind::Ident)?;
    Ok(Expr::Ident {
        name,
        loc: token.loc,
    })
}

// parse an array
fn array<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Expr, ParserError> {
    let mut values = Vec::new();
    // track the last token consumed
    let mut arr_token = parser.consume();
    loop {
        let expr = expr(parser, &arr_token, 0)?;
        values.push(expr);
        arr_token = parser.consume();
        if arr_token.kind == TokenKind::Comma {
            arr_token = parser.consume();
        } else {
            break;
        }
    }
    // ensure loop exited with a ]
    if arr_token.kind != TokenKind::Rbracket {
        return Err(ParserError::UnexpectedToken { at: arr_token.loc, data: arr_token.error_data() });
    }
    let loc = token.loc;
    Ok(Expr::Array {
        values,
        loc,
    })
}

fn block_expr<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Expr, ParserError> {
    let stmts = parse_block(parser)?;
    let loc = token.loc;
    Ok(Expr::Block { stmts, loc })
}

/// parse a number
/// 
/// returns an Int or Float depending on what can be parsed
fn number<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Expr, ParserError> {
    let value = unwrap_lexeme_with_kind(token, TokenKind::Number)?;
    if let Ok(int_value) = value.parse::<i64>() {
        Ok(Expr::Int {
            value: int_value,
            loc: token.loc,
        })
    } else if let Ok(float_value) = value.parse::<f64>() {
        Ok(Expr::Float {
            value: float_value,
            loc: token.loc,
        })
    } else {
        Err(ParserError::ExpectedValidNumber { at: token.loc, data: token.error_data() })
    }
}

fn string<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Expr, ParserError> {
    let value = unwrap_lexeme_with_kind(token, TokenKind::String)?;
    Ok(Expr::String {
        value,
        loc: token.loc,
    })
}

// utility macro for defining unary operators
macro_rules! define_unary_op {
    ($func_id:ident, $op_kind:expr) => {
        fn $func_id<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Expr, ParserError> {
            let expr_token = parser.consume();
            Ok(Expr::UnOp {
                kind: $op_kind,
                expr: Box::new(expr(parser, &expr_token, 0)?),
                loc: token.loc,
            })
        }
    }
}

define_unary_op!(prefix_minus, UnOpKind::Neg);
define_unary_op!(prefix_bang, UnOpKind::Not);


/// parse an infix expression
fn infix<'src>(parser: &mut Parser<'src>, lhs: Expr, token: &Token<'src>) -> Result<Expr, ParserError> {
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
        TokenKind::Lparen => infix_call(parser, lhs),
        TokenKind::Lbracket => infix_index(parser, lhs),
        TokenKind::Dot => infix_field_access(parser, lhs),
        TokenKind::ColonColon => infix_path_access(parser, lhs),
        _ => Err(ParserError::UnexpectedToken { at: token.loc, data: token.error_data() }),
    }
}

macro_rules! define_binary_op {
    ($func_id:ident, $op_kind:expr) => {
        fn $func_id<'src>(parser: &mut Parser<'src>, lhs: Expr, token: &Token<'src>) -> Result<Expr, ParserError> {
            let rhs_token = parser.consume();
            // pass the pass the operator precedence and parse the rhs 
            let rhs = expr(parser, &rhs_token, get_prec(token.kind))?;
            Ok(Expr::BinOp {
                kind: $op_kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                loc: token.loc,
            })
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

/// parse a function call
fn infix_call<'src>(parser: &mut Parser<'src>, lhs: Expr) -> Result<Expr, ParserError> {
    // parse arguments
    let mut args = Vec::new();
    let mut infix_token = parser.consume();
    loop {
        args.push(expr(parser, &infix_token, 0)?);
        infix_token = parser.consume();
        if infix_token.kind == TokenKind::Comma {
            infix_token = parser.consume();
        } else {
            break;
        }
    }

    // ensure loop exited with a )
    expect_token(parser, TokenKind::Rparen)?;

    let loc = lhs.loc();
    Ok(Expr::Call {
        callee: Box::new(lhs),
        args,
        loc,
    })
}

/// parse an index
fn infix_index<'src>(parser: &mut Parser<'src>, lhs: Expr) -> Result<Expr, ParserError> {
    let index_token = parser.consume();
    let index = expr(parser, &index_token, 0)?;
    expect_token(parser, TokenKind::Rbracket)?;
    let loc = lhs.loc();
    Ok(Expr::Index {
        array: Box::new(lhs),
        index: Box::new(index),
        loc,
    })
}

/// parse a field access
fn infix_field_access<'src>(parser: &mut Parser<'src>, lhs: Expr) -> Result<Expr, ParserError> {
    let field_token = parser.consume();
    let rhs = expr(parser, &field_token, 0)?;
    let loc = lhs.loc();
    Ok(Expr::FieldAccess {
        expr: Box::new(lhs),
        field: Box::new(rhs),
        loc,
    })
}

/// parse a path access
fn infix_path_access<'src>(parser: &mut Parser<'src>, lhs: Expr) -> Result<Expr, ParserError> {
    let path_token = parser.consume();
    let rhs = expr(parser, &path_token, 0)?;
    let loc = lhs.loc();
    Ok(Expr::PathAccess {
        expr: Box::new(lhs),
        path: vec![rhs],
        loc,
    })
}

/// returns the precedence of the operator
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

/// parse a top level statement
fn stmt<'src>(parser: &mut Parser<'src>) -> Result<Stmt, ParserError> {
    let token = parser.consume();
    match token.kind {
        TokenKind::Return => return_stmt(parser, &token),
        TokenKind::Let => var_decl(parser, &token, true),
        TokenKind::Const => var_decl(parser, &token, false),
        TokenKind::Lbrace => block_stmt(parser, &token),
        TokenKind::If => if_stmt(parser, &token),
        TokenKind::Ident => assign_stmt(parser, &token),
        _ => {
            let expr = expr(parser, &token, 0)?;
            expect_token(parser, TokenKind::Semicolon)?;
            Ok(Stmt::Expr(expr))
        }
    }
}

/// parse an assignment statement
fn assign_stmt<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Stmt, ParserError> {
    let var = expr(parser, &token, 0)?;
    let loc = token.loc;
    match parser.consume().kind {
        TokenKind::Eq => {
            let value_token = parser.consume();
            let value = expr(parser, &value_token, 0)?;
            expect_token(parser, TokenKind::Semicolon)?;
            Ok(Stmt::Assign { 
                var,
                value,
                loc,
            })
        }
        TokenKind::Semicolon => Ok(Stmt::Expr(var)),
        _ => Err(ParserError::UnexpectedToken { at: token.loc, data: token.error_data() }),
    }
}

/// parse a return statement
fn return_stmt<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Stmt, ParserError> {
    let expr_token = parser.consume();
    let expr = if expr_token.kind != TokenKind::Semicolon {
        Some(expr(parser, &expr_token, 0)?)
    } else {
        None
    };

    Ok(Stmt::Return {
        expr,
        loc: token.loc,
    })
}

/// parse a variable declaration
fn var_decl<'src>(parser: &mut Parser<'src>, token: &Token<'src>, mutable: bool) -> Result<Stmt, ParserError> {
    let loc = token.loc;
    let name = unwrap_lexeme_with_kind(&parser.consume(), TokenKind::Ident)?;

    // TODO: add type annotation parsing
    let ty = if parser.peek().kind == TokenKind::Colon {
        parser.consume();
        Some(parse_type_annotation(parser)?)
    } else {
        None
    };

    let value_token = parser.consume();
    let value = expr(parser, &value_token, 0)?;

    // expect semicolon
    expect_token(parser, TokenKind::Semicolon)?;

    if mutable {
        Ok(Stmt::LetDef {
            name,
            ty,
            value,
            loc,
        })
    } else {
        Ok(Stmt::ConstDef {
            name,
            ty,
            value,
            loc,
        })
    }
}

/// parse a block
fn parse_block<'src>(parser: &mut Parser<'src>) -> Result<Vec<Stmt>, ParserError> {
    let mut stmts = Vec::new();
    while parser.peek().kind != TokenKind::Rbrace && !parser.is_at_end() {
        stmts.push(stmt(parser)?);
    }
    expect_token(parser, TokenKind::Rbrace)?;
    Ok(stmts)
}

/// parse a block
/// 
/// where the token is the opening {
fn block_stmt<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Stmt, ParserError> {
    let stmts = parse_block(parser)?;
    Ok(Stmt::Block { stmts, loc: token.loc })
}

/// parse an if statement
fn if_stmt<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Stmt, ParserError> {
    let cond_token = parser.consume();
    let cond = expr(parser, &cond_token, 0)?;
    let lbrace = expect_token(parser, TokenKind::Lbrace)?;
    let then_block = Box::new(block_stmt(parser, &lbrace)?);
    let else_block = if parser.peek().kind == TokenKind::Else {
        parser.consume();
        // check for an else if
        if parser.peek().kind == TokenKind::If {
            // consume the if token
            let token = parser.consume();
            // recursively parse the if statement
            let if_stmt = Box::new(if_stmt(parser, &token)?);
            Some(if_stmt)
        } else {
            let lbrace = parser.consume();
            if lbrace.kind != TokenKind::Lbrace {
                return Err(ParserError::ExpectedToken { expected: TokenKind::Lbrace, at: lbrace.loc, data: lbrace.error_data() });
            }
            // parse the else block
            let else_block = Box::new(block_stmt(parser, &lbrace)?);
            Some(else_block)
        }
    } else {
        None
    };
    Ok(Stmt::If {
        cond,
        then_block,
        else_block,
        loc: token.loc,
    })
}

// type annotation parsing

/// parse a type annotation
/// 
/// expects to start with TokenKind Ident, Lbracket, or Lparen
/// will return an error if it doesn't
fn parse_type_annotation<'src>(parser: &mut Parser<'src>) -> Result<TypeExpr, ParserError> {
    let token = parser.consume();
    // will either start with TokenKind Ident, Lbracket, or Lparen
    match token.kind {
        TokenKind::Ident => {
            let name = unwrap_lexeme_with_kind(&token, TokenKind::Ident)?;
            // check for generic args
            if parser.peek().kind == TokenKind::Lt {
                // consume the <
                let token = parser.consume();
                let generic_args = parse_generic_args(parser)?;
                return Ok(TypeExpr::Generic(name, generic_args, token.loc))
            } 
            // match on the primitive type
            // otherwise it's an alias
            let ty =match name.as_str() {
                "i8" => TypeExpr::I8(token.loc),
                "i16" => TypeExpr::I16(token.loc),
                "i32" => TypeExpr::I32(token.loc),
                "i64" => TypeExpr::I64(token.loc),
                "u8" => TypeExpr::U8(token.loc),
                "u16" => TypeExpr::U16(token.loc),
                "u32" => TypeExpr::U32(token.loc),
                "u64" => TypeExpr::U64(token.loc),
                "f32" => TypeExpr::F32(token.loc),
                "f64" => TypeExpr::F64(token.loc),
                "bool" => TypeExpr::Bool(token.loc),
                "void" => TypeExpr::Void(token.loc),
                _ => TypeExpr::Alias(name, token.loc),
            };
            Ok(ty)
        }
        TokenKind::Lbracket => parse_array_type(parser, &token),
        TokenKind::Lparen => parse_tuple_type(parser, &token),
        _ => Err(ParserError::UnexpectedToken { at: token.loc, data: token.error_data() }),
    }
}

/// parse an array type
/// 
/// errors if it doesn't close with a ]
fn parse_array_type<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<TypeExpr, ParserError> {
    let loc = token.loc;
    let elem_type = parse_type_annotation(parser)?;
    // expect a semicolon
    expect_token(parser, TokenKind::Semicolon)?;
    // parse the size
    let size_token = parser.consume();
    let size = expr(parser, &size_token, 0)?;

    Ok(TypeExpr::Array(Box::new(elem_type), size, loc))
}


/// parse a tuple type
/// 
/// errors if it doesn't close with a )
fn parse_tuple_type<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<TypeExpr, ParserError> {
    let loc = token.loc;
    let mut types = Vec::new();
    while parser.peek().kind != TokenKind::Rparen {
        types.push(parse_type_annotation(parser)?);
    }
    expect_token(parser, TokenKind::Rparen)?;
    Ok(TypeExpr::Tuple(types, loc))
}

/// parse a generic argument list
/// 
/// errors if it doesn't close with a >
fn parse_generic_args<'src>(parser: &mut Parser<'src>) -> Result<Vec<TypeExpr>, ParserError> {
    let mut args = Vec::new();
    while parser.peek().kind != TokenKind::Gt {
        args.push(parse_type_annotation(parser)?);
    }
    Ok(args)
}

/// parse a generic parameter
fn parse_generic_param<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<GenericParam, ParserError> {
    let name = unwrap_lexeme_with_kind(&parser.consume(), TokenKind::Ident)?;
    let mut bounds = Vec::new();
    if parser.peek().kind == TokenKind::Colon {
        let mut token =parser.consume();
        loop {
            token = parser.consume();
            // parse the bound
            match token.kind {
                TokenKind::Ident => {
                    bounds.push(unwrap_lexeme_with_kind(&token, TokenKind::Ident)?);
                }
                _ => return Err(ParserError::UnexpectedToken { at: token.loc, data: token.error_data() }),
            }
            token = parser.consume();

            match token.kind {
                // continue parsing the next bound
                TokenKind::Plus => continue,
                // done parsing bounds
                TokenKind::Comma => break,
                // the only valid tokens are + and ,
                _ => return Err(ParserError::UnexpectedToken { at: token.loc, data: token.error_data() }),
            }
        }
    }
    Ok(GenericParam { name, bounds, loc: token.loc })
}

/// parse a generic context
fn parse_generic_context<'src>(parser: &mut Parser<'src>) -> Result<GenericContext, ParserError> {
    if parser.peek().kind != TokenKind::Lt {
        return Ok(GenericContext::new());
    }
    // consume the <
    parser.consume();
    let mut context = GenericContext::new();
    let mut token = parser.consume();
    loop {
        match context.add_generic(parse_generic_param(parser, &token)?) {
            Ok(_) => (),
            Err(e) => return Err(ParserError::GenericParamError(e)),
        };
        
        token = parser.consume();

        match token.kind {
            TokenKind::Comma => {
                // consume the comma
                parser.consume();
                // parse the next generic param
                continue;
            }
            TokenKind::Gt => {
                // consume the >
                parser.consume();
                break;
            }
            _ => return Err(ParserError::UnexpectedToken { at: token.loc, data: token.error_data() }),
        }
    }
    Ok(context)
}

/// parse a function parameter list
fn parse_fn_params<'src>(parser: &mut Parser<'src>) -> Result<Vec<(String, TypeExpr)>, ParserError> {
    // expect a (
    expect_token(parser, TokenKind::Lparen)?;
    // handle the case where there are no parameters
    if parser.peek().kind == TokenKind::Rparen {
        parser.consume();
        return Ok(Vec::new());
    }
    let mut params = Vec::new();
    loop {
        let name = unwrap_lexeme_with_kind(&parser.consume(), TokenKind::Ident)?;
        // expect a :
        expect_token(parser, TokenKind::Colon)?;
        // parse the type
        let ty = parse_type_annotation(parser)?;
        params.push((name, ty));

        if parser.peek().kind == TokenKind::Comma {
            parser.consume();
            continue;
        }
        break;
    }
    // expect a )
    expect_token(parser, TokenKind::Rparen)?;
    Ok(params)
}

/// parse a function definition
fn parse_fn_def<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<FnDef, ParserError> {
    let name = unwrap_lexeme_with_kind(&parser.consume(), TokenKind::Ident)?;
    let loc = token.loc;
    // gives an empty generic context if there are no generics
    let generic_context = parse_generic_context(parser)?;
    let params = parse_fn_params(parser)?;
    // expect a ->
    expect_token(parser, TokenKind::Arrow)?;
    let ret_ty = parse_type_annotation(parser)?;
    expect_token(parser, TokenKind::Lbrace)?;
    let body = parse_block(parser)?;
    Ok(FnDef::new(name, generic_context, params, ret_ty, body, loc))
}

/// parse a struct field
fn parse_struct_field<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<StructField, ParserError> {
    let is_private = matches!(parser.peek().kind, TokenKind::Private);
    if is_private {
        parser.consume();
    }
    let name = unwrap_lexeme_with_kind(&parser.consume(), TokenKind::Ident)?;
    // expect a :
    expect_token(parser, TokenKind::Colon)?;
    let ty = parse_type_annotation(parser)?;
    Ok(StructField { is_private, name, ty, loc: token.loc })
}

/// parse a struct field list
fn parse_struct_fields<'src>(parser: &mut Parser<'src>) -> Result<Vec<StructField>, ParserError> {
    // expect a {
    expect_token(parser, TokenKind::Lbrace)?;
    let mut fields = Vec::new();
    let mut comma = true;
    while parser.peek().kind != TokenKind::Rbrace && !parser.is_at_end() {
        if !comma {
            return Err(ParserError::ExpectedToken { expected: TokenKind::Comma, at: parser.peek().loc, data: parser.peek().error_data() });
        }
        let field = parse_struct_field(parser, &parser.peek())?;
        fields.push(field);
        // expect a comma or a }
        if parser.peek().kind == TokenKind::Comma {
            parser.consume();
            comma = true;
        } else {
            comma = false;
        }
    }
    // expect a }
    expect_token(parser, TokenKind::Rbrace)?;
    Ok(fields)
}

/// parse a struct definition
fn parse_struct_def<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<StructDef, ParserError> {
    let name = unwrap_lexeme_with_kind(&parser.consume(), TokenKind::Ident)?;
    let loc = token.loc;
    let generic_context = parse_generic_context(parser)?;
    let fields = parse_struct_fields(parser)?; 
    Ok(StructDef::new(name, generic_context, fields, loc))
}

/// parse an enum variant
/// 
/// example of expected input:
/// ```
/// enum Message {
///     // unit variant
///     Quit,
///     // Struct variant
///     Write { text: String },
///     // Tuple variant
///     Move(i32, i32),
/// }
/// ```
fn parse_enum_variant<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<EnumVariant, ParserError> {
    let name = unwrap_lexeme_with_kind(token, TokenKind::Ident)?;
    let loc = token.loc;
    if parser.peek().kind == TokenKind::Lbrace {
        // parse a struct variant
        let fields = parse_struct_fields(parser)?;
        Ok(EnumVariant::Struct { name, fields, loc })
    } else if parser.peek().kind == TokenKind::Lparen {
        // parse a tuple variant
        // consume the (
        parser.consume();
        // simply parse a vec of type annotation until we hit a )
        let mut values = Vec::new();
        let mut comma = true;
        while parser.peek().kind != TokenKind::Rparen && !parser.is_at_end() {
            if !comma {
                return Err(ParserError::ExpectedToken { expected: TokenKind::Comma, at: parser.peek().loc, data: parser.peek().error_data() });
            }
            values.push(parse_type_annotation(parser)?);
            if parser.peek().kind == TokenKind::Comma {
                parser.consume();
                comma = true;
            } else {
                comma = false;
            }
        }
        // consume the )
        expect_token(parser, TokenKind::Rparen)?;
        Ok(EnumVariant::Tuple { name, fields: values, loc })
    } else {
        // parse a unit variant
        Ok(EnumVariant::Unit { name, loc })
    }
}

/// parse an enum definition
/// 
/// example of expected input:
/// ```
/// enum Message {
///     Quit,
///     Write { text: String },
///     Move(i32, i32),
/// }
/// ```
fn parse_enum_def<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<EnumDef, ParserError> {
    let name = unwrap_lexeme_with_kind(&parser.consume(), TokenKind::Ident)?;
    let loc = token.loc;
    let generics = parse_generic_context(parser)?;
    expect_token(parser, TokenKind::Lbrace)?;
    let mut variants = Vec::new();
    let mut comma = true;
    while parser.peek().kind != TokenKind::Rbrace && !parser.is_at_end() {
        if !comma {
            return Err(ParserError::ExpectedToken { expected: TokenKind::Comma, at: parser.peek().loc, data: parser.peek().error_data() });
        }
        let next_token = parser.consume();
        variants.push(parse_enum_variant(parser, &next_token)?);
        if parser.peek().kind == TokenKind::Comma {
            parser.consume();
            comma = true;
        } else {
            comma = false;
        }
    }
    // expect a }
    expect_token(parser, TokenKind::Rbrace)?;
    Ok(EnumDef { name, generics, variants, loc })
}

/// parse an alias definition
/// 
/// example of expected input:
/// ```
/// type Number = i64;
/// type Point<T> = (T, T);
/// ```
fn parse_alias_def<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<AliasDef, ParserError> {
    let loc = token.loc;
    let name = unwrap_lexeme_with_kind(&parser.consume(), TokenKind::Ident)?;
    let generics = parse_generic_context(parser)?;
    // expect a =
    expect_token(parser, TokenKind::Eq)?;
    let ty = parse_type_annotation(parser)?;
    // expect a semicolon
    expect_token(parser, TokenKind::Semicolon)?;
    Ok(AliasDef { name, generics, ty, loc })
}

/// parse a trait function
/// 
/// either a required function or a given function
/// 
/// example of expected input:
/// ```
/// trait HasX {
///     // required function
///     fn x(&self) -> i32;
///     // given function
///     fn do_something_with_x(self) -> i32 {
///         self.x()
///     }
/// }
/// ```
fn parse_trait_fn<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<TraitFn, ParserError> {
    let name = unwrap_lexeme_with_kind(token, TokenKind::Ident)?;
    let loc = token.loc;
    let generics = parse_generic_context(parser)?;
    let params = parse_fn_params(parser)?;
    let ret_ty = parse_type_annotation(parser)?;
    let next_token = parser.consume();
    match next_token.kind {
        TokenKind::Semicolon => Ok(TraitFn::Required(RequiredFn { name, generics, params, ret_ty, loc })),
        TokenKind::Lbrace => {
            let body = parse_block(parser)?;
            Ok(TraitFn::Given(GivenFn::new(name, generics, params, ret_ty, body, loc)))
        }
        _ => Err(ParserError::UnexpectedToken { at: next_token.loc, data: next_token.error_data() }),
    }
}

/// parse a required trait field
fn parse_required_trait_field<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<RequiredField, ParserError> {
    let name = unwrap_lexeme_with_kind(token, TokenKind::Ident)?;
    let loc = token.loc;
    // expect a :
    expect_token(parser, TokenKind::Colon)?;
    let ty = parse_type_annotation(parser)?;
    // expect a semicolon
    expect_token(parser, TokenKind::Semicolon)?;
    Ok(RequiredField { name, ty, loc })
}

/// parse a trait definition
fn parse_trait_def<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<TraitDef, ParserError> {
    let name = unwrap_lexeme_with_kind(token, TokenKind::Ident)?;
    let loc = token.loc;
    let generics = parse_generic_context(parser)?;
    let mut required_fns = Vec::new();
    let mut given_fns = Vec::new();
    let mut required_fields = Vec::new();
    let mut next_token = parser.consume();
    loop {
        match next_token.kind {
            TokenKind::Fn => match parse_trait_fn(parser, &next_token)? {
                TraitFn::Required(required_fn) => required_fns.push(required_fn),
                TraitFn::Given(given_fn) => given_fns.push(given_fn),
            }
            TokenKind::Ident => required_fields.push(parse_required_trait_field(parser, &next_token)?),
            TokenKind::Rbrace => break,
            _ => return Err(ParserError::UnexpectedToken { at: next_token.loc, data: next_token.error_data() }),
        }
        next_token = parser.consume();
    }
    expect_token(parser, TokenKind::Rbrace)?;
    Ok(TraitDef { name, generics, required_fns, given_fns, required_fields, loc })
}


/// parse an impl block
fn parse_impl_block<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<ImplBlock, ParserError> {
    // token is the impl keyword
    let loc = token.loc;
    // check if there are generics
    let generics = parse_generic_context(parser)?;
    let mut trait_ = None;
    // expect a trait or a struct
    let ty = parse_type_annotation(parser)?;
    // if there is a for token then it's an impl for a trait and ty is the trait name
    let for_type = if parser.peek().kind == TokenKind::For {
        parser.consume();
        trait_ = Some(ty);
        // parse the type the impl belongs to
        parse_type_annotation(parser)?
    } else {
        ty
    };
    expect_token(parser, TokenKind::Lbrace)?;
    let mut fns: Vec<FnDef> = Vec::new();
    let mut next_token = parser.consume();
    loop {
        match next_token.kind {
            TokenKind::Fn => fns.push(parse_fn_def(parser, &next_token)?),
            TokenKind::Rbrace => break,
            _ => return Err(ParserError::UnexpectedToken { at: next_token.loc, data: next_token.error_data() }),
        }
        next_token = parser.consume();
    }
    expect_token(parser, TokenKind::Rbrace)?;
    Ok(ImplBlock { trait_, generics, for_type, fns, loc })
}

/// consume tokens until next top level token
fn consume_to_top_level<'src>(parser: &mut Parser<'src>) {
    while !parser.is_at_end() {
        let next_token = parser.peek();
        if next_token.kind == TokenKind::Fn || next_token.kind == TokenKind::Struct || next_token.kind == TokenKind::Enum || next_token.kind == TokenKind::Type || next_token.kind == TokenKind::Trait || next_token.kind == TokenKind::Impl {
            break;
        } else {
            parser.consume();
        }
    }
}


/// parse a program
fn parse_program<'src>(parser: &mut Parser<'src>) -> Result<Program, Vec<ParserError>> {
    let mut program = Program::new();
    let mut errors = Vec::new();
    while !parser.is_at_end() {
        let next_token = parser.consume();
        match next_token.kind {
            TokenKind::Fn => match parse_fn_def(parser, &next_token) {
                Ok(fn_def) => match program.add_fn_def(fn_def) {
                    Ok(_) => (),
                    Err(e) => {
                        errors.push(e);
                    }
                }
                Err(e) => {
                    errors.push(e);
                    consume_to_top_level(parser);
                }
            }
            TokenKind::Struct => match parse_struct_def(parser, &next_token) {
                Ok(struct_def) => match program.add_struct_def(struct_def) {
                    Ok(_) => (),
                    Err(e) => errors.push(e),
                }
                Err(e) => errors.push(e),
            }
            TokenKind::Enum => match parse_enum_def(parser, &next_token) {
                Ok(enum_def) => match program.add_enum_def(enum_def) {
                    Ok(_) => (),
                    Err(e) => errors.push(e),
                }
                Err(e) => errors.push(e),
            }
            TokenKind::Type => match parse_alias_def(parser, &next_token) {
                Ok(alias_def) => match program.add_alias_def(alias_def) {
                    Ok(_) => (),
                    Err(e) => errors.push(e),
                }
                Err(e) => errors.push(e),
            }
            TokenKind::Trait => match parse_trait_def(parser, &next_token) {
                Ok(trait_def) => match program.add_trait_def(trait_def) {
                    Ok(_) => (),
                    Err(e) => errors.push(e),
                }
                Err(e) => errors.push(e),
            }
            TokenKind::Impl => match parse_impl_block(parser, &next_token) {
                Ok(impl_block) => match program.add_impl_block(impl_block) {
                    Ok(_) => (),
                    Err(e) => errors.push(e),
                }
                Err(e) => errors.push(e),
            }
            _ => {
                errors.push(ParserError::UnexpectedToken { at: next_token.loc, data: next_token.error_data() });
                consume_to_top_level(parser);
            }
        };
    }
    if errors.is_empty() {
        Ok(program)
    } else {
        Err(errors)
    }
}

pub fn parse<'src>(source: &'src str) -> Result<Program, ()> {
    let mut tokenizer = Tokenizer::new(source);
    let tokens = tokenizer.tokenize();
    let mut parser = Parser::new(tokens);
    match parse_program(&mut parser) {
        Ok(program) => Ok(program),
        Err(errors) => {
            for error in errors {
                eprintln!("{}", error);
            }
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_fn_def() {
        let source = "fn main() -> i32 { 0 }";
        let program = parse(source);
        assert!(program.is_ok());
    }

    #[test]
    fn parse_struct_def() {
        let source = "struct Point { x: i32, y: i32 }";
        let program = parse(source);
        assert!(program.is_ok());
    }

    #[test]
    fn parse_enum_def() {
        let source = "enum Message { Quit, Write { text: String }, Move(i32, i32) }";
        let program = parse(source);
        assert!(program.is_ok());
    }
}
