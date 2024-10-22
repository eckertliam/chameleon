use std::fmt;

use super::{ast::*, tokenizer::{Token, TokenKind}, Loc};

#[derive(Debug, Clone)]
enum ParserError {
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
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken { at, data } => write!(f, "Unexpected token: {} at {}", data, at),
            Self::ExpectedToken { expected, at, data } => write!(f, "Expected {} at {} but got {}", expected.as_str(), at, data),
            Self::UnexpectedEOF { at } => write!(f, "Unexpected EOF at {}", at),
            Self::ExpectedValidNumber { at, data } => write!(f, "Expected a valid number at {} but got {}", at, data),
            Self::GenericParamError(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for ParserError {}

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

fn expect_lexeme<'src>(token: &Token<'src>) -> Result<String, ParserError> {
    token.lexeme
        .map(|lexeme| lexeme.to_string())
        .ok_or(ParserError::UnexpectedToken { at: token.loc, data: token.error_data() })
}

// Expression parsing
// The expression parser is a simple Pratt parser 
// Inspired by https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/

/// parse an expression with a given precedence
fn expr<'src>(parser: &mut Parser<'src>, token: &Token<'src>, precedence: u8) -> Result<Expr, ParserError> {
    let mut lhs = match prefix(parser, token) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
    };

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
    let name = expect_lexeme(token)?;
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
    let value = expect_lexeme(token)?;
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
    let value = expect_lexeme(token)?;
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
define_unary_op!(prefix_not, UnOpKind::Not);


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
    let rparen = parser.consume();
    if rparen.kind != TokenKind::Rparen {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Rparen, at: rparen.loc, data: rparen.error_data() });
    }

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
    let close = parser.consume();
    if close.kind != TokenKind::Rbracket {
        return Err(ParserError::UnexpectedToken { at: close.loc, data: close.error_data() });
    }
    let loc = lhs.loc();
    Ok(Expr::Index {
        array: Box::new(lhs),
        index: Box::new(index),
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
        _ => Ok(Stmt::Expr(expr(parser, &token, 0)?)),
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
    let name = expect_lexeme(&parser.consume())?;

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
    let semicolon = parser.consume();
    if semicolon.kind != TokenKind::Semicolon {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Semicolon, at: semicolon.loc, data: semicolon.error_data() });
    }

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
    let rbrace = parser.consume();
    if rbrace.kind != TokenKind::Rbrace {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Rbrace, at: rbrace.loc, data: rbrace.error_data() });
    }
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
    let lbrace = parser.consume();
    if lbrace.kind != TokenKind::Lbrace {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Lbrace, at: lbrace.loc, data: lbrace.error_data() });
    }
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
            // consume the lbrace
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
fn parse_type_annotation<'src>(parser: &mut Parser<'src>) -> Result<Type, ParserError> {
    let token = parser.consume();
    // will either start with TokenKind Ident, Lbracket, or Lparen
    match token.kind {
        TokenKind::Ident => {
            let name = expect_lexeme(&token)?;
            // check for generic args
            if parser.peek().kind == TokenKind::Lt {
                // consume the <
                let token = parser.consume();
                let generic_args = parse_generic_args(parser)?;
                return Ok(Type::Generic(name, generic_args, token.loc))
            } 
            // match on the primitive type
            // otherwise it's an alias
            let ty =match name.as_str() {
                "i8" => Type::I8(token.loc),
                "i16" => Type::I16(token.loc),
                "i32" => Type::I32(token.loc),
                "i64" => Type::I64(token.loc),
                "u8" => Type::U8(token.loc),
                "u16" => Type::U16(token.loc),
                "u32" => Type::U32(token.loc),
                "u64" => Type::U64(token.loc),
                "f32" => Type::F32(token.loc),
                "f64" => Type::F64(token.loc),
                "bool" => Type::Bool(token.loc),
                "void" => Type::Void(token.loc),
                _ => Type::Alias(name, token.loc),
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
fn parse_array_type<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Type, ParserError> {
    let loc = token.loc;
    let elem_type = parse_type_annotation(parser)?;
    // expect a semicolon
    let semicolon = parser.consume();
    if semicolon.kind != TokenKind::Semicolon {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Semicolon, at: semicolon.loc, data: semicolon.error_data() })
    }
    // parse the size
    let size_token = parser.consume();
    let size = expr(parser, &size_token, 0)?;

    Ok(Type::Array(Box::new(elem_type), size, loc))
}


/// parse a tuple type
/// 
/// errors if it doesn't close with a )
fn parse_tuple_type<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<Type, ParserError> {
    let loc = token.loc;
    let mut types = Vec::new();
    while parser.peek().kind != TokenKind::Rparen {
        types.push(parse_type_annotation(parser)?);
    }
    let rparen = parser.consume();
    if rparen.kind != TokenKind::Rparen {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Rparen, at: rparen.loc, data: rparen.error_data() });
    }
    Ok(Type::Tuple(types, loc))
}

/// parse a generic argument list
/// 
/// errors if it doesn't close with a >
fn parse_generic_args<'src>(parser: &mut Parser<'src>) -> Result<Vec<Type>, ParserError> {
    let mut args = Vec::new();
    while parser.peek().kind != TokenKind::Gt {
        args.push(parse_type_annotation(parser)?);
    }
    Ok(args)
}

/// parse a generic parameter
fn parse_generic_param<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<GenericParam, ParserError> {
    let name = expect_lexeme(&parser.consume())?;
    let mut bounds = Vec::new();
    if parser.peek().kind == TokenKind::Colon {
        let mut token =parser.consume();
        loop {
            token = parser.consume();
            // parse the bound
            match token.kind {
                TokenKind::Ident => {
                    bounds.push(expect_lexeme(&token)?);
                }
                _ => return Err(ParserError::UnexpectedToken { at: token.loc, data: token.error_data() }),
            }

            if parser.is_at_end() {
                return Err(ParserError::UnexpectedEOF { at: token.loc });
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
    // consume the <
    parser.consume();
    let mut context = GenericContext::new();
    let mut token = parser.consume();
    loop {
        match context.add_generic(parse_generic_param(parser, &token)?) {
            Ok(_) => (),
            Err(e) => return Err(ParserError::GenericParamError(e)),
        };
        
        if parser.is_at_end() {
            return Err(ParserError::UnexpectedEOF { at: token.loc });
        }

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
fn parse_fn_params<'src>(parser: &mut Parser<'src>) -> Result<Vec<(String, Type)>, ParserError> {
    // expect a (
    let lparen = parser.consume();
    if lparen.kind != TokenKind::Lparen {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Lparen, at: lparen.loc, data: lparen.error_data() });
    }
    let mut params = Vec::new();
    loop {
        let name = expect_lexeme(&parser.consume())?;
        // expect a :
        let colon = parser.consume();
        if colon.kind != TokenKind::Colon {
            return Err(ParserError::ExpectedToken { expected: TokenKind::Colon, at: colon.loc, data: colon.error_data() });
        }
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
    let rparen = parser.consume();
    if rparen.kind != TokenKind::Rparen {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Rparen, at: rparen.loc, data: rparen.error_data() });
    }
    Ok(params)
}

/// parse a function definition
fn parse_fn_def<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<FnDef, ParserError> {
    let name = expect_lexeme(&token)?;
    let loc = token.loc;
    // gives an empty generic context if there are no generics
    let generic_context = match parser.peek().kind {
        TokenKind::Lt => parse_generic_context(parser)?,
        _ => GenericContext::new(),
    };
    let params = parse_fn_params(parser)?;
    // expect a ->
    let arrow = parser.consume();
    if arrow.kind != TokenKind::Arrow {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Arrow, at: arrow.loc, data: arrow.error_data() });
    }
    let ret_ty = parse_type_annotation(parser)?;
    if parser.peek().kind == TokenKind::Lbrace {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Lbrace, at: parser.peek().loc, data: parser.peek().error_data() });
    }
    let body = parse_block(parser)?;
    Ok(FnDef::new(name, generic_context, params, ret_ty, body, loc))
}

/// parse a struct field
fn parse_struct_field<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<StructField, ParserError> {
    let is_private = matches!(parser.peek().kind, TokenKind::Private);
    if is_private {
        parser.consume();
    }
    let name = expect_lexeme(&token)?;
    // expect a :
    let colon = parser.consume();
    if colon.kind != TokenKind::Colon {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Colon, at: colon.loc, data: colon.error_data() });
    }
    let ty = parse_type_annotation(parser)?;
    Ok(StructField { is_private, name, ty, loc: token.loc })
}

/// parse a struct field list
fn parse_struct_fields<'src>(parser: &mut Parser<'src>) -> Result<Vec<StructField>, ParserError> {
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
    let rbrace = parser.consume();
    if rbrace.kind != TokenKind::Rbrace {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Rbrace, at: rbrace.loc, data: rbrace.error_data() });
    }
    Ok(fields)
}

/// parse a struct definition
fn parse_struct_def<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<StructDef, ParserError> {
    let name = expect_lexeme(&token)?;
    let loc = token.loc;
    let generic_context = match parser.peek().kind {
        TokenKind::Lt => parse_generic_context(parser)?,
        _ => GenericContext::new(),
    };
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
    let name = expect_lexeme(&token)?;
    let loc = token.loc;
    if parser.peek().kind == TokenKind::Lbrace {
        // parse a struct variant
        // consume the {
        parser.consume();
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
        if parser.consume().kind != TokenKind::Rparen {
            return Err(ParserError::ExpectedToken { expected: TokenKind::Rparen, at: parser.peek().loc, data: parser.peek().error_data() });
        }
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
    let name = expect_lexeme(&token)?;
    let loc = token.loc;
    let generics = match parser.peek().kind {
        TokenKind::Lt => parse_generic_context(parser)?,
        _ => GenericContext::new(),
    };
    let lbrace = parser.consume();
    if lbrace.kind != TokenKind::Lbrace {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Lbrace, at: lbrace.loc, data: lbrace.error_data() });
    }
    let mut variants = Vec::new();
    let mut comma = true;
    while parser.peek().kind != TokenKind::Rbrace && !parser.is_at_end() {
        if !comma {
            return Err(ParserError::ExpectedToken { expected: TokenKind::Comma, at: parser.peek().loc, data: parser.peek().error_data() });
        }
        variants.push(parse_enum_variant(parser, &parser.peek())?);
        if parser.peek().kind == TokenKind::Comma {
            parser.consume();
            comma = true;
        } else {
            comma = false;
        }
    }
    // expect a }
    let rbrace = parser.consume();
    if rbrace.kind != TokenKind::Rbrace {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Rbrace, at: rbrace.loc, data: rbrace.error_data() });
    }
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
    let name = expect_lexeme(&parser.consume())?;
    let generics = match parser.peek().kind {
        TokenKind::Lt => parse_generic_context(parser)?,
        _ => GenericContext::new(),
    };
    // expect a =
    let eq = parser.consume();
    if eq.kind != TokenKind::Eq {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Eq, at: eq.loc, data: eq.error_data() });
    }
    let ty = parse_type_annotation(parser)?;
    // expect a semicolon
    let semicolon = parser.consume();
    if semicolon.kind != TokenKind::Semicolon {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Semicolon, at: semicolon.loc, data: semicolon.error_data() });
    }
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
    let name = expect_lexeme(&token)?;
    let loc = token.loc;
    let generics = match parser.peek().kind {
        TokenKind::Lt => parse_generic_context(parser)?,
        _ => GenericContext::new(),
    };
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
    let name = expect_lexeme(&token)?;
    let loc = token.loc;
    // expect a :
    let colon = parser.consume();
    if colon.kind != TokenKind::Colon {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Colon, at: colon.loc, data: colon.error_data() });
    }
    let ty = parse_type_annotation(parser)?;
    // expect a semicolon
    let semicolon = parser.consume();
    if semicolon.kind != TokenKind::Semicolon {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Semicolon, at: semicolon.loc, data: semicolon.error_data() });
    }
    Ok(RequiredField { name, ty, loc })
}

/// parse a trait definition
fn parse_trait_def<'src>(parser: &mut Parser<'src>, token: &Token<'src>) -> Result<TraitDef, ParserError> {
    let name = expect_lexeme(&token)?;
    let loc = token.loc;
    let generics = match parser.peek().kind {
        TokenKind::Lt => parse_generic_context(parser)?,
        _ => GenericContext::new(),
    };
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
    if next_token.kind != TokenKind::Rbrace {
        return Err(ParserError::ExpectedToken { expected: TokenKind::Rbrace, at: next_token.loc, data: next_token.error_data() });
    }
    Ok(TraitDef { name, generics, required_fns, given_fns, required_fields, loc })
}
