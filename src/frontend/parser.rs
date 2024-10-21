// TODO: replace all panic! with Results

use std::{collections::HashMap, fmt::{self, Display}};

use super::{ast::*, tokenizer::{Token, TokenKind, Tokenizer}, Loc};

#[derive(Debug)]
enum ParserError<'src> {
    UnexpectedToken {
        token: Token<'src>,
    },
    ExpectedToken {
        expected: TokenKind,
        got: Token<'src>,
    },
    UnexpectedEOF {
        loc: Loc,
    },
}

impl<'src> fmt::Display for ParserError<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken { token } => write!(f, "Unexpected token: {} at {}", token.error_data(), token.loc),
            Self::ExpectedToken { expected, got } => write!(f, "Expected {} at {} but got {}", expected.as_str(), got.loc, got.error_data()),
            Self::UnexpectedEOF { loc } => write!(f, "Unexpected EOF at {}", loc),
        }
    }
}

impl<'src> std::error::Error for ParserError<'src> {}

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

/// parse an expression with a given precedence
fn expr(parser: &mut Parser, token: &Token, precedence: u8) -> Expr {
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

/// parse an identifier
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
    // track the last token consumed
    let mut arr_token = parser.consume();
    loop {
        values.push(expr(parser, &arr_token, 0));
        arr_token = parser.consume();
        if arr_token.kind == TokenKind::Comma {
            arr_token = parser.consume();
        } else {
            break;
        }
    }
    // ensure loop exited with a ]
    if arr_token.kind != TokenKind::Rbracket {
        panic!("Expected ] at {}", arr_token.loc);
    }
    let loc = token.loc;
    Expr::Array {
        values,
        loc,
    }
}

fn block_expr(parser: &mut Parser, token: &Token) -> Expr {
    let stmts = parse_block(parser);
    let loc = token.loc;
    Expr::Block { stmts, loc }
}

/// parse a number
/// 
/// returns an Int or Float depending on what can be parsed
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
            let expr_token = parser.consume();
            Expr::UnOp {
                kind: $op_kind,
                expr: Box::new(expr(parser, &expr_token, 0)),
                loc: token.loc,
            }
        }
    }
}

define_unary_op!(prefix_minus, UnOpKind::Neg);
define_unary_op!(prefix_bang, UnOpKind::Not);
define_unary_op!(prefix_not, UnOpKind::Not);


/// parse an infix expression
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
            let rhs_token = parser.consume();
            // pass the pass the operator precedence and parse the rhs 
            let rhs = expr(parser, &rhs_token, get_prec(token.kind));
            Expr::BinOp {
                kind: $op_kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
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

/// parse a function call
fn infix_call(parser: &mut Parser, lhs: Expr, token: &Token) -> Expr {
    // parse arguments
    let mut args = Vec::new();
    let mut infix_token = parser.consume();
    loop {
        args.push(expr(parser, &infix_token, 0));
        infix_token = parser.consume();
        if infix_token.kind == TokenKind::Comma {
            infix_token = parser.consume();
        } else {
            break;
        }
    }

    // ensure loop exited with a )
    if infix_token.kind != TokenKind::Rparen {
        panic!("Expected ) at {}", infix_token.loc);
    }

    let loc = lhs.loc();
    Expr::Call {
        callee: Box::new(lhs),
        args,
        loc,
    }
}

/// parse an index
fn infix_index(parser: &mut Parser, lhs: Expr, token: &Token) -> Expr {
    let index_token = parser.consume();
    let index = expr(parser, &index_token, 0);
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
fn stmt(parser: &mut Parser) -> Stmt {
    let token = parser.consume();
    match token.kind {
        TokenKind::Return => return_stmt(parser, &token),
        TokenKind::Let => var_decl(parser, &token, true),
        TokenKind::Const => var_decl(parser, &token, false),
        TokenKind::Lbrace => block_stmt(parser, &token),
        TokenKind::If => if_stmt(parser, &token),
        _ => Stmt::Expr(expr(parser, &token, 0)),
    }
}

/// parse a return statement
fn return_stmt(parser: &mut Parser, token: &Token) -> Stmt {
    let expr = if parser.peek().kind != TokenKind::Semicolon {
        Some(expr(parser, &token, 0))
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

/// parse a variable declaration
fn var_decl(parser: &mut Parser, token: &Token, mutable: bool) -> Stmt {
    let name = if let Token { kind: TokenKind::Ident, lexeme: Some(lexeme), .. } = parser.consume() {
        lexeme.to_string()
    } else {
        panic!("Expected identifier at {}", token.loc);
    };

    // TODO: add type annotation parsing
    let ty = None;

    let value_token = parser.consume();
    let value = expr(parser, &value_token, 0);

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

/// parse a block
fn parse_block(parser: &mut Parser) -> Vec<Stmt> {
    let mut stmts = Vec::new();
    while parser.peek().kind != TokenKind::Rbrace && !parser.is_at_end() {
        stmts.push(stmt(parser));
    }
    if parser.consume().kind != TokenKind::Rbrace {
        panic!("Expected }} at {}", parser.peek().loc);
    }
    stmts
}

/// parse a block
/// 
/// where the token is the opening {
fn block_stmt(parser: &mut Parser, token: &Token) -> Stmt {
    let stmts = parse_block(parser);
    Stmt::Block { stmts, loc: token.loc }
}

/// parse an if statement
fn if_stmt(parser: &mut Parser, token: &Token) -> Stmt {
    let cond_token = parser.consume();
    let cond = expr(parser, &cond_token, 0);
    let lbrace = parser.consume();
    if lbrace.kind != TokenKind::Lbrace {
        panic!("Expected {{ at {}", lbrace.loc);
    }
    let then_block = Box::new(block_stmt(parser, &lbrace));
    let else_block = if parser.peek().kind == TokenKind::Else {
        parser.consume();
        // check for an else if
        if parser.peek().kind == TokenKind::If {
            // consume the if token
            let token = parser.consume();
            // recursively parse the if statement
            let if_stmt = Box::new(if_stmt(parser, &token));
            Some(if_stmt)
        } else {
            // consume the lbrace
            let lbrace = parser.consume();
            if lbrace.kind != TokenKind::Lbrace {
                panic!("Expected {{ at {}", lbrace.loc);
            }
            // parse the else block
            let else_block = Box::new(block_stmt(parser, &lbrace));
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

// type annotation parsing

/// parse a type annotation
/// 
/// expects to start with TokenKind Ident, Lbracket, or Lparen
/// will panic if it doesn't
fn parse_type_annotation(parser: &mut Parser) -> Type {
    let token = parser.consume();
    // will either start with TokenKind Ident, Lbracket, or Lparen
    match token.kind {
        TokenKind::Ident => {
            let name = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
            // check for generic args
            if parser.peek().kind == TokenKind::Lt {
                // consume the <
                let token = parser.consume();
                let generic_args = parse_generic_args(parser);
                return Type::Generic(name, generic_args, token.loc)
            } 
            // match on the primitive type
            // otherwise it's an alias
            match name.as_str() {
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
            }
        }
        TokenKind::Lbracket => parse_array_type(parser, &token),
        TokenKind::Lparen => parse_tuple_type(parser, &token),
        _ => panic!("Expected identifier at {}", token.loc),
    }
}

/// parse an array type
/// 
/// panics if it doesn't close with a ]
fn parse_array_type(parser: &mut Parser, token: &Token) -> Type {
    let loc = token.loc;
    let elem_type = parse_type_annotation(parser);
    // expect a semicolon
    if parser.consume().kind != TokenKind::Semicolon {
        panic!("Expected semicolon at {}", parser.peek().loc);
    }
    // parse the size
    let size_token = parser.consume();
    let size = expr(parser, &size_token, 0);

    Type::Array(Box::new(elem_type), size, loc)
}


/// parse a tuple type
/// 
/// panics if it doesn't close with a )
fn parse_tuple_type(parser: &mut Parser, token: &Token) -> Type {
    let loc = token.loc;
    let mut types = Vec::new();
    while parser.peek().kind != TokenKind::Rparen {
        types.push(parse_type_annotation(parser));
    }
    if parser.consume().kind != TokenKind::Rparen {
        panic!("Expected ) at {}", parser.peek().loc);
    }
    Type::Tuple(types, loc)
}

/// parse a generic argument list
/// 
/// panics if it doesn't close with a >
fn parse_generic_args(parser: &mut Parser) -> Vec<Type> {
    let mut args = Vec::new();
    while parser.peek().kind != TokenKind::Gt {
        args.push(parse_type_annotation(parser));
    }
    args
}

/// parse a generic parameter
fn parse_generic_param(parser: &mut Parser, token: &Token) -> GenericParam {
    let name = parser.consume().lexeme.expect(&format!("Expected a generic parameter name at {}", token.loc)).to_string();
    let mut bounds = Vec::new();
    if parser.peek().kind == TokenKind::Colon {
        let mut token =parser.consume();
        loop {
            token = parser.consume();
            // parse the bound
            match token.kind {
                TokenKind::Ident => {
                    bounds.push(token.lexeme.expect(&format!("Expected a trait bound at {}", token.loc)).to_string());
                }
                _ => panic!("Expected trait bound at {}", token.loc),
            }

            if parser.is_at_end() {
                panic!("Unexpected EOF while parsing trait bound at {}", token.loc);
            }

            token = parser.consume();

            match token.kind {
                // continue parsing the next bound
                TokenKind::Plus => continue,
                // done parsing bounds
                TokenKind::Comma => break,
                // the only valid tokens are + and ,
                _ => panic!("Expected + or , at {}", token.loc),
            }
        }
    }
    GenericParam { name, bounds, loc: token.loc }
}

/// parse a generic context
/// 
/// panics if it doesn't close with a >
fn parse_generic_context(parser: &mut Parser) -> GenericContext {
    // consume the <
    parser.consume();
    let mut context = GenericContext::new();
    let mut token = parser.consume();
    loop {
        match context.add_generic(parse_generic_param(parser, &token)) {
            Ok(_) => (),
            Err(e) => panic!("{}", e),
        };
        
        if parser.is_at_end() {
            panic!("Expected > at {}", token.loc);
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
            _ => panic!("Expected , or > at {}", token.loc),
        }
    }
    context
}

/// parse a function parameter list
/// 
/// panics if it doesn't close with a )
fn parse_fn_params(parser: &mut Parser) -> Vec<(String, Type)> {
    // expect a (
    if parser.consume().kind != TokenKind::Lparen {
        panic!("Expected ( at {}", parser.peek().loc);
    }
    let mut params = Vec::new();
    loop {
        let name = parser.consume().lexeme.expect(&format!("Expected a parameter name at {}", parser.peek().loc)).to_string();
        // expect a :
        if parser.consume().kind != TokenKind::Colon {
            panic!("Expected : at {}", parser.peek().loc);
        }
        // parse the type
        let ty = parse_type_annotation(parser);
        params.push((name, ty));

        if parser.peek().kind == TokenKind::Comma {
            parser.consume();
            continue;
        }
        break;
    }
    // expect a )
    let token = parser.consume();
    if token.kind != TokenKind::Rparen {
        panic!("Unclosed parameter list at {}", parser.peek().loc);
    }
    params
}

/// parse a function definition
fn parse_fn_def(parser: &mut Parser, token: &Token) -> FnDef {
    let name = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    let loc = token.loc;
    // gives an empty generic context if there are no generics
    let generic_context = match parser.peek().kind {
        TokenKind::Lt => parse_generic_context(parser),
        _ => GenericContext::new(),
    };
    let params = parse_fn_params(parser);
    // expect a ->
    if parser.consume().kind != TokenKind::Arrow {
        panic!("Expected -> at {}", parser.peek().loc);
    }
    let ret_ty = parse_type_annotation(parser);
    if parser.peek().kind == TokenKind::Lbrace {
        panic!("Expected body after -> at {}", parser.peek().loc);
    }
    let body = parse_block(parser);
    FnDef::new(name, generic_context, params, ret_ty, body, loc)
}

/// parse a struct field
fn parse_struct_field(parser: &mut Parser, token: &Token) -> StructField {
    let is_private = matches!(parser.peek().kind, TokenKind::Private);
    if is_private {
        parser.consume();
    }
    let name = token.lexeme.expect(&format!("Expected a field name at {}", parser.peek().loc)).to_string();
    // expect a :
    if parser.consume().kind != TokenKind::Colon {
        panic!("Expected : in struct field at {}", parser.peek().loc);
    }
    let ty = parse_type_annotation(parser);
    StructField { is_private, name, ty, loc: token.loc }
}

/// parse a struct field list
fn parse_struct_fields(parser: &mut Parser) -> Vec<StructField> {
    let mut fields = Vec::new();
    let mut comma = true;
    while parser.peek().kind != TokenKind::Rbrace && !parser.is_at_end() {
        if !comma {
            panic!("Expected comma at {} before next field", parser.peek().loc);
        }
        let field = parse_struct_field(parser, &parser.peek());
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
    if parser.consume().kind != TokenKind::Rbrace {
        panic!("Expected }} at {}", parser.peek().loc);
    }
    fields
}

/// parse a struct definition
fn parse_struct_def(parser: &mut Parser, token: &Token) -> StructDef {
    let name = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    let loc = token.loc;
    let generic_context = parse_generic_context(parser);
    let fields = parse_struct_fields(parser); 
    StructDef::new(name, generic_context, fields, loc)
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
fn parse_enum_variant(parser: &mut Parser, token: &Token) -> EnumVariant {
    let name = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    let loc = token.loc;
    if parser.peek().kind == TokenKind::Lbrace {
        // parse a struct variant
        // consume the {
        parser.consume();
        // parse a struct variant
        let fields = parse_struct_fields(parser);
        EnumVariant::Struct { name, fields, loc }
    } else if parser.peek().kind == TokenKind::Lparen {
        // parse a tuple variant
        // consume the (
        parser.consume();
        // simply parse a vec of type annotation until we hit a )
        let mut values = Vec::new();
        let mut comma = true;
        while parser.peek().kind != TokenKind::Rparen && !parser.is_at_end() {
            if !comma {
                panic!("Expected comma at {} before next value", parser.peek().loc);
            }
            values.push(parse_type_annotation(parser));
            if parser.peek().kind == TokenKind::Comma {
                parser.consume();
                comma = true;
            } else {
                comma = false;
            }
        }
        // consume the )
        if parser.consume().kind != TokenKind::Rparen {
            panic!("Expected ) at {}", parser.peek().loc);
        }
        EnumVariant::Tuple { name, fields: values, loc }
    } else {
        // parse a unit variant
        EnumVariant::Unit { name, loc }
    }
}

/// parse an enum definition
/// 
/// panics if it doesn't close with a }
/// 
/// example of expected input:
/// ```
/// enum Message {
///     Quit,
///     Write { text: String },
///     Move(i32, i32),
/// }
/// ```
fn parse_enum_def(parser: &mut Parser, token: &Token) -> EnumDef {
    let name = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    let loc = token.loc;
    let generics = parse_generic_context(parser);
    if parser.consume().kind != TokenKind::Lbrace {
        panic!("Expected {{ at {}", parser.peek().loc);
    }
    let mut variants = Vec::new();
    let mut comma = true;
    while parser.peek().kind != TokenKind::Rbrace && !parser.is_at_end() {
        if !comma {
            panic!("Expected comma at {} before next variant", parser.peek().loc);
        }
        variants.push(parse_enum_variant(parser, &parser.peek()));
        if parser.peek().kind == TokenKind::Comma {
            parser.consume();
            comma = true;
        } else {
            comma = false;
        }
    }
    // expect a }
    if parser.consume().kind != TokenKind::Rbrace {
        panic!("Expected }} at {}", parser.peek().loc);
    }
    
    EnumDef { name, generics, variants, loc }
}

/// parse an alias definition
/// 
/// example of expected input:
/// ```
/// type Number = i64;
/// type Point<T> = (T, T);
/// ```
fn parse_alias_def(parser: &mut Parser, token: &Token) -> AliasDef {
    let loc = token.loc;
    let name = if let Token { kind: TokenKind::Ident, lexeme: Some(lexeme), .. } = parser.consume() {
        lexeme.to_string()
    } else {
        panic!("Expected identifier at {}", token.loc);
    };
    let generics = parse_generic_context(parser);
    if parser.consume().kind != TokenKind::Eq {
        panic!("Expected = at {}", parser.peek().loc);
    }
    let ty = parse_type_annotation(parser);
    if parser.consume().kind != TokenKind::Semicolon {
        panic!("Expected semicolon at {}", parser.peek().loc);
    }
    AliasDef { name, generics, ty, loc }
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
fn parse_trait_fn(parser: &mut Parser, token: &Token) -> TraitFn {
    let name = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    let loc = token.loc;
    let generics = parse_generic_context(parser);
    let params = parse_fn_params(parser);
    let ret_ty = parse_type_annotation(parser);
    match parser.consume().kind {
        TokenKind::Semicolon => TraitFn::Required(RequiredFn { name, generics, params, ret_ty, loc }),
        TokenKind::Lbrace => {
            let body = parse_block(parser);
            TraitFn::Given(GivenFn::new(name, generics, params, ret_ty, body, loc))
        }
        _ => panic!("Expected semicolon or {{ at {}", parser.peek().loc),
    }
}

/// parse a required trait field
fn parse_required_trait_field(parser: &mut Parser, token: &Token) -> RequiredField {
    let name = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    let loc = token.loc;
    // expect a :
    if parser.consume().kind != TokenKind::Colon {
        panic!("Expected : in required trait field at {}", parser.peek().loc);
    }
    let ty = parse_type_annotation(parser);
    // expect a semicolon
    if parser.consume().kind != TokenKind::Semicolon {
        panic!("Expected semicolon at {}", parser.peek().loc);
    }
    RequiredField { name, ty, loc }
}

/// parse a trait definition
fn parse_trait_def(parser: &mut Parser, token: &Token) -> TraitDef {
    let name = token.lexeme.expect(&format!("Expected lexeme for token: {:?} at {}", token.kind, token.loc)).to_string();
    let loc = token.loc;
    let generics = parse_generic_context(parser);
    let mut required_fns = Vec::new();
    let mut given_fns = Vec::new();
    let mut required_fields = Vec::new();
    while parser.peek().kind != TokenKind::Rbrace && !parser.is_at_end() {
        match parser.peek().kind {
            TokenKind::Fn => match parse_trait_fn(parser, &parser.peek()) {
                TraitFn::Required(required_fn) => required_fns.push(required_fn),
                TraitFn::Given(given_fn) => given_fns.push(given_fn),
            }
            TokenKind::Ident => required_fields.push(parse_required_trait_field(parser, &parser.peek())),
            _ => panic!("Expected fn or identifier at {}", parser.peek().loc),
        }
    }
    TraitDef { name, generics, required_fns, given_fns, required_fields, loc }
}
