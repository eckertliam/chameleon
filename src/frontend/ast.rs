use std::collections::HashMap;
use crate::frontend::Loc;

pub struct Program {
    pub fn_defs: Vec<FnDef>,
    /// all UDTs
    pub user_types: HashMap<String, Type>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            fn_defs: Vec::new(),
            user_types: HashMap::new(),
        }
    }

    pub fn add_fn_def(&mut self, fn_def: FnDef) {
        self.fn_defs.push(fn_def);
    }

    pub fn add_user_type(&mut self, name: String, ty: Type) {
        self.user_types.insert(name, ty);
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int {
        value: i64,
        loc: Loc,
    },
    Float {
        value: f64,
        loc: Loc,
    },
    String {
        value: String,
        loc: Loc,
    },
    Bool {
        value: bool,
        loc: Loc,
    },
    Ident {
        name: String,
        loc: Loc,
    },
    BinOp {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        kind: BinOpKind,
        loc: Loc,
    },
    UnOp {
        expr: Box<Expr>,
        kind: UnOpKind,
        loc: Loc,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        loc: Loc,
    },
    Array {
        values: Vec<Expr>,
        loc: Loc,
    },
    Block {
        stmts: Vec<Stmt>,
        loc: Loc,
    },
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
        loc: Loc,
    },
}

impl Expr {
    pub fn loc(&self) -> Loc {
        match self {
            Expr::Int { loc, .. } => *loc,
            Expr::Float { loc, .. } => *loc,
            Expr::String { loc, .. } => *loc,
            Expr::Bool { loc, .. } => *loc,
            Expr::Ident { loc, .. } => *loc,
            Expr::BinOp { loc, .. } => *loc,
            Expr::UnOp { loc, .. } => *loc,
            Expr::Call { loc, .. } => *loc,
            Expr::Array { loc, .. } => *loc,
            Expr::Block { loc, .. } => *loc,
            Expr::Index { loc, .. } => *loc,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    BitOr,
    BitAnd,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Return {
        expr: Option<Expr>,
        loc: Loc,
    },
    Expr(Expr),
    LetDef {
        name: String,
        ty: Option<Type>,
        value: Expr,
        loc: Loc,
    },
    ConstDef {
        name: String,
        ty: Option<Type>,
        value: Expr,
        loc: Loc,
    },
    Block {
        stmts: Vec<Stmt>,
        loc: Loc,
    },
    If {
        cond: Expr,
        then_block: Box<Stmt>,
        else_block: Option<Box<Stmt>>,
        loc: Loc,
    },
}

#[derive(Debug, Clone)]
pub enum Type {
    Float,
    Int,
    Bool,
    Char,
    String,
    UnsignedInt,
    Void,
    Ptr(Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>, usize),
    Enum(Vec<(String, Option<Type>)>),
    Struct(Vec<Type>),
}

#[derive(Debug, Clone)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub ret_ty: Type,
    pub body: Vec<Stmt>,
}

impl FnDef {
    pub fn new(name: String, params: Vec<(String, Type)>, ret_ty: Type, body: Vec<Stmt>) -> Self {
        Self {
            name,
            params,
            ret_ty,
            body,
        }
    }
}

// TODO: Add support for struct definitions
// TODO: Add support for enum definitions
// TODO: Add support for alias definitions