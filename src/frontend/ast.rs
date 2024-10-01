use std::collections::HashMap;
use crate::frontend::Loc;

pub struct Program {
    pub fn_defs: Vec<FnDef>,
    /// all UDTs
    pub user_types: HashMap<String, AstType>,
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

    pub fn add_user_type(&mut self, name: String, ty: AstType) {
        self.user_types.insert(name, ty);
    }
}

#[derive(Debug, Clone)]
pub enum AstExpr {
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
    Char {
        value: char,
        loc: Loc,
    },
    BinOp {
        lhs: Box<AstExpr>,
        rhs: Box<AstExpr>,
        kind: BinOpKind,
        loc: Loc,
    },
    UnOp {
        expr: Box<AstExpr>,
        kind: UnOpKind,
        loc: Loc,
    },
    Call {
        name: String,
        args: Vec<AstExpr>,
        loc: Loc,
    },
    Array {
        values: Vec<AstExpr>,
        loc: Loc,
    },
    Block {
        stmts: Vec<AstExpr>,
        loc: Loc,
    },
}

impl AstExpr {
    pub fn loc(&self) -> Loc {
        match self {
            AstExpr::Int { loc, .. } => *loc,
            AstExpr::Float { loc, .. } => *loc,
            AstExpr::String { loc, .. } => *loc,
            AstExpr::Bool { loc, .. } => *loc,
            AstExpr::Ident { loc, .. } => *loc,
            AstExpr::Char { loc, .. } => *loc,
            AstExpr::BinOp { loc, .. } => *loc,
            AstExpr::UnOp { loc, .. } => *loc,
            AstExpr::Call { loc, .. } => *loc,
            AstExpr::Array { loc, .. } => *loc,
            AstExpr::Block { loc, .. } => *loc,
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
pub enum AstStatement {
    Return {
        expr: Option<AstExpr>,
        loc: Loc,
    },
    Expr(AstExpr),
    LetDef {
        name: String,
        ty: Option<AstType>,
        value: AstExpr,
        loc: Loc,
    },
    ConstDef {
        name: String,
        ty: Option<AstType>,
        value: AstExpr,
        loc: Loc,
    },
    If {
        cond: AstExpr,
        then_block: AstExpr,
        else_block: Option<AstExpr>,
        loc: Loc,
    },
}

#[derive(Debug, Clone)]
pub enum AstType {
    Float,
    Int,
    Bool,
    Char,
    String,
    UnsignedInt,
    Void,
    Ptr(Box<AstType>),
    Tuple(Vec<AstType>),
    Array(Box<AstType>, usize),
    Enum(Vec<(String, Option<AstType>)>),
    Struct(Vec<AstType>),
}

pub struct FnDef {
    pub name: String,
    pub params: Vec<(String, AstType)>,
    pub ret_ty: AstType,
    pub body: Vec<AstStatement>,
}

impl FnDef {
    pub fn new(name: String, params: Vec<(String, AstType)>, ret_ty: AstType, body: Vec<AstStatement>) -> Self {
        Self {
            name,
            params,
            ret_ty,
            body,
        }
    }
}