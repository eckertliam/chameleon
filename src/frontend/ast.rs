use std::collections::HashMap;

pub struct Program {
    pub fn_defs: Vec<FnDef>,
    pub use_types: HashMap<String, AstType>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            fn_defs: Vec::new(),
            use_types: HashMap::new(),
        }
    }

    pub fn add_fn_def(&mut self, fn_def: FnDef) {
        self.fn_defs.push(fn_def);
    }

    pub fn add_use_type(&mut self, name: String, ty: AstType) {
        self.use_types.insert(name, ty);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub start: usize,
    pub end: usize,
}

impl Loc {
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Default for Loc {
    fn default() -> Self {
        Self::new(0, 0)
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
        ty: AstType,
        value: AstExpr,
        loc: Loc,
    },
    ConstDef {
        name: String,
        ty: AstType,
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
    Sum(Vec<(String, Option<AstType>)>),
    Product(Vec<AstType>),
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