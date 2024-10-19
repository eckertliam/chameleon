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

/// All type annotations
#[derive(Debug, Clone)]
pub enum Type {
    // Primitive types
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Void,
    Ptr(Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>, usize),
    // User-defined types
    /// An alias type
    /// 
    /// ```
    /// type Number = i64;
    /// let x: Number = 10;
    /// ```     
    Alias(String),
    /// A generic type
    /// 
    /// ```
    /// let x: Vec<i64> = Vec::new();
    /// ```
    Generic(String, Vec<Type>),
}

/// A generic parameter
#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: String,
    /// The trait bounds of the generic parameter
    pub bounds: Vec<Type>,
    pub loc: Loc,
}

/// A context for generic parameters
#[derive(Debug, Clone)]
pub struct GenericContext {
    /// Key: generic name, Value: generic parameter
    generics: HashMap<String, GenericParam>,
}

impl GenericContext {
    pub fn new() -> Self {
        Self {
            generics: HashMap::new(),
        }
    }

    /// Add a generic parameter to the context.
    /// On conflict, return error with location of existing generic.
    pub fn add_generic(&mut self, param: GenericParam) -> Result<(), String> {
        if let Some(existing) = self.generics.get(&param.name) {
            return Err(format!("Generic parameter {} already exists at {}", param.name, existing.loc));
        }
        self.generics.insert(param.name.clone(), param);
        Ok(())
    }

    pub fn get_generic(&self, name: &str) -> Option<&GenericParam> {
        self.generics.get(name)
    }
}

/// A function definition
/// 
/// ```
/// fn add<T>(a: T, b: T) -> T {
///     return a + b
/// }
/// ```
pub struct FnDef {
    pub name: String,
    pub generics: GenericContext,
    pub params: Vec<(String, Type)>,
    pub ret_ty: Type,
    pub body: Vec<Stmt>,
    pub loc: Loc,
}

impl FnDef {
    pub fn new(name: String, generics: GenericContext, params: Vec<(String, Type)>, ret_ty: Type, body: Vec<Stmt>, loc: Loc) -> Self {
        Self {
            name,
            generics,
            params,
            ret_ty,
            body,
            loc,
        }
    }
}

/// A struct definition
/// 
/// ```
/// struct Point<T> {
///     x: T,
///     y: T,
/// }
/// ```
pub struct StructDef {
    pub name: String,
    pub generics: GenericContext,
    pub fields: HashMap<String, Type>, 
    pub loc: Loc,
}

impl StructDef {
    pub fn new(name: String, generics: GenericContext, fields: HashMap<String, Type>, loc: Loc) -> Self {
        Self {
            name,
            generics,
            fields,
            loc,
        }
    }
}
// TODO: Add support for enum definitions
// TODO: Add support for alias definitions