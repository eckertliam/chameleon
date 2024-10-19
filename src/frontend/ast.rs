use std::collections::HashMap;
use crate::frontend::Loc;

pub struct Program {
    pub fn_defs: HashMap<String, FnDef>,
    pub struct_defs: HashMap<String, StructDef>,
    pub enum_defs: HashMap<String, EnumDef>,
    pub alias_defs: HashMap<String, AliasDef>,
    pub trait_defs: HashMap<String, TraitDef>,
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
    I8(Loc),
    I16(Loc),
    I32(Loc),
    I64(Loc),
    U8(Loc),
    U16(Loc),
    U32(Loc),
    U64(Loc),
    F32(Loc),
    F64(Loc),
    Bool(Loc),
    Void(Loc),
    Tuple(Vec<Type>, Loc),
    Array(Box<Type>, Expr, Loc),
    // User-defined types
    /// An alias type
    /// 
    /// Example:
    /// ```
    /// type Number = i64;
    /// let x: Number = 10;
    /// ```     
    Alias(String, Loc),
    /// A generic type
    /// 
    /// Example:
    /// ```
    /// let x: Vec<i64> = Vec::new();
    /// ```
    Generic(String, Vec<Type>, Loc),
}

/// A generic parameter
/// 
/// Example:
/// ```
/// fn add<T: Add>(a: T, b: T) -> T {
///     return a + b
/// }
/// ```
#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: String,
    /// The trait bounds of the generic parameter
    pub bounds: Vec<String>,
    pub loc: Loc,
}

/// A context for generic parameters.
/// 
/// A generic context is everything between the `<` and `>` in a generic type.
/// 
/// Example:
/// ```
/// struct Point<T: Add + Sub + Mul + Div> {
///     x: T,
///     y: T,
/// }
/// ```
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

/// A struct field
pub struct StructField {
    pub name: String,
    pub ty: Type,
    pub loc: Loc,
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
    pub fields: Vec<StructField>,
    pub loc: Loc,
}

impl StructDef {
    pub fn new(name: String, generics: GenericContext, fields: Vec<StructField>, loc: Loc) -> Self {
        Self {
            name,
            generics,
            fields,
            loc,
        }
    }
}

/// An enum variant. Covers Symbol, Tuple, and Struct variants.
pub enum EnumVariant {
    /// A symbolic variant
    /// 
    /// Example:
    /// ```
    /// enum Color {
    ///     Red,
    ///     Green,
    ///     Blue,
    /// }
    /// ```
    Symbol {
        name: String,
        loc: Loc,
    },
    /// A tuple variant
    /// 
    /// Example:
    /// ```
    /// enum IP {
    ///     V4(u8, u8, u8, u8),
    ///     V6(String),
    /// }
    /// ```
    Tuple {
        name: String,
        fields: Vec<Type>,
        loc: Loc,
    },
    /// A struct variant
    /// 
    /// Example:
    /// ```
    /// enum Point<T> {
    ///     FourD {
    ///         x: T,
    ///         y: T,
    ///         z: T,
    ///         w: T,
    ///     },
    ///     ThreeD {
    ///         x: T,
    ///         y: T,
    ///         z: T,
    ///     },
    ///     TwoD {
    ///         x: T,
    ///         y: T,
    ///     },
    /// }
    /// ```
    Struct {
        name: String,
        fields: Vec<StructField>,
        loc: Loc,
    },
}

impl EnumVariant {
    pub fn loc(&self) -> Loc {
        match self {
            EnumVariant::Symbol { loc, .. } => *loc,
            EnumVariant::Tuple { loc, .. } => *loc,
            EnumVariant::Struct { loc, .. } => *loc,
        }
    }
}

/// An enum definition
/// 
/// ```
/// enum Color {
///     Red,
///     Green,
///     Blue,
/// }
/// ```
pub struct EnumDef {
    pub name: String,
    pub generics: GenericContext,
    pub variants: Vec<EnumVariant>,
    pub loc: Loc,
}

/// An alias definition
/// 
/// ```
/// type Number = i64;
/// type Point<T> = (T, T);
/// ```
pub struct AliasDef {
    pub name: String,
    pub generics: GenericContext,
    pub ty: Type,
    pub loc: Loc,
}

/// A required fn in a trait
/// 
/// ```
/// trait Add {
///     fn add(a: T, b: T) -> T;
/// }
/// ```
pub struct RequiredFn {
    pub name: String,
    pub generics: GenericContext,
    pub params: Vec<(String, Type)>,
    pub ret_ty: Type,
    pub loc: Loc,
}


/// A required field in a trait
/// 
/// ```
/// trait HasX {
///     x: i32;
/// }
/// ```
pub struct RequiredField {
    pub name: String,
    pub ty: Type,
    pub loc: Loc,
}

/// A trait definition
/// 
/// ```
/// trait HasX {
///     // required field
///     x: i32;
///     // a required fn
///     fn do_something_with_x(self) -> i32;
///     // a given fn
///     fn do_something_else_with_x(self) -> i32 {
///         let y = self.do_something_with_x();
///         return y + 1;
///     }
/// }
/// ```
pub struct TraitDef {
    pub name: String,
    pub generics: GenericContext,
    pub required_fns: Vec<RequiredFn>,
    pub given_fns: Vec<FnDef>,
    pub required_fields: Vec<RequiredField>,
    pub loc: Loc,
}
