use std::collections::HashMap;
use crate::frontend::Loc;

use super::parser::ParserError;

pub struct Program {
    /// store the location of each definition in order to report duplicates
    pub namespace: HashMap<String, Loc>,
    pub fn_defs: HashMap<String, FnDef>,
    pub struct_defs: HashMap<String, StructDef>,
    pub enum_defs: HashMap<String, EnumDef>,
    pub alias_defs: HashMap<String, AliasDef>,
    pub trait_defs: HashMap<String, TraitDef>,
    pub impl_blocks: Vec<ImplBlock>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            namespace: HashMap::new(),
            fn_defs: HashMap::new(),
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            alias_defs: HashMap::new(),
            trait_defs: HashMap::new(),
            impl_blocks: Vec::new(),
        }
    }

    fn add_definition(&mut self, name: &str, loc: Loc) -> Result<(), ParserError> {
        if let Some(existing) = self.namespace.get(name) {
            return Err(ParserError::DuplicateDefinition { name: name.to_string(), original: *existing, new: loc });
        }
        self.namespace.insert(name.to_string(), loc);
        Ok(())
    }

    pub fn add_fn_def(&mut self, fn_def: FnDef) -> Result<(), ParserError> {
        self.add_definition(&fn_def.name, fn_def.loc)?;
        self.fn_defs.insert(fn_def.name.clone(), fn_def);
        Ok(())
    }

    pub fn add_struct_def(&mut self, struct_def: StructDef) -> Result<(), ParserError> {
        self.add_definition(&struct_def.name, struct_def.loc)?;
        self.struct_defs.insert(struct_def.name.clone(), struct_def);
        Ok(())
    }

    pub fn add_enum_def(&mut self, enum_def: EnumDef) -> Result<(), ParserError> {
        self.add_definition(&enum_def.name, enum_def.loc)?;
        self.enum_defs.insert(enum_def.name.clone(), enum_def);
        Ok(())
    }

    pub fn add_alias_def(&mut self, alias_def: AliasDef) -> Result<(), ParserError> {
        self.add_definition(&alias_def.name, alias_def.loc)?;
        self.alias_defs.insert(alias_def.name.clone(), alias_def);
        Ok(())
    }

    pub fn add_trait_def(&mut self, trait_def: TraitDef) -> Result<(), ParserError> {
        self.add_definition(&trait_def.name, trait_def.loc)?;
        self.trait_defs.insert(trait_def.name.clone(), trait_def);
        Ok(())
    }

    pub fn add_impl_block(&mut self, impl_block: ImplBlock) -> Result<(), ParserError> {
        // make sure that no trait is implemented twice for the same type
        if let Some(new_trait_) = &impl_block.trait_ {
            for existing in self.impl_blocks.iter() {
                if let Some(existing_trait_) = &existing.trait_ {
                    if existing_trait_ == new_trait_ {
                        return Err(ParserError::DuplicateDefinition { name: new_trait_.to_string(), original: existing.loc, new: impl_block.loc });
                    }
                }
            }
        }
        self.impl_blocks.push(impl_block);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    FieldAccess {
        expr: Box<Expr>,
        field: Box<Expr>,
        loc: Loc,
    },
    PathAccess {
        expr: Box<Expr>,
        path: Vec<Expr>,
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
            Expr::FieldAccess { loc, .. } => *loc,
            Expr::PathAccess { loc, .. } => *loc,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOpKind {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Return {
        expr: Option<Expr>,
        loc: Loc,
    },
    Expr(Expr),
    LetDef {
        name: String,
        ty: Option<TypeExpr>,
        value: Expr,
        loc: Loc,
    },
    ConstDef {
        name: String,
        ty: Option<TypeExpr>,
        value: Expr,
        loc: Loc,
    },
    Assign {
        var: Expr,
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
    // TODO: match stmt
}

/// All type annotations
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
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
    String(Loc),
    Tuple(Vec<TypeExpr>, Loc),
    Array(Box<TypeExpr>, Expr, Loc),
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
    Generic(String, Vec<TypeExpr>, Loc),
}

impl ToString for TypeExpr {
    fn to_string(&self) -> String {
        match self {
            TypeExpr::I8(_) => "i8".to_string(),
            TypeExpr::I16(_) => "i16".to_string(),
            TypeExpr::I32(_) => "i32".to_string(),
            TypeExpr::I64(_) => "i64".to_string(),
            TypeExpr::U8(_) => "u8".to_string(),
            TypeExpr::U16(_) => "u16".to_string(),
            TypeExpr::U32(_) => "u32".to_string(),
            TypeExpr::U64(_) => "u64".to_string(),
            TypeExpr::F32(_) => "f32".to_string(),
            TypeExpr::F64(_) => "f64".to_string(),
            TypeExpr::Bool(_) => "bool".to_string(),
            TypeExpr::Void(_) => "void".to_string(),
            TypeExpr::String(_) => "string".to_string(),
            TypeExpr::Tuple(tys, _) => format!("({})", tys.iter().map(|ty| ty.to_string()).collect::<Vec<String>>().join(", ")),
            TypeExpr::Array(ty, size, _) => format!("Array<{}, {:?}>", ty.to_string(), size),
            TypeExpr::Alias(name, _) => name.clone(),
            TypeExpr::Generic(name, tys, _) => format!("{}<{}>", name, tys.iter().map(|ty| ty.to_string()).collect::<Vec<String>>().join(", ")),
        }
    }
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
    pub params: Vec<(String, TypeExpr)>,
    pub ret_ty: TypeExpr,
    pub body: Vec<Stmt>,
    pub loc: Loc,
}

impl FnDef {
    pub fn new(name: String, generics: GenericContext, params: Vec<(String, TypeExpr)>, ret_ty: TypeExpr, body: Vec<Stmt>, loc: Loc) -> Self {
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
    // defaults to false
    pub is_private: bool,
    pub name: String,
    pub ty: TypeExpr,
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
    Unit {
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
        fields: Vec<TypeExpr>,
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
            EnumVariant::Unit { loc, .. } => *loc,
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
    pub ty: TypeExpr,
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
    pub params: Vec<(String, TypeExpr)>,
    pub ret_ty: TypeExpr,
    pub loc: Loc,
}

pub type GivenFn = FnDef;

pub enum TraitFn {
    Required(RequiredFn),
    Given(GivenFn),
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
    pub ty: TypeExpr,
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

/// An impl block
/// 
/// ```
/// impl HasX for Point<T> {
///     fn do_something_with_x(self) -> i32 {
///         return self.x;
///     }
/// }
/// ```
pub struct ImplBlock {
    // none if it's an impl for a struct
    pub trait_: Option<TypeExpr>,
    pub generics: GenericContext,
    pub for_type: TypeExpr,
    pub fns: Vec<FnDef>,
    pub loc: Loc,
}
