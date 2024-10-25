// the entirety of Chameleon's type system lives here

use std::collections::HashMap;

pub enum EnumVariant {
    Unit(String),
    Tuple(Vec<MonoType>),
    Record {
        name: String,
        fields: HashMap<String, MonoType>,
    },
}

// define concrete types
pub enum ConcreteType {
    // for numeric types u8 is the number of bytes
    Int(u8),
    Float(u8),
    UInt(u8),
    Bool,
    String,
    // a type with no values ie ()
    Unit,
    Function {
        name: Option<String>,
        params: Vec<MonoType>,
        ret: Box<MonoType>,
    },
    Struct {
        name: String,
        fields: HashMap<String, MonoType>,
    },
    Enum {
        name: String,
        variants: Vec<EnumVariant>,
    },
}

impl ToString for ConcreteType {
    fn to_string(&self) -> String {
        match self {
            ConcreteType::Int(width) => format!("i{}", width * 8),
            ConcreteType::Float(width) => format!("f{}", width * 8),
            ConcreteType::UInt(width) => format!("u{}", width * 8),
            ConcreteType::Bool => "bool".to_string(),
            ConcreteType::String => "string".to_string(),
            ConcreteType::Unit => "unit".to_string(),
            ConcreteType::Struct { name, fields } => format!("{}", name),
            ConcreteType::Enum { name, variants } => format!("{}", name),
            ConcreteType::Function { name, params, ret } => format!("Fn({}) -> {}", params.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", "), ret.to_string()),
        }
    }
}

/// Applied types
pub enum AppliedType {
    Function {
        name: Option<String>,
        type_args: Vec<MonoType>,
        params: Vec<MonoType>,
        ret: Box<MonoType>,
    },
    Record {
        name: String,
        type_args: Vec<MonoType>,
        fields: HashMap<String, MonoType>,
    },
    Enum {
        name: String,
        type_args: Vec<MonoType>,
        variants: Vec<EnumVariant>,
    },
}

impl ToString for AppliedType {
    fn to_string(&self) -> String {
        match self {
            AppliedType::Function { name, type_args, params, ret } => format!("Fn<{}>({}) -> {}", type_args.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", "), params.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", "), ret.to_string()),
            AppliedType::Record { name, type_args, fields } => format!("{}<{}>", name, type_args.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", ")),
            AppliedType::Enum { name, type_args, variants } => format!("{}<{}>", name, type_args.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", ")),
        }
    }
}

/// Monomorphic types
pub enum MonoType {
    TypeVar(TypeVar),
    Concrete(ConcreteType),
    Applied(AppliedType),
}

impl ToString for MonoType {
    fn to_string(&self) -> String {
        match self {
            MonoType::Concrete(c) => c.to_string(),
            MonoType::Applied(a) => a.to_string(),
            MonoType::TypeVar(v) => v.to_string(),
        }
    }
}

/// A type variable
pub struct TypeVar {
    pub name: String,
}

impl ToString for TypeVar {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

impl From<String> for TypeVar {
    fn from(name: String) -> Self {
        TypeVar { name }
    }
}

// TODO: add polymorphic types