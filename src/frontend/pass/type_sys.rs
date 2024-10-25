// the entirety of Chameleon's type system lives here


// define concrete types

use std::collections::HashMap;

pub enum ConcreteType {
    // for numeric types u8 is the number of bytes
    Int(u8),
    Float(u8),
    UInt(u8),
    Bool,
    String,
    // a type with no values ie ()
    Unit,
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
        }
    }
}

pub enum AppliedType {
    Function(Box<MonoType>, Box<MonoType>),
    List(Box<MonoType>),
    Option(Box<MonoType>),
    Result(Box<MonoType>, Box<MonoType>),
    Tuple(Vec<MonoType>),
    Union(Vec<MonoType>),
    Intersection(Vec<MonoType>),
}

impl ToString for AppliedType {
    fn to_string(&self) -> String {
        match self {
            AppliedType::Function(param, ret) => format!("Fn<{}, {}>", param.to_string(), ret.to_string()),
            AppliedType::List(t) => format!("List<{}>", t.to_string()),
            AppliedType::Option(t) => format!("Option<{}>", t.to_string()),
            AppliedType::Result(ok, err) => format!("Result<{}, {}>", ok.to_string(), err.to_string()),
            AppliedType::Tuple(ts) => format!("({})", ts.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", ")),
            AppliedType::Union(ts) => format!("{}", ts.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(" | ")),
            AppliedType::Intersection(ts) => format!("{}", ts.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(" & ")),
        }
    }
}

pub enum MonoType {
    TypeVar(String),
    Concrete(ConcreteType),
    Applied(AppliedType),
}

impl ToString for MonoType {
    fn to_string(&self) -> String {
        match self {
            MonoType::TypeVar(v) => v.clone(),
            MonoType::Concrete(c) => c.to_string(),
            MonoType::Applied(a) => a.to_string(),
        }
    }
}

