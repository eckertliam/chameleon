use std::collections::HashMap;

use crate::frontend::{Loc, Program, Type, Expr, Stmt};

#[derive(Debug)]
enum TypeCheckError {
    DuplicateDefinition { name: String, original: Loc, new: Loc },
    UnboundIdentifier { name: String, loc: Loc },
    IncompatibleTypes { expected: Type, actual: Type, loc: Loc },
}

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DuplicateDefinition { name, original, new } => write!(f, "Duplicate definition of {} at {} and {}", name, original, new),
            Self::UnboundIdentifier { name, loc } => write!(f, "Unbound identifier {} at {}", name, loc),
            Self::IncompatibleTypes { expected, actual, loc } => write!(f, "Incompatible types: expected {:?}, got {:?} at {}", expected, actual, loc),
        }
    }
}

impl std::error::Error for TypeCheckError {}

/// contains all type bindings in the current scope
/// 
/// for example:
/// if x: i32 an entry would be key: "x" value: Type::I32
type TypeCheckCtx = HashMap<String, Type>;

struct TypeCheckState {
    scopes: Vec<TypeCheckCtx>,
    errors: Vec<TypeCheckError>,
}

impl TypeCheckState {
    fn new() -> Self {
        Self { scopes: vec![HashMap::new()], errors: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn add_error(&mut self, error: TypeCheckError) {
        self.errors.push(error);
    }

    pub fn errors(&self) -> &[TypeCheckError] {
        &self.errors
    }

    pub fn push_type(&mut self, name: String, ty: Type) {
        self.scopes.last_mut().unwrap().insert(name, ty);
    }

    pub fn get_type(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }
}

pub fn type_check(program: &Program) -> Result<(), Vec<TypeCheckError>> {
    let mut state = TypeCheckState::new();
    Ok(())
}

/// get the type of an expression
fn resolve_expr_type(expr: &Expr, state: &mut TypeCheckState) -> Result<Type, TypeCheckError> {
    match expr {
        Expr::Ident { name, loc } => {
            if let Some(ty) = state.get_type(name) {
                Ok(ty)
            } else {
                Err(TypeCheckError::UnboundIdentifier { name: name.clone(), loc: *loc })
            }
        }
        Expr::Int { value, loc } => Ok(Type::I64(*loc)),
        Expr::Float { value, loc } => Ok(Type::F64(*loc)),
        Expr::String { value, loc } => Ok(Type::String(*loc)),
        Expr::Bool { value, loc } => Ok(Type::Bool(*loc)),
        Expr::Array { values, loc } => {
            // iterate over all values and make sure they are all the same type
            let mut prev_ty = resolve_expr_type(&values[0], state)?;
            let size = values.len() as i64;
            for value in values.iter().skip(1) {
                let ty = resolve_expr_type(value, state)?;
                if ty != prev_ty {
                    return Err(TypeCheckError::IncompatibleTypes { expected: prev_ty, actual: ty, loc: *loc });
                }
            }
            Ok(Type::Array(Box::new(prev_ty), Expr::Int { value: size, loc: *loc }, *loc))
        }
        // TODO: add more cases
        _ => unimplemented!("Type Resolution for {:?}", expr),
    }
}
