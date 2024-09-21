use crate::frontend::{AstExpr, BinOpKind, Loc};



pub enum CodegenError {
    /// Error when trying to perform binop on two types that is not supported
    UnimplementedOp(BinOpKind, AstExpr, AstExpr, Loc),
    UndefinedSymbol(String, Loc),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CodegenError::UnimplementedOp(op, lhs, rhs, loc) => {
                write!(f, "{:?} is not supported for expressions {:?} and {:?} at {:?}", op, lhs, rhs, loc)
            }
            CodegenError::UndefinedSymbol(symbol, loc) => {
                write!(f, "{} at {:?} is undefined in the current context", symbol, loc)
            }
        }
    }
}