use crate::frontend::{AstType, BinOpKind};



pub enum CodegenError {
    /// Error when trying to perform binop on two types that is not supported
    BadBinOp(BinOpKind, AstType, AstType),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CodegenError::BadBinOp(op, lhs, rhs) => {
                write!(f, "{:?} is not supported for types {:?} and {:?}", op, lhs, rhs)
            }
        }
    }
}