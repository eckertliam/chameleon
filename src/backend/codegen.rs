use inkwell::{types::{AnyType, AnyTypeEnum}, values::{AnyValue, AnyValueEnum}};

use crate::frontend::{AstExpr, AstType};

use super::{codegen_ctx::CodegenContext, codegen_err::CodegenError};

type IRResult<T> = Result<T, CodegenError>;

pub type IRValueResult<'ir> = IRResult<AnyValueEnum<'ir>>;

pub type IRTypeResult<'ir> = IRResult<AnyTypeEnum<'ir>>;

pub trait ValueCodegen<'ctx, 'ir> 
where 'ctx: 'ir
{
     fn codegen(&self, context: &CodegenContext<'ctx>) -> IRValueResult<'ir>;
}

impl<'ctx, 'ir> ValueCodegen<'ctx, 'ir> for AstExpr 
where 'ctx: 'ir
{
    fn codegen(&self, codegen_context: &CodegenContext<'ctx>) -> IRValueResult<'ir> {
        match self {
            AstExpr::Int { value, .. } => {
                let int_value = codegen_context.context.i64_type().const_int(*value as u64, false);
                Ok(int_value.as_any_value_enum())
            }
            AstExpr::Float { value, .. } => {
                let float_value = codegen_context.context.f64_type().const_float(*value);
                Ok(float_value.as_any_value_enum())
            }
            AstExpr::Ident { name, loc } => {
                // we have to see if the identifier is defined in the symbol table
                match codegen_context.symbol_table.get(name) {
                    Some((ptr_val, pointee_ty)) => {
                        // we'll return a load instruction
                        let load_instr = codegen_context
                            .builder
                            .build_load(*pointee_ty, *ptr_val, "load_val")
                            .expect("Error: An error occured while building a load instruction");
                        Ok(load_instr.as_any_value_enum())
                    }
                    None => Err(CodegenError::UndefinedSymbol(name.clone(), *loc))
                }
            }
            _ => unimplemented!("codegen for {:?} has not been implemented", self)
        }
    }
}

impl<'ctx, 'ir> AstType
where 'ctx: 'ir
{
    fn codegen(&self, codegen_context: &CodegenContext<'ctx>) -> IRTypeResult<'ir> {
        match self {
            AstType::Int => Ok(codegen_context.context.i64_type().as_any_type_enum()),
            AstType::Float => Ok(codegen_context.context.f64_type().as_any_type_enum()),
            AstType::UnsignedInt => Ok(codegen_context.context.i64_type().as_any_type_enum()),
            _ => unimplemented!("codegen for {:?} has not been implemented", self)
        }
    }
}