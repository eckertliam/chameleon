use inkwell::values::AnyValueEnum;

use super::{codegen_ctx::CodegenContext, codegen_err::CodegenError};

pub type IRResult<'ir> = Result<AnyValueEnum<'ir>, CodegenError>;

pub trait Codegen<'ctx, 'ir> 
where
    'ctx: 'ir,
{
    fn codegen(&self, context: &CodegenContext) -> IRResult<'ir>;
}