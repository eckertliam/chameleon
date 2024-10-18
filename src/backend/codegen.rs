use inkwell::{
    builder::Builder, module::Linkage, types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, IntType}, values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FloatValue, IntValue}
};
use crate::frontend::{Expr, Stmt, Type, BinOpKind, FnDef, Program};

use super::{codegen_ctx::{CodegenContext, SymbolValue}, codegen_err::CodegenError};

type IRResult<T> = Result<T, CodegenError>;

pub type IRValueResult<'ir> = IRResult<AnyValueEnum<'ir>>;

pub type IRTypeResult<'ir> = IRResult<AnyTypeEnum<'ir>>;

pub trait Codegen<'ctx, 'ir> 
where 'ctx: 'ir
{
     fn codegen(&self, context: &CodegenContext<'ctx>) -> IRValueResult<'ir>;
}

impl<'ctx, 'ir> Codegen<'ctx, 'ir> for Expr 
where 'ctx: 'ir
{
    fn codegen(&self, codegen_context: &CodegenContext<'ctx>) -> IRValueResult<'ir> {
        match self {
            Expr::Int { value, .. } => {
                let int_value = codegen_context.context.i64_type().const_int(*value as u64, false);
                Ok(int_value.as_any_value_enum())
            }
            Expr::Float { value, .. } => {
                let float_value = codegen_context.context.f64_type().const_float(*value);
                Ok(float_value.as_any_value_enum())
            }
            Expr::Ident { name, loc } => {
                // we have to see if the identifier is defined in the symbol table
                match codegen_context.symbol_table.borrow().get(name) {
                    Some(symbol_value) => {
                        let ptr = symbol_value.ptr_val;
                        let ty = symbol_value.pointee_ty;
                        let load_instr = codegen_context.builder.build_load(ty, ptr, name)
                            .expect("Error: An error occured while building a load instruction");
                        Ok(load_instr.as_any_value_enum())
                    }
                    None => Err(CodegenError::UndefinedSymbol(name.clone(), *loc))
                }
            }
            Expr::BinOp { lhs, rhs, kind, loc } => {
                let lhs_val = lhs.codegen(codegen_context)?;
                let rhs_val = rhs.codegen(codegen_context)?;
                match kind {
                    BinOpKind::Add => {
                        match (lhs_val, rhs_val) {
                            (AnyValueEnum::IntValue(lhs_int), AnyValueEnum::IntValue(rhs_int)) => {
                                let result = codegen_context.builder.build_int_add(lhs_int, rhs_int, "add");
                                if let Ok(instr_val) = result {
                                    Ok(instr_val.as_any_value_enum())
                                } else {
                                    panic!("Error: An error occured while building add instruction at location {:?} {:?}", loc, result.err())
                                }
                            }
                            (AnyValueEnum::FloatValue(lhs_float), AnyValueEnum::FloatValue(rhs_float)) => {
                                let result = codegen_context.builder.build_float_add(lhs_float, rhs_float, "add");
                                if let Ok(instr_val) = result {
                                    Ok(instr_val.as_any_value_enum())
                                } else {
                                    panic!("Error: An error occured while building add instruction at location {:?} {:?}", loc, result.err())
                                }
                            }
                            _ => Err(CodegenError::UnimplementedOp(*kind, *lhs.clone(), *rhs.clone(), *loc))
                        }
                    }
                    BinOpKind::Sub => {
                        match (lhs_val, rhs_val) {
                            (AnyValueEnum::IntValue(lhs_int), AnyValueEnum::IntValue(rhs_int)) => {
                                let result = codegen_context.builder.build_int_sub(lhs_int, rhs_int, "sub");
                                if let Ok(instr_val) = result {
                                    Ok(instr_val.as_any_value_enum())
                                } else {
                                    panic!("Error: An error occured while building sub instruction at location {:?} {:?}", loc, result.err())
                                }
                            }
                            (AnyValueEnum::FloatValue(lhs_float), AnyValueEnum::FloatValue(rhs_float)) => {
                                let result = codegen_context.builder.build_float_sub(lhs_float, rhs_float, "sub");
                                if let Ok(instr_val) = result {
                                    Ok(instr_val.as_any_value_enum())
                                } else {
                                    panic!("Error: An error occured while building sub instruction at location {:?} {:?}", loc, result.err())
                                }
                            }
                            _ => Err(CodegenError::UnimplementedOp(*kind, *lhs.clone(), *rhs.clone(), *loc))
                        }
                    }
                    BinOpKind::Mul => {
                        match (lhs_val, rhs_val) {
                            (AnyValueEnum::IntValue(lhs_int), AnyValueEnum::IntValue(rhs_int)) => {
                                let result = codegen_context.builder.build_int_mul(lhs_int, rhs_int, "mul");
                                if let Ok(instr_val) = result {
                                    Ok(instr_val.as_any_value_enum())
                                } else {
                                    panic!("Error: An error occured while building mul instruction at location {:?} {:?}", loc, result.err())
                                }
                            }
                            (AnyValueEnum::FloatValue(lhs_float), AnyValueEnum::FloatValue(rhs_float)) => {
                                let result = codegen_context.builder.build_float_mul(lhs_float, rhs_float, "mul");
                                if let Ok(instr_val) = result {
                                    Ok(instr_val.as_any_value_enum())
                                } else {
                                    panic!("Error: An error occured while building mul instruction at location {:?} {:?}", loc, result.err())
                                }
                            }
                            _ => Err(CodegenError::UnimplementedOp(*kind, *lhs.clone(), *rhs.clone(), *loc))
                        }
                    }
                    BinOpKind::Div => {
                        match (lhs_val, rhs_val) {
                            (AnyValueEnum::IntValue(lhs_int), AnyValueEnum::IntValue(rhs_int)) => {
                                let result = codegen_context.builder.build_int_signed_div(lhs_int, rhs_int, "div");
                                if let Ok(instr_val) = result {
                                    Ok(instr_val.as_any_value_enum())
                                } else {
                                    panic!("Error: An error occured while building div instruction at location {:?} {:?}", loc, result.err())
                                }
                            }
                            (AnyValueEnum::FloatValue(lhs_float), AnyValueEnum::FloatValue(rhs_float)) => {
                                let result = codegen_context.builder.build_float_div(lhs_float, rhs_float, "div");
                                if let Ok(instr_val) = result {
                                    Ok(instr_val.as_any_value_enum())
                                } else {
                                    panic!("Error: An error occured while building div instruction at location {:?} {:?}", loc, result.err())
                                }
                            }
                            _ => Err(CodegenError::UnimplementedOp(*kind, *lhs.clone(), *rhs.clone(), *loc))
                        }
                    }
                    _ => Err(CodegenError::UnimplementedOp(*kind, *lhs.clone(), *rhs.clone(), *loc)),
                }
            }
            _ => unimplemented!("codegen for {:?} has not been implemented", self)
        }
    }
}

impl<'ctx, 'ir> Codegen<'ctx, 'ir> for Stmt
where 'ctx: 'ir
{
    fn codegen(&self, codegen_context: &CodegenContext<'ctx>) -> IRValueResult<'ir> {
        match self {
            Stmt::Expr(expr) => expr.codegen(codegen_context),
            Stmt::ConstDef { name, ty, value, loc } => {
                // generate the IR value then convert it from an AnyValueEnum to a AnyValueEnum
                let value = match BasicValueEnum::try_from(value.codegen(codegen_context)?) {
                    Ok(basic_val) => basic_val,
                    Err(_) => panic!("Error: An error occured while converting value to BasicValueEnum")
                };
                // get the type of the constant definition and convert it from an AnyTypeEnum to a BasicTypeEnum
                let ty = match BasicTypeEnum::try_from(ty.as_ref().unwrap().codegen(codegen_context)?) {
                    Ok(basic_ty) => basic_ty,
                    Err(_) => panic!("Error: An error occured while converting type to BasicTypeEnum")
                };
                // allocate memory for the constant definition
                let value_ptr = codegen_context.builder.build_alloca(ty, name.as_str()).unwrap();
                // create an instruction to store the value in the allocated memory
                let store_instr = codegen_context.builder.build_store(value_ptr, value).unwrap();
                // store the symbol value in the symbol table
                let symbol_value = SymbolValue::constant(value_ptr, ty);
                codegen_context.symbol_table.borrow_mut().insert(name.clone(), symbol_value);
                Ok(store_instr.as_any_value_enum())
            }
            Stmt::LetDef { name, ty, value, .. } => {
                // generate the IR value then convert it from an AnyValueEnum to a AnyValueEnum
                let value = match BasicValueEnum::try_from(value.codegen(codegen_context)?) {
                    Ok(basic_val) => basic_val,
                    Err(_) => panic!("Error: An error occured while converting value to BasicValueEnum")
                };
                // get the type of the constant definition and convert it from an AnyTypeEnum to a BasicTypeEnum
                let ty = match BasicTypeEnum::try_from(ty.as_ref().unwrap().codegen(codegen_context)?) {
                    Ok(basic_ty) => basic_ty,
                    Err(_) => panic!("Error: An error occured while converting type to BasicTypeEnum")
                };
                // allocate memory for the constant definition
                let value_ptr = codegen_context.builder.build_alloca(ty, name.as_str()).unwrap();
                // create an instruction to store the value in the allocated memory
                let store_instr = codegen_context.builder.build_store(value_ptr, value).unwrap();
                // store the symbol value in the symbol table
                let symbol_value = SymbolValue::mutable(value_ptr, ty);
                codegen_context.symbol_table.borrow_mut().insert(name.clone(), symbol_value);
                Ok(store_instr.as_any_value_enum())
            }
            Stmt::Return { expr, loc } => {
                let return_expr = if let Some(e) = expr {
                    let ir_expr = match BasicValueEnum::try_from(e.codegen(codegen_context)?) {
                        Ok(basic_val) => basic_val,
                        Err(_) => panic!("Error: An error occured while converting value to BasicValueEnum")
                    };
                    Some(ir_expr)
                } else {
                    None
                };
                if let Some(basic_val_enum) = return_expr {
                    let return_instr = codegen_context.builder.build_return(Some(&basic_val_enum));
                    if let Ok(instr_val) = return_instr {
                        return Ok(instr_val.as_any_value_enum())
                    }
                }
                if let Ok(instr_val) = codegen_context.builder.build_return(None) {
                    return Ok(instr_val.as_any_value_enum())
                } else {
                    panic!("Error: An error occured while building return instruction at location {:?}", loc)
                }
            }
            _ => unimplemented!("codegen for {:?} has not been implemented", self)
        }
    }
}

impl<'ctx, 'ir> Type
where 'ctx: 'ir
{
    fn codegen(&self, codegen_context: &CodegenContext<'ctx>) -> IRTypeResult<'ir> {
        match self {
            Type::Int => Ok(codegen_context.context.i64_type().as_any_type_enum()),
            Type::Float => Ok(codegen_context.context.f64_type().as_any_type_enum()),
            Type::UnsignedInt => Ok(codegen_context.context.i64_type().as_any_type_enum()),
            _ => unimplemented!("codegen for {:?} has not been implemented", self)
        }
    }
}

impl<'ctx, 'ir> Codegen<'ctx, 'ir> for FnDef
where 'ctx: 'ir
{
    fn codegen(&self, codegen_context: &CodegenContext<'ctx>) -> IRValueResult<'ir> {
        // push a new scope to the symbol table
        codegen_context.symbol_table.borrow_mut().push_scope();
        let ir_ret_ty = match BasicTypeEnum::try_from(self.ret_ty.codegen(codegen_context)?) {
            Ok(basic_ty) => basic_ty,
            Err(_) => panic!("Error: An error occured while converting type to BasicType")
        };
        let mut ir_param_tys = Vec::new();
        for (_, param_ty) in self.params.iter() {
            let any_ty = param_ty.codegen(codegen_context)?;
            let basic_ty = match BasicTypeEnum::try_from(any_ty) {
                Ok(basic_ty) => basic_ty,
                Err(_) => panic!("Error: An error occured while converting type to BasicType")
            };
            let meta_ty = match BasicMetadataTypeEnum::try_from(basic_ty) {
                Ok(meta_ty) => meta_ty,
                Err(_) => panic!("Error: An error occured while converting type to BasicMetadataType")
            };
            ir_param_tys.push(meta_ty);
        }
        let fn_ty = ir_ret_ty.fn_type(&ir_param_tys, false);
        let fn_val = codegen_context.module.add_function(&self.name, fn_ty, None);
        // create a basic block for the function
        let entry_bb = codegen_context.context.append_basic_block(fn_val, "entry");
        codegen_context.builder.position_at_end(entry_bb);
        
        for (param, arg) in self.params.iter().zip(fn_val.get_param_iter()) {
            // set the argument name
            arg.set_name(param.0.as_str());
            // allocate memory for the argument
            let alloca_ptr = codegen_context.builder.build_alloca(arg.get_type(), param.0.as_str()).unwrap();
            // store the argument in the allocated memory
            codegen_context.builder.build_store(alloca_ptr, arg).unwrap();
            let symbol_value = SymbolValue::mutable(alloca_ptr, arg.get_type());
            codegen_context.symbol_table.borrow_mut().insert(param.0.clone(), symbol_value);
        }

        for stmt in self.body.iter() {
            stmt.codegen(codegen_context)?;
        }

        // pop the scope from the symbol table
        codegen_context.symbol_table.borrow_mut().pop_scope();

        Ok(fn_val.as_any_value_enum())
    }
}

impl <'ctx, 'ir> Program
where 'ctx: 'ir
{
    pub fn codegen(&self, codegen_context: &CodegenContext<'ctx>) -> IRResult<()> {
        for fn_def in self.fn_defs.iter() {
            fn_def.codegen(codegen_context)?;
        }
        Ok(())
    }
}