use std::collections::HashMap;

use inkwell::values::PointerValue;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;

pub struct CodegenContext<'ctx> {
    pub builder: Builder<'ctx>,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub symbol_table: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>
}

impl<'ctx> CodegenContext<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();

        Self {
            builder,
            context,
            module,
            symbol_table: HashMap::new(),
        }
    }

    pub fn ir_to_string(&self) -> String {
        self.module.print_to_string().to_string()
    }
}

