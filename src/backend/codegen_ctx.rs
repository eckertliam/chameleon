use std::collections::HashMap;

use inkwell::values::PointerValue;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

pub struct CodegenContext<'ctx> {
    builder: Builder<'ctx>,
    context: &'ctx Context,
    module: Module<'ctx>,
    symbol_table: HashMap<String, PointerValue<'ctx>>
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

