use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use inkwell::values::PointerValue;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;

pub struct CodegenContext<'ctx> {
    pub builder: Builder<'ctx>,
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub symbol_table: RefCell<SymbolTable<'ctx>>,
}

pub struct SymbolTable<'ctx> {
    pub symbol_tables: Vec<HashMap<String, SymbolValue<'ctx>>>,
    pub current_scope: usize,
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn new() -> Self {
        Self {
            symbol_tables: vec![HashMap::new()],
            current_scope: 0,
        }
    }

    pub fn insert(&mut self, name: String, symbol_value: SymbolValue<'ctx>) {
        self.symbol_tables[self.current_scope].insert(name, symbol_value);
    }

    pub fn get(&self, name: &str) -> Option<&SymbolValue<'ctx>> {
        for i in (0..=self.current_scope).rev() {
            if let Some(symbol_value) = self.symbol_tables[i].get(name) {
                return Some(symbol_value);
            }
        }
        None
    }

    pub fn push_scope(&mut self) {
        self.symbol_tables.push(HashMap::new());
        self.current_scope += 1;
    }

    pub fn pop_scope(&mut self) {
        self.symbol_tables.pop();
        self.current_scope -= 1;
    }
}

pub struct SymbolValue<'ctx> {
    pub mutable: bool,
    pub ptr_val: PointerValue<'ctx>,
    pub pointee_ty: BasicTypeEnum<'ctx>,
}

impl<'ctx> SymbolValue<'ctx> {
    pub fn constant(ptr_val: PointerValue<'ctx>, pointee_ty: BasicTypeEnum<'ctx>) -> Self {
        Self {
            mutable: false,
            ptr_val,
            pointee_ty,
        }
    }

    pub fn mutable(ptr_val: PointerValue<'ctx>, pointee_ty: BasicTypeEnum<'ctx>) -> Self {
        Self {
            mutable: true,
            ptr_val,
            pointee_ty,
        }
    }
}

impl<'ctx> CodegenContext<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();

        Self {
            builder,
            context,
            module,
            symbol_table: RefCell::new(SymbolTable::new()),
        }
    }

    pub fn ir_to_string(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub fn emit_bitcode(&self, file_name: &str) {
        let path = Path::new(file_name);
        self.module.write_bitcode_to_path(path);
    }
}

