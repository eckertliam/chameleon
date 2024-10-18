#[macro_use]
extern crate lazy_static;

use backend::CodegenContext;
use frontend::{Expr, Stmt, Type, FnDef, Loc, Program};
use inkwell::context::Context;

mod frontend;
mod backend;

fn main() {
    let context = Context::create();
    let codegen_ctx = CodegenContext::new(&context);
    let mut program = Program::new();
    let main_fn = FnDef::new("main".to_string(), vec![], Type::Int, vec![
        Stmt::ConstDef { name: "a".to_string(), ty: Some(Type::Int), value: Expr::Int { value: 10, loc: Loc::default() }, loc: Loc::default() },
        Stmt::Return { expr: Some(Expr::Ident { name: "a".to_string(), loc: Loc::default() }), loc: Loc::default() },
    ]);
    program.add_fn_def(main_fn);
    program.codegen(&codegen_ctx).unwrap();
    println!("{}", codegen_ctx.emit_ir());
    codegen_ctx.emit_asm("main.o");
}
