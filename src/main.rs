use backend::CodegenContext;
use frontend::{AstExpr, AstStatement, AstType, FnDef, Loc, Program};
use inkwell::context::Context;

mod frontend;
mod backend;

fn main() {
    let context = Context::create();
    let codegen_ctx = CodegenContext::new(&context);
    let mut program = Program::new();
    let main_fn = FnDef::new("main".to_string(), vec![], AstType::Int, vec![
        AstStatement::ConstDef { name: "a".to_string(), ty: AstType::Int, value: AstExpr::Int { value: 10, loc: Loc::default() }, loc: Loc::default() },
        AstStatement::Return { expr: Some(AstExpr::Ident { name: "a".to_string(), loc: Loc::default() }), loc: Loc::default() },
    ]);
    program.add_fn_def(main_fn);
    program.codegen(&codegen_ctx).unwrap();
    println!("{}", codegen_ctx.ir_to_string());
}
