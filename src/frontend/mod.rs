mod ast;
mod tokenizer;
mod parser;
mod pass;

pub use ast::*;
pub use tokenizer::*;

// TODO: implement the type checker
// TODO: implement constant folding