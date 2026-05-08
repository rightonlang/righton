pub mod ast;
pub mod borrow_checker;
pub mod compiler;
pub mod lexer;
pub mod llvm;
pub mod parser;
pub mod type_checker;

#[derive(Debug, Clone)]
pub struct CompileObjectOptions {
    pub target: Option<String>,
    pub cpu: Option<String>,
    pub no_pie: bool,
    pub codegen_level: Option<String>,
}
