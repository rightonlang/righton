mod ast;
mod compiler;
mod lexer;
mod llvm;
mod parser;

use clap::Parser;
use compiler::*;
use lexer::*;
use llvm_sys::target::{
    LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos, LLVM_InitializeAllTargetMCs,
    LLVM_InitializeAllTargets,
};
use std::fs;
use std::path::Path;

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long)]
    input: Option<String>,
    #[arg(short, long)]
    output: Option<String>,

    #[arg(short, long)]
    profile: Option<String>,
    #[arg(short, long)]
    target: Option<String>,
    #[arg(short, long)]
    cpu: Option<String>,

    #[clap(long)]
    no_pie: bool,

    #[clap(long)]
    codegen_level: Option<String>,
}

fn main() {
    let cli = Cli::parse();

    let input_path = match cli.input {
        Some(ref path) => {
            if !Path::new(path).exists() {
                eprintln!("Error: input file '{}' does not exist.", path);
                std::process::exit(1);
            }
            path
        }
        None => {
            eprintln!("Error: input file not specified. Use -i or --input.");
            std::process::exit(1);
        }
    };

    let output_path: &String = match cli.output.as_deref() {
        Some(ref path) => &path.to_string(),
        None => {
            eprintln!("Error: output file not specified. Use -o or --output.");
            std::process::exit(1);
        }
    };

    let profile = cli
        .profile
        .as_deref()
        .unwrap_or(&*"debug".to_string())
        .to_lowercase();
    if profile != "debug" && profile != "release" {
        eprintln!("Error: profile must be either 'debug' or 'release'.");
        std::process::exit(1);
    }

    let code = String::from_utf8(fs::read(input_path).unwrap()).unwrap();
    let name = Path::new(input_path)
        .file_stem()    
        .and_then(|s| s.to_str())
        .unwrap();   

    let mut lexer = Lexer::new(&*code);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse_program(profile, name.to_string());

    let mut r#gen = LLVMTextGen::new();
    let ir = r#gen.generate(&program);
    let output_type;

    println!("LLVM Ir:\n{}", ir);

    if output_path.ends_with(".o") || output_path.ends_with(".obj") {
        output_type = "object";
        unsafe {
            println!("initializing LLVM targets…");
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmPrinters();
        }
        println!("LLVM targets initialized");
        let cloned_cli = cli.clone();
        llvm::compile_object(cloned_cli, ir.clone(), output_path.parse().unwrap(), name.to_string());
    } else {
        output_type = "ir";
        fs::write(&output_path, &ir).unwrap();
    }

    if output_type == "object" {
        println!("object saved to {}", output_path);
    } else if output_type == "ir" {
        println!("LLVM Ir saved to {}", output_path);
    }
}
