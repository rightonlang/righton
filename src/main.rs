use clap::Parser;
use righton::compiler::LLVMTextGen;
use righton::lexer::Lexer;
use righton::llvm;
use righton::parser;
use righton::CompileObjectOptions;
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
    if let Err(err) = run() {
        eprintln!("Error: {}", err);
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let cli = Cli::parse();

    let input_path = match cli.input {
        Some(path) => {
            if !Path::new(&path).exists() {
                return Err(format!("input file '{}' does not exist.", path));
            }
            path
        }
        None => {
            return Err("input file not specified. Use -i or --input.".to_string());
        }
    };

    let output_path: String = match cli.output {
        Some(path) => path,
        None => {
            return Err("output file not specified. Use -o or --output.".to_string());
        }
    };

    let profile = cli
        .profile
        .as_deref()
        .unwrap_or("debug")
        .to_lowercase();
    if profile != "debug" && profile != "release" {
        return Err("profile must be either 'debug' or 'release'.".to_string());
    }

    let code = fs::read_to_string(&input_path).map_err(|e| e.to_string())?;
    let name = Path::new(&input_path)
        .file_stem()    
        .and_then(|s| s.to_str())
        .ok_or_else(|| "failed to derive input file stem".to_string())?;

    let mut lexer = Lexer::new(&*code);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser
        .parse_program(profile, name.to_string())
        .map_err(|e| e.to_string())?;

    let mut r#gen = LLVMTextGen::new();
    let ir = r#gen.generate(&program).map_err(|e| e.to_string())?;

    println!("LLVM Ir:\n{}", ir);

    if output_path.ends_with(".o") || output_path.ends_with(".obj") {
        unsafe {
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmPrinters();
        }
        let options = CompileObjectOptions {
            target: cli.target.clone(),
            cpu: cli.cpu.clone(),
            no_pie: cli.no_pie,
            codegen_level: cli.codegen_level.clone(),
        };
        llvm::compile_object(&options, ir.clone(), output_path.clone(), name.to_string())
            .map_err(|e| e.to_string())?;
    } else {
        fs::write(&output_path, &ir).map_err(|e| e.to_string())?;
    }

    if output_path.ends_with(".o") || output_path.ends_with(".obj") {
        println!("object saved to {}", output_path);
    } else {
        println!("LLVM Ir saved to {}", output_path);
    }

    Ok(())
}
