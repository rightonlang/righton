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

    #[clap(long)]
    emit_ir: bool,
}

fn format_error(source: &str, err: &str, is_parse: bool) -> String {
    if !is_parse {
        return format!("Error: {}", err);
    }
    if let Some(cap) = err.split("(line=").nth(1) {
        if let Some(line_str) = cap.split(", ").next() {
            if let Ok(line_num) = line_str.parse::<usize>() {
                if let Some(col_str) = cap.split("col=").nth(1) {
                    if let Some(col_str) = col_str.split(')').next() {
                        if let Ok(col_num) = col_str.parse::<usize>() {
                            let err_msg = err.split("(line=").next().unwrap_or(err).trim();
                            let source_line = source.lines().nth(line_num - 1).unwrap_or("");
                            let mut result = format!("Error: {} (line={}, col={})\n", err_msg, line_num, col_num);
                            if !source_line.is_empty() {
                                result.push_str(&format!("  {}\n", source_line));
                            let indent = col_num.min(source_line.len());
                            let caret = if indent > 0 {
                                format!("  {:indent$}^---\n", "", indent = indent)
                            } else {
                                format!("  ^---\n")
                            };
                            result.push_str(&caret);
                            }
                            return result;
                        }
                    }
                }
            }
        }
    }
    format!("Error: {}", err)
}

fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
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

    let mut lexer = Lexer::new(&code);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser
        .parse_program(profile, name.to_string())
        .map_err(|e| format_error(&code, &e.to_string(), true))?;

    let mut r#gen = LLVMTextGen::new();
    let ir = r#gen.generate(&program).map_err(|e| format_error(&code, &e.to_string(), false))?;

    if cli.emit_ir {
        println!("{}", ir);
    }

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
        eprintln!("object saved to {}", output_path);
    } else {
        fs::write(&output_path, &ir).map_err(|e| e.to_string())?;
        eprintln!("LLVM IR saved to {}", output_path);
    }

    Ok(())
}
