use clap::Parser;
use llvm_sys::target::{
    LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos, LLVM_InitializeAllTargetMCs,
    LLVM_InitializeAllTargets,
};
use righton::CompileObjectOptions;
use righton::compiler::LLVMTextGen;
use righton::diagnostics::Diagnostic;
use righton::lexer::Lexer;
use righton::llvm;
use righton::parser;
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

    #[clap(long)]
    error_format: Option<String>,
}

fn main() {
    let cli = Cli::parse();
    if let Err(diagnostics) = run() {
        for diag in &diagnostics {
            if cli.error_format.as_deref() == Some("json") {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&diag.to_json())
                        .unwrap_or_else(|_| diag.to_user_message())
                );
            } else {
                eprintln!("{}", diag);
            }
        }
        std::process::exit(1);
    }
}

fn run() -> Result<(), Vec<Diagnostic>> {
    let cli = Cli::parse();

    let input_path = match cli.input {
        Some(ref path) if Path::new(path).exists() => path.clone(),
        Some(path) => {
            return Err(vec![Diagnostic::error(
                Some("E0005"),
                format!("input file '{}' does not exist.", path),
            )]);
        }
        None => {
            return Err(vec![Diagnostic::error(
                Some("E0005"),
                "input file not specified. Use -i or --input.",
            )]);
        }
    };

    let _name = Path::new(&input_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| {
            vec![Diagnostic::error(
                Some("E0005"),
                "failed to derive input file stem",
            )]
        })?;

    let output_path = match cli.output {
        Some(path) => path,
        None => {
            return Err(vec![Diagnostic::error(
                Some("E0005"),
                "output file not specified. Use -o or --output.",
            )]);
        }
    };

    let profile = cli.profile.as_deref().unwrap_or("debug").to_lowercase();
    if profile != "debug" && profile != "release" {
        return Err(vec![Diagnostic::error(
            Some("E0005"),
            "profile must be either 'debug' or 'release'.",
        )]);
    }

    let code = fs::read_to_string(&input_path).map_err(|e| {
        vec![Diagnostic::error(
            Some("E0005"),
            format!("failed to read input file: {}", e),
        )]
    })?;
    let name = Path::new(&input_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| {
            vec![Diagnostic::error(
                Some("E0005"),
                "failed to derive input file stem",
            )]
        })?;

    let mut lexer = Lexer::new(&code);
    let mut parser = parser::Parser::new(&mut lexer);
    let program = match parser.parse_program(profile, name.to_string()) {
        Ok(program) => program,
        Err(errors) => {
            let diags: Vec<Diagnostic> = errors.into_iter().map(|e| e.to_diagnostic()).collect();
            return Err(diags);
        }
    };

    let mut r#gen = LLVMTextGen::new();
    let ir = match r#gen.generate(&program) {
        Ok(ir) => ir,
        Err(e) => return Err(vec![e.to_diagnostic()]),
    };

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
            .map_err(|e| vec![Diagnostic::error(Some("E0004"), e)])?;
        eprintln!("object saved to {}", output_path);
    } else {
        fs::write(&output_path, &ir).map_err(|e| {
            vec![Diagnostic::error(
                Some("E0005"),
                format!("failed to write output: {}", e),
            )]
        })?;
        eprintln!("LLVM IR saved to {}", output_path);
    }

    Ok(())
}
