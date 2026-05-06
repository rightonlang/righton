#[cfg(test)]
mod tests {
    use righton::ast::*;
    use righton::compiler::LLVMTextGen;
    use std::fs;
    use std::path::PathBuf;
    use std::process::Command;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_file(ext: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock moved backwards")
            .as_nanos();
        path.push(format!("righton_test_{}.{}", stamp, ext));
        path
    }

    fn run_bin(args: &[&str]) -> std::process::Output {
        Command::new(env!("CARGO_BIN_EXE_righton"))
            .args(args)
            .output()
            .expect("failed to run binary")
    }

    #[test]
    fn test_float_literal_codegen() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                body: vec![
                    // Test float literal
                    Expr::Let {
                        name: "x".to_string(),
                        typ: Some("f64".to_string()),
                        value: Box::new(Expr::Literal(Literal::Float(3.14))),
                        is_const: false,
                    },
                ],
            }],
            profile: "debug".to_string(),
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("alloca double"));
        assert!(llvm_ir.contains("store double 3.14"));
    }

    #[test]
    fn test_boolean_literal_codegen() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                body: vec![
                    // Test boolean literal
                    Expr::Let {
                        name: "flag".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Literal(Literal::Bool(true))),
                        is_const: false,
                    },
                ],
            }],
            profile: "debug".to_string(),
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("store i32 1"));
    }

    #[test]
    fn test_float_arithmetic_operations() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                body: vec![
                    // Test float arithmetic operations
                    Expr::Let {
                        name: "a".to_string(),
                        typ: Some("f64".to_string()),
                        value: Box::new(Expr::Literal(Literal::Float(2.5))),
                        is_const: false,
                    },
                    Expr::Let {
                        name: "b".to_string(),
                        typ: Some("f64".to_string()),
                        value: Box::new(Expr::Literal(Literal::Float(3.5))),
                        is_const: false,
                    },
                    // Create an expression that uses a and b
                    Expr::Let {
                        name: "c".to_string(),
                        typ: Some("f64".to_string()),
                        value: Box::new(Expr::Binary(
                            Box::new(Expr::Identifier("a".to_string())),
                            BinOp::Add,
                            Box::new(Expr::Identifier("b".to_string())),
                        )),
                        is_const: false,
                    },
                ],
            }],
            profile: "debug".to_string(),
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("fadd double"));
    }

    #[test]
    fn test_integer_arithmetic_operations() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                body: vec![
                    // Test integer arithmetic operations
                    Expr::Let {
                        name: "a".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Literal(Literal::Int(5))),
                        is_const: false,
                    },
                    Expr::Let {
                        name: "b".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Literal(Literal::Int(3))),
                        is_const: false,
                    },
                    // Create an expression that uses a and b
                    Expr::Let {
                        name: "c".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Binary(
                            Box::new(Expr::Identifier("a".to_string())),
                            BinOp::Mul,
                            Box::new(Expr::Identifier("b".to_string())),
                        )),
                        is_const: false,
                    },
                ],
            }],
            profile: "debug".to_string(),
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("mul i32"));
    }

    #[test]
    fn test_comparison_operators() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                body: vec![
                    // Test comparison operations
                    Expr::Let {
                        name: "a".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Literal(Literal::Int(5))),
                        is_const: false,
                    },
                    Expr::Let {
                        name: "b".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Literal(Literal::Int(3))),
                        is_const: false,
                    },
                    // Create a comparison expression
                    Expr::Let {
                        name: "result".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Binary(
                            Box::new(Expr::Identifier("a".to_string())),
                            BinOp::Gt,
                            Box::new(Expr::Identifier("b".to_string())),
                        )),
                        is_const: false,
                    },
                ],
            }],
            profile: "debug".to_string(),
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("icmp sgt i32"));
    }

    #[test]
    fn test_mixed_type_operations() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                body: vec![
                    // Test mixing int and float (should produce float)
                    Expr::Let {
                        name: "a".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Literal(Literal::Int(5))),
                        is_const: false,
                    },
                    Expr::Let {
                        name: "b".to_string(),
                        typ: Some("f64".to_string()),
                        value: Box::new(Expr::Literal(Literal::Float(3.5))),
                        is_const: false,
                    },
                    // This should convert int to float during operation
                    Expr::Let {
                        name: "c".to_string(),
                        typ: Some("f64".to_string()),
                        value: Box::new(Expr::Binary(
                            Box::new(Expr::Identifier("a".to_string())),
                            BinOp::Add,
                            Box::new(Expr::Identifier("b".to_string())),
                        )),
                        is_const: false,
                    },
                ],
            }],
            profile: "debug".to_string(),
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        // Should use floating point operations when mixed with float
        assert!(llvm_ir.contains("sitofp i32"));
        assert!(llvm_ir.contains("fadd double"));
    }

    #[test]
    fn test_cli_invocation_writes_ir() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    return 1").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        assert!(output.exists());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_cli_invalid_syntax_fails_cleanly() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main(\n    return 1").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        assert!(String::from_utf8_lossy(&result.stderr).contains("Error:"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_cli_const_reassignment_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    const x = 1\n    x = 2").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        assert!(String::from_utf8_lossy(&result.stderr).contains("const-variable"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_cli_missing_flags_fail_cleanly() {
        let no_args = run_bin(&[]);
        assert!(!no_args.status.success());
        assert!(String::from_utf8_lossy(&no_args.stderr).contains("input file not specified"));

        let input = temp_file("ron");
        fs::write(&input, "fn main():\n    return 1").unwrap();

        let missing_output = run_bin(&["-i", input.to_str().unwrap()]);
        assert!(!missing_output.status.success());
        assert!(String::from_utf8_lossy(&missing_output.stderr).contains("output file not specified"));

        let _ = fs::remove_file(input);
    }

    #[test]
    fn test_cli_object_file_emission() {
        let input = temp_file("ron");
        let output = temp_file("o");
        fs::write(&input, "fn main():\n    return 1").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        assert!(output.exists());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }
}
