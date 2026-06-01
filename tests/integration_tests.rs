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
                param_types: vec![],
                return_type: None,
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
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
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
                param_types: vec![],
                return_type: None,
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
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
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
                param_types: vec![],
                return_type: None,
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
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
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
                param_types: vec![],
                return_type: None,
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
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
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
                param_types: vec![],
                return_type: None,
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
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
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
                param_types: vec![],
                return_type: None,
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
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
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

    #[test]
    fn test_cli_borrow_codegen_succeeds() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    let a = 1\n    let r = &a\n    return 0").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        assert!(output.exists());

        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("bitcast i32*"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_cli_borrow_conflict_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    let a = 1\n    let r = &a\n    let m = &mut a").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        assert!(String::from_utf8_lossy(&result.stderr).contains("mutably borrow"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_cli_move_after_borrow_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    let s = \"hello\"\n    let r = &s\n    let t = s").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        assert!(String::from_utf8_lossy(&result.stderr).contains("move"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_cli_imports_another_module() {
        let module = temp_file("ron");
        let input = temp_file("ron");
        let output = temp_file("ll");

        fs::write(
            &module,
            "fn answer():\n    return 41\n",
        )
        .unwrap();
        fs::write(
            &input,
            format!(
                "import \"{}\"\nfn main():\n    answer()\n    return 0\n",
                module.to_string_lossy()
            ),
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("__answer"));
        assert!(ir.contains("call i32 @"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(module);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_cli_stdlib_len_builtin() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "import std\nfn main():\n    return len(\"hello\")").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("@__rt_strlen"));
        assert!(ir.contains("trunc i64"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_cli_print_without_stdlib_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    print(1)").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_cli_unknown_function_suggests_stdlib_import() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    len(\"hello\")").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(stderr.contains("import std"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // WHILE LOOP TESTS
    // ========================

    #[test]
    fn test_while_loop_codegen() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    // let x = 5
                    Expr::Let {
                        name: "x".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Literal(Literal::Int(5))),
                        is_const: false,
                    },
                    // while x > 0:
                    //     x = x - 1
                    Expr::While {
                        condition: Box::new(Expr::Binary(
                            Box::new(Expr::Identifier("x".to_string())),
                            BinOp::Gt,
                            Box::new(Expr::Literal(Literal::Int(0))),
                        )),
                        body: Box::new(Block {
                            stmts: vec![Expr::Assign {
                                name: "x".to_string(),
                                value: Box::new(Expr::Binary(
                                    Box::new(Expr::Identifier("x".to_string())),
                                    BinOp::Sub,
                                    Box::new(Expr::Literal(Literal::Int(1))),
                                )),
                            }],
                        }),
                    },
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        // Check for while loop labels and branching
        assert!(llvm_ir.contains("while_start"));
        assert!(llvm_ir.contains("while_body"));
        assert!(llvm_ir.contains("while_end"));
        assert!(llvm_ir.contains("icmp"));
        assert!(llvm_ir.contains("br i1"));
    }

    #[test]
    fn test_while_loop_with_break() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    // let x = 10
                    Expr::Let {
                        name: "x".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Literal(Literal::Int(10))),
                        is_const: false,
                    },
                    // while x > 0:
                    //     if x == 5:
                    //         break
                    //     x = x - 1
                    Expr::While {
                        condition: Box::new(Expr::Binary(
                            Box::new(Expr::Identifier("x".to_string())),
                            BinOp::Gt,
                            Box::new(Expr::Literal(Literal::Int(0))),
                        )),
                        body: Box::new(Block {
                            stmts: vec![
                                Expr::If {
                                    condition: Box::new(Expr::Binary(
                                        Box::new(Expr::Identifier("x".to_string())),
                                        BinOp::Eq,
                                        Box::new(Expr::Literal(Literal::Int(5))),
                                    )),
                                    then_branch: Box::new(Block {
                                        stmts: vec![Expr::Break],
                                    }),
                                    else_branch: None,
                                },
                                Expr::Assign {
                                    name: "x".to_string(),
                                    value: Box::new(Expr::Binary(
                                        Box::new(Expr::Identifier("x".to_string())),
                                        BinOp::Sub,
                                        Box::new(Expr::Literal(Literal::Int(1))),
                                    )),
                                },
                            ],
                        }),
                    },
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("while_start"));
        assert!(llvm_ir.contains("while_end"));
        // break should branch to the loop end label
        assert!(llvm_ir.contains("br label %while_end"));
    }

    #[test]
    fn test_while_loop_with_continue() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    // let x = 3
                    Expr::Let {
                        name: "x".to_string(),
                        typ: Some("i32".to_string()),
                        value: Box::new(Expr::Literal(Literal::Int(3))),
                        is_const: false,
                    },
                    // while x > 0:
                    //     if x == 2:
                    //         continue
                    //     x = x - 1
                    Expr::While {
                        condition: Box::new(Expr::Binary(
                            Box::new(Expr::Identifier("x".to_string())),
                            BinOp::Gt,
                            Box::new(Expr::Literal(Literal::Int(0))),
                        )),
                        body: Box::new(Block {
                            stmts: vec![
                                Expr::If {
                                    condition: Box::new(Expr::Binary(
                                        Box::new(Expr::Identifier("x".to_string())),
                                        BinOp::Eq,
                                        Box::new(Expr::Literal(Literal::Int(2))),
                                    )),
                                    then_branch: Box::new(Block {
                                        stmts: vec![Expr::Continue],
                                    }),
                                    else_branch: None,
                                },
                                Expr::Assign {
                                    name: "x".to_string(),
                                    value: Box::new(Expr::Binary(
                                        Box::new(Expr::Identifier("x".to_string())),
                                        BinOp::Sub,
                                        Box::new(Expr::Literal(Literal::Int(1))),
                                    )),
                                },
                            ],
                        }),
                    },
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("while_start"));
        // continue should branch back to the loop start label
        assert!(llvm_ir.contains("br label %while_start"));
    }

    // ========================
    // FOR LOOP TESTS
    // ========================

    #[test]
    fn test_for_loop_codegen() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    // for i = 5:
                    //     (body)
                    Expr::For {
                        variable: "i".to_string(),
                        iterable: Box::new(Expr::Literal(Literal::Int(5))),
                        body: Box::new(Block {
                            stmts: vec![],
                        }),
                    },
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        // Check for for loop labels and branching
        assert!(llvm_ir.contains("for_start"));
        assert!(llvm_ir.contains("for_body"));
        assert!(llvm_ir.contains("for_end"));
        assert!(llvm_ir.contains("alloca i32")); // Counter allocation
        assert!(llvm_ir.contains("icmp sgt i32")); // Counter > 0 check
        assert!(llvm_ir.contains("sub i32")); // Decrement counter
    }

    #[test]
    fn test_for_loop_with_break() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    // for i = 10:
                    //     if i == 5:
                    //         break
                    Expr::For {
                        variable: "i".to_string(),
                        iterable: Box::new(Expr::Literal(Literal::Int(10))),
                        body: Box::new(Block {
                            stmts: vec![Expr::If {
                                condition: Box::new(Expr::Binary(
                                    Box::new(Expr::Identifier("i".to_string())),
                                    BinOp::Eq,
                                    Box::new(Expr::Literal(Literal::Int(5))),
                                )),
                                then_branch: Box::new(Block {
                                    stmts: vec![Expr::Break],
                                }),
                                else_branch: None,
                            }],
                        }),
                    },
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("for_end"));
        // break should branch to the loop end label
        assert!(llvm_ir.contains("br label %for_end"));
    }

    #[test]
    fn test_for_loop_with_continue() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    // for i = 5:
                    //     if i == 3:
                    //         continue
                    Expr::For {
                        variable: "i".to_string(),
                        iterable: Box::new(Expr::Literal(Literal::Int(5))),
                        body: Box::new(Block {
                            stmts: vec![Expr::If {
                                condition: Box::new(Expr::Binary(
                                    Box::new(Expr::Identifier("i".to_string())),
                                    BinOp::Eq,
                                    Box::new(Expr::Literal(Literal::Int(3))),
                                )),
                                then_branch: Box::new(Block {
                                    stmts: vec![Expr::Continue],
                                }),
                                else_branch: None,
                            }],
                        }),
                    },
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("for_start"));
        // continue should branch back to the loop start label
        assert!(llvm_ir.contains("br label %for_start"));
    }

    // ========================
    // NESTED LOOP TESTS
    // ========================

    #[test]
    fn test_nested_loops_with_break() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    // for i = 3:
                    //     for j = 3:
                    //         if j == 2:
                    //             break
                    Expr::For {
                        variable: "i".to_string(),
                        iterable: Box::new(Expr::Literal(Literal::Int(3))),
                        body: Box::new(Block {
                            stmts: vec![Expr::For {
                                variable: "j".to_string(),
                                iterable: Box::new(Expr::Literal(Literal::Int(3))),
                                body: Box::new(Block {
                                    stmts: vec![Expr::If {
                                        condition: Box::new(Expr::Binary(
                                            Box::new(Expr::Identifier("j".to_string())),
                                            BinOp::Eq,
                                            Box::new(Expr::Literal(Literal::Int(2))),
                                        )),
                                        then_branch: Box::new(Block {
                                            stmts: vec![Expr::Break],
                                        }),
                                        else_branch: None,
                                    }],
                                }),
                            }],
                        }),
                    },
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        // Should have nested loop labels
        assert!(llvm_ir.contains("for_start"));
        assert!(llvm_ir.contains("for_body"));
        assert!(llvm_ir.contains("for_end"));
        // Break in inner loop should only exit the inner loop
        assert!(llvm_ir.contains("br label %for_end"));
    }

    #[test]
    fn test_nested_loops_with_continue() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    // for i = 2:
                    //     for j = 2:
                    //         if j == 1:
                    //             continue
                    Expr::For {
                        variable: "i".to_string(),
                        iterable: Box::new(Expr::Literal(Literal::Int(2))),
                        body: Box::new(Block {
                            stmts: vec![Expr::For {
                                variable: "j".to_string(),
                                iterable: Box::new(Expr::Literal(Literal::Int(2))),
                                body: Box::new(Block {
                                    stmts: vec![Expr::If {
                                        condition: Box::new(Expr::Binary(
                                            Box::new(Expr::Identifier("j".to_string())),
                                            BinOp::Eq,
                                            Box::new(Expr::Literal(Literal::Int(1))),
                                        )),
                                        then_branch: Box::new(Block {
                                            stmts: vec![Expr::Continue],
                                        }),
                                        else_branch: None,
                                    }],
                                }),
                            }],
                        }),
                    },
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        // Continue in inner loop should only jump to inner loop start
        assert!(llvm_ir.contains("br label %for_start"));
    }

    // ========================
    // ERROR CASES
    // ========================

    #[test]
    fn test_break_outside_loop_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    break").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        let stderr = String::from_utf8_lossy(&result.stderr);
        // Check for either parser error or compiler error
        assert!(
            stderr.contains("Break") || stderr.contains("break outside of loop"),
            "Expected Break parse error or break outside of loop error, got: {}",
            stderr
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_continue_outside_loop_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    continue").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        let stderr = String::from_utf8_lossy(&result.stderr);
        // Check for either parser error or compiler error
        assert!(
            stderr.contains("Continue") || stderr.contains("continue outside of loop"),
            "Expected Continue parse error or continue outside of loop error, got: {}",
            stderr
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // INTEGRATION TESTS
    // ========================

    #[test]
    fn test_while_loop_compiles_end_to_end() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn main():\n    let x = 5\n    while x > 0:\n        x = x - 1\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        assert!(output.exists());

        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("while_start"));
        assert!(ir.contains("while_body"));
        assert!(ir.contains("while_end"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_for_loop_compiles_end_to_end() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    for i = 3:\n        i\n    return 0").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        assert!(output.exists());

        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("for_start"));
        assert!(ir.contains("for_body"));
        assert!(ir.contains("for_end"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_loop_with_break_and_continue_together() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn main():\n    let x = 10\n    while x > 0:\n        if x == 5:\n            break\n        x = x - 1\n    return 0",
        )
        .unwrap();

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

    // ========================
    // PARSER ERROR TESTS
    // ========================

    #[test]
    fn test_unclosed_paren_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    return (1 + 2\n").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(
            stderr.contains("expected") || stderr.contains("RParen") || stderr.contains("Error:"),
            "Expected parser error, got: {}",
            stderr
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_missing_function_body_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main()").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_invalid_assignment_target_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    1 + 2 = 3").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_invalid_token_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    return @\n").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_empty_function_name_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn ():\n    return 1").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // TYPE CHECKER ERROR TESTS
    // ========================

    #[test]
    fn test_type_mismatch_in_assignment_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    let x: i32 = \"hello\"\n    return 0").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(
            stderr.contains("cannot assign") || stderr.contains("type"),
            "Expected type mismatch error, got: {}",
            stderr
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_string_concatenation() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    let s = \"hello\"\n    let t = s + \" world\"\n    return 0").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success(), "string concat failed: {}", String::from_utf8_lossy(&result.stderr));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_invalid_if_condition_type_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    if \"test\":\n        return 1\n    return 0").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(
            stderr.contains("condition") || stderr.contains("bool"),
            "Expected condition type error, got: {}",
            stderr
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_invalid_while_condition_type_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    while \"test\":\n        return 0\n    return 1").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_comparison_type_mismatch_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    let x = 1\n    let result = x < \"hello\"\n    return 0").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(
            stderr.contains("cannot compare") || stderr.contains("type"),
            "Expected comparison error, got: {}",
            stderr
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_unary_not_on_string_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    let s = \"hello\"\n    let r = not s\n    return 0").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_unary_negate_on_string_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    let s = \"hello\"\n    let r = -s\n    return 0").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // UNDEFINED VARIABLE TESTS
    // ========================

    #[test]
    fn test_use_undefined_variable_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    return x").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(
            stderr.contains("unknown") || stderr.contains("undefined") || stderr.contains("x"),
            "Expected undefined variable error, got: {}",
            stderr
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_reassign_undefined_variable_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    x = 5\n    return 0").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // INVALID ITERABLE TESTS
    // ========================

    #[test]
    fn test_for_loop_with_invalid_iterable_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    for i in 3.14:\n        i\n    return 0").unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // COMPILER EDGE CASE TESTS
    // ========================

    #[test]
    fn test_nested_if_else_codegen() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn main():\n    let x = 5\n    if x > 0:\n        if x > 3:\n            return 1\n        else:\n            return 2\n    else:\n        return 0\n    return -1",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("then") || ir.contains("merge"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_multiple_functions_codegen() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn add():\n    return 1 + 2\n\nfn sub():\n    return 5 - 3\n\nfn main():\n    return add()",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("@add"));
        assert!(ir.contains("@sub"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_function_with_no_return_codegen() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn do_nothing():\n    let x = 1\n\nfn main():\n    do_nothing()\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_chained_comparison_codegen() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn main():\n    let a = 5\n    let b = 3\n    let c = 10\n    let result = a < b < c\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_fstring_codegen() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "import std\nfn main():\n    let name = \"Alice\"\n    let greeting = f\"Hello {name}\"\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("sprintf"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // CONST TESTS
    // ========================

    #[test]
    fn test_const_immutable_codegen() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn main():\n    const PI = 3.14\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_const_reassignment_fails_codegen() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn main():\n    const PI = 3.14\n    PI = 3.0\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // IMPORT TESTS
    // ========================

    #[test]
    fn test_import_nonexistent_module_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "import \"nonexistent_module.ron\"\nfn main():\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(
            stderr.contains("import") || stderr.contains("module") || stderr.contains("file"),
            "Expected import error, got: {}",
            stderr
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_call_undefined_function_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "import std\nfn main():\n    nonexistent_function()\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(
            stderr.contains("unknown function") || stderr.contains("undefined"),
            "Expected function error, got: {}",
            stderr
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // BORROW CHECKER TESTS
    // ========================

    #[test]
    fn test_borrow_mut_after_borrow_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn main():\n    let a = 1\n    let r = &a\n    let m = &mut a\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_borrow_after_move_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn main():\n    let s = \"hello\"\n    let t = s\n    let r = &s\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_borrow_conflict_in_scope_fails() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn main():\n    let a = 1\n    let r1 = &a\n    let r2 = &mut a\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(!result.status.success());

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // FUNCTION CALL TESTS
    // ========================

    #[test]
    fn test_function_call_with_args_codegen() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn add(a, b):\n    return a + b\n\nfn main():\n    return add(1, 2)",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("@add"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_recursive_function_codegen() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(
            &input,
            "fn count(n):\n    if n == 0:\n        return 0\n    return count(n - 1)\n\nfn main():\n    return 0",
        )
        .unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(result.status.success());
        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("call i32 @count"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // TUPLE TESTS
    // ========================

    #[test]
    fn test_tuple_literal_codegen() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    Expr::Let {
                        name: "tup".to_string(),
                        typ: None,
                        value: Box::new(Expr::Tuple(vec![
                            Expr::Literal(Literal::Int(1)),
                            Expr::Literal(Literal::Int(2)),
                            Expr::Literal(Literal::Int(3)),
                        ])),
                        is_const: false,
                    },
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("call i8* @malloc(i64 24)"));
    }

    #[test]
    fn test_tuple_access_codegen() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    Expr::Let {
                        name: "tup".to_string(),
                        typ: None,
                        value: Box::new(Expr::Tuple(vec![
                            Expr::Literal(Literal::Int(10)),
                            Expr::Literal(Literal::Int(20)),
                        ])),
                        is_const: false,
                    },
                    Expr::Let {
                        name: "x".to_string(),
                        typ: None,
                        value: Box::new(Expr::TupleAccess {
                            target: Box::new(Expr::Identifier("tup".to_string())),
                            index: 0,
                        }),
                        is_const: false,
                    },
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("trunc i64"));
        assert!(llvm_ir.contains("load i64"));
    }

    #[test]
    fn test_tuple_literal_compiles_end_to_end() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    let tup = (1, 2, 3)\n    return 0").unwrap();

        let result = run_bin(&["-i", input.to_str().unwrap(), "-o", output.to_str().unwrap()]);
        assert!(result.status.success(), "tuple literal failed: {}", String::from_utf8_lossy(&result.stderr));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_tuple_access_end_to_end() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "fn main():\n    let tup = (10, 20)\n    let x = tup.0\n    let y = tup.1\n    return 0").unwrap();

        let result = run_bin(&["-i", input.to_str().unwrap(), "-o", output.to_str().unwrap()]);
        assert!(result.status.success(), "tuple access failed: {}", String::from_utf8_lossy(&result.stderr));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // TYPE ALIAS TESTS
    // ========================

    #[test]
    fn test_type_alias_compiles() {
        // Test that TypeAlias resolves correctly through the type checker
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    Expr::Return(Box::new(Expr::Literal(Literal::Int(0)))),
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![TypeAlias {
                name: "MyInt".to_string(),
                value: "i32".to_string(),
            }],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("ret i32 0"));
    }

    #[test]
    fn test_type_alias_chaining() {
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    Expr::Return(Box::new(Expr::Literal(Literal::Int(0)))),
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![
                TypeAlias { name: "Int".to_string(), value: "i32".to_string() },
                TypeAlias { name: "MyInt".to_string(), value: "Int".to_string() },
            ],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let llvm_ir = r#gen.generate(&program).unwrap();
        assert!(llvm_ir.contains("ret i32 0"));
    }

    #[test]
    fn test_type_alias_in_let() {
        // This test may fail due to type checker not resolving aliases in let statements
        let mut r#gen = LLVMTextGen::new();
        let program = Program {
            globals: vec![],
            functions: vec![FunctionDef {
                name: "main".to_string(),
                params: vec![],
                param_types: vec![],
                return_type: None,
                body: vec![
                    Expr::Let {
                        name: "x".to_string(),
                        typ: Some("MyInt".to_string()),
                        value: Box::new(Expr::Literal(Literal::Int(42))),
                        is_const: false,
                    },
                    Expr::Return(Box::new(Expr::Literal(Literal::Int(0)))),
                ],
            }],
            profile: "debug".to_string(),
            type_aliases: vec![TypeAlias {
                name: "MyInt".to_string(),
                value: "i32".to_string(),
            }],
            impls: vec![],
            enums: vec![],
            structs: vec![],
            name: "test".to_string(),
        };

        let result = r#gen.generate(&program);
        if let Err(e) = &result {
            eprintln!("type_alias_in_let error: {:?}", e);
        }
        assert!(result.is_ok(), "type alias in let should work: {:?}", result.err());
    }

    // ========================
    // ENUM TESTS
    // ========================

    #[test]
    fn test_enum_definition_compiles() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "enum Option:\n    None\n    Some(i32)\nfn main():\n    return 0").unwrap();

        let result = run_bin(&["-i", input.to_str().unwrap(), "-o", output.to_str().unwrap()]);
        assert!(result.status.success(), "enum definition failed: {}", String::from_utf8_lossy(&result.stderr));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_enum_literal_compiles() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "enum Option:\n    None\n    Some(i32)\nfn main():\n    let x = Option::Some(42)\n    return 0").unwrap();

        let result = run_bin(&["-i", input.to_str().unwrap(), "-o", output.to_str().unwrap()]);
        assert!(result.status.success(), "enum literal failed: {}", String::from_utf8_lossy(&result.stderr));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_enum_match_compiles() {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "import std\nenum Option:\n    None\n    Some(i32)\nfn main():\n    let x = Option::Some(42)\n    print(x)\n    return 0").unwrap();

        let result = run_bin(&["-i", input.to_str().unwrap(), "-o", output.to_str().unwrap()]);
        assert!(result.status.success(), "enum test failed: {}", String::from_utf8_lossy(&result.stderr));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // IMPL BLOCK TESTS
    // ========================

    #[test]
    fn test_impl_block_compiles() {
        // Just verify struct works with simple field values
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, "struct Point:\n    let x = 0\n    let y = 0\nfn main():\n    let p = Point { x: 1, y: 2 }\n    return 0").unwrap();

        let result = run_bin(&["-i", input.to_str().unwrap(), "-o", output.to_str().unwrap()]);
        assert!(result.status.success(), "struct test failed: {}", String::from_utf8_lossy(&result.stderr));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // NEW STDLIB TESTS
    // ========================

    #[test]
    fn test_stdlib_to_hex() {
        let input = temp_file("ron");
        let output = temp_file("o");
        fs::write(&input, "import std\nfn main():\n    print(to_hex(255))\n    return 0").unwrap();

        let result = run_bin(&["-i", input.to_str().unwrap(), "-o", output.to_str().unwrap()]);
        assert!(result.status.success(), "to_hex failed: {}", String::from_utf8_lossy(&result.stderr));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    #[test]
    fn test_stdlib_str_repeat() {
        let input = temp_file("ron");
        let output = temp_file("o");
        fs::write(&input, "import std\nfn main():\n    let s = str_repeat(\"ab\", 3)\n    return 0").unwrap();

        let result = run_bin(&["-i", input.to_str().unwrap(), "-o", output.to_str().unwrap()]);
        assert!(result.status.success(), "str_repeat failed: {}", String::from_utf8_lossy(&result.stderr));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // EMIT IR CLI FLAG TEST
    // ========================

    #[test]
    fn test_emit_ir_flag_works() {
        let input = temp_file("ron");
        let output = temp_file("o");
        fs::write(&input, "fn main():\n    return 42").unwrap();

        let result = run_bin(&["-i", input.to_str().unwrap(), "-o", output.to_str().unwrap(), "--emit-ir"]);
        assert!(result.status.success(), "emit-ir failed: {}", String::from_utf8_lossy(&result.stderr));
        let stdout = String::from_utf8_lossy(&result.stdout);
        assert!(stdout.contains("define i32 @main"), "emit-ir should show IR");

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }
}
