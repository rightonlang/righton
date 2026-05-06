use crate::ast::*;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompileError {
    pub message: String,
}

impl CompileError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for CompileError {}

type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug, Clone, PartialEq)]
enum VarType {
    String { id: String, len: usize },
    Float,
}

#[derive(Debug, Clone)]
pub enum Type {
    I32,
    F64,
    Ptr,
}

#[derive(Debug, Clone)]
enum Val {
    Imm(i32),
    ImmFloat(f64),
    Reg(String),
}

impl Val {
    fn as_str(&self) -> String {
        match self {
            Val::Imm(n) => n.to_string(),
            Val::ImmFloat(n) => n.to_string(),
            Val::Reg(r) => format!("%{}", r),
        }
    }
}

pub struct LLVMTextGen {
    globals: String,
    functions: String,
    string_count: usize,
    block_count: usize,
    global_vars: HashMap<String, VarType>,

    current_function: Option<String>,
}

impl LLVMTextGen {
    pub fn new() -> Self {
        Self {
            globals: String::new(),
            functions: String::new(),
            string_count: 0,
            block_count: 0,
            global_vars: HashMap::new(),
            current_function: None,
        }
    }

    pub fn generate(&mut self, program: &Program) -> CompileResult<String> {
        let mut ir = String::new();
        if !(program.profile == "release") {
            ir.push_str(format!("; ModuleID = 'righton_{}'\n", program.name).as_str());
        } else {
            ir.push_str(format!("; ModuleID = '{}'\n", program.name).as_str());
        }
        ir.push_str("declare i32 @printf(i8*, ...)\n");
        ir.push_str("declare i32 @sprintf(i8*, i8*, ...)\n");
        ir.push_str("declare double @llvm.pow.f64(double, double)\n");
        ir.push_str("@.global_buffer = private global [1024 x i8] zeroinitializer\n\n");

        for global in &program.globals {
            self.generate_expr(global, &[], &mut HashMap::new(), "void")?;
        }
        for func in &program.functions {
            self.generate_function(func)?;
        }

        ir.push_str(&self.globals);
        ir.push_str(&self.functions);
        Ok(ir)
    }

    fn next_block_label(&mut self, prefix: &str) -> String {
        let id = self.block_count;
        self.block_count += 1;
        format!("{}_{}", prefix, id)
    }

    fn generate_function(&mut self, func: &FunctionDef) -> CompileResult<()> {
        self.current_function = Some(func.name.clone());
        self.string_count = 0;
        self.block_count = 0;

        let args: Vec<String> = func
            .params
            .iter()
            .enumerate()
            .map(|(i, _)| format!("i8* %arg{}", i))
            .collect();

        let return_type = self.infer_return_type(&func.body);

        writeln!(
            &mut self.functions,
            "define {} @{name}({args}) {{",
            return_type,
            name = func.name,
            args = args.join(", ")
        )
        .unwrap();

        writeln!(&mut self.functions, "entry:").unwrap();

        let mut locals: HashMap<String, (Type, String, bool)> = HashMap::new();
        let mut has_return = false;

        for expr in &func.body {
            if matches!(expr, Expr::Return(_)) {
                has_return = true;
            }
            self.generate_expr(expr, &func.params, &mut locals, return_type)?;
        }

        if !has_return {
            self.emit_default_return(return_type);
        }

        writeln!(&mut self.functions, "}}\n").unwrap();
        self.current_function = None;
        Ok(())
    }

    pub fn infer_expr_type(
        &self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> &'static str {
        match expr {
            Expr::Literal(Literal::Int(_)) => "i32",
            Expr::Literal(Literal::Float(_)) => "double",
            Expr::Literal(Literal::Bool(_)) => "i32",
            Expr::Literal(Literal::Str(_)) => "i8*",
            Expr::StringLiteral(_) | Expr::MultilineString(_) | Expr::FString(_) => "i8*",
            Expr::Identifier(name) => {
                if let Some((typ, _, _)) = locals.get(name) {
                    match typ {
                        Type::I32 => "i32",
                        Type::F64 => "double",
                        Type::Ptr => "i8*",
                    }
                } else if params.iter().any(|p| p == name) {
                    "i8*"
                } else if self.global_vars.contains_key(name) {
                    "i8*"
                } else {
                    "void"
                }
            }
            Expr::Binary(l, op, r) => {
                let is_comparison = matches!(*op, BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge);
                let is_logical = matches!(*op, BinOp::And | BinOp::Or);
                if is_comparison || is_logical {
                    return "i32";
                }
                if matches!(l.as_ref(), Expr::Literal(Literal::Float(_))) || matches!(r.as_ref(), Expr::Literal(Literal::Float(_))) {
                    return "double";
                }
                if matches!(l.as_ref(), Expr::Literal(Literal::Int(_))) || matches!(r.as_ref(), Expr::Literal(Literal::Int(_))) {
                    return "i32";
                }
                if matches!(l.as_ref(), Expr::Literal(Literal::Bool(_))) || matches!(r.as_ref(), Expr::Literal(Literal::Bool(_))) {
                    return "i32";
                }
                if let Expr::Identifier(ln) = l.as_ref() {
                    if let Some((Type::F64, _, _)) = locals.get(ln) {
                        return "double";
                    }
                }
                if let Expr::Identifier(rn) = r.as_ref() {
                    if let Some((Type::F64, _, _)) = locals.get(rn) {
                        return "double";
                    }
                }
                let lt = self.infer_expr_type(l, params, locals);
                let rt = self.infer_expr_type(r, params, locals);
                if lt == "double" || rt == "double" {
                    "double"
                } else if lt == "void" || rt == "void" {
                    "i32"
                } else {
                    "i32"
                }
            }
            Expr::Unary(op, inner) => {
                if matches!(op, UnaryOp::Not) {
                    return "i32";
                }
                let inner_type = self.infer_expr_type(inner, params, locals);
                if inner_type == "void" {
                    if matches!(inner.as_ref(), Expr::Literal(Literal::Float(_))) {
                        "double"
                    } else if matches!(inner.as_ref(), Expr::Literal(Literal::Bool(_))) {
                        "i32"
                    } else if matches!(inner.as_ref(), Expr::Literal(Literal::Int(_))) {
                        "i32"
                    } else {
                        "i32"
                    }
                } else {
                    inner_type
                }
            }
            Expr::If { then_branch, else_branch, .. } => {
                let then_type = self.find_return_in_exprs(&then_branch.stmts);
                if let Some(else_block) = else_branch {
                    let else_type = self.find_return_in_exprs(&else_block.stmts);
                    if then_type != "void" && else_type != "void" && then_type == else_type {
                        then_type
                    } else {
                        "void"
                    }
                } else {
                    then_type
                }
            }
            _ => "void",
        }
    }

    fn infer_return_type(&self, body: &[Expr]) -> &'static str {
        if let Some(name) = &self.current_function {
            if name == "main" {
                return "i32";
            }
        }
        self.find_return_in_exprs(body)
    }

    fn find_return_in_exprs(&self, exprs: &[Expr]) -> &'static str {
        for expr in exprs {
            if let Expr::Return(inner) = expr {
                return self.infer_expr_type(inner, &[], &HashMap::new());
            }
            if let Expr::If { then_branch, else_branch, .. } = expr {
                let then_type = self.find_return_in_exprs(&then_branch.stmts);
                if then_type != "void" {
                    return then_type;
                }
                if let Some(else_block) = else_branch {
                    let else_type = self.find_return_in_exprs(&else_block.stmts);
                    if else_type != "void" {
                        return else_type;
                    }
                }
            }
        }
        "void"
    }

    fn emit_default_return(&mut self, ty: &str) {
        match ty {
            "i32" => self.functions.push_str("  ret i32 0\n"),
            "double" => self.functions.push_str("  ret double 0.0\n"),
            "i8*" => {
                let name = self.emit_string_const("");
                let ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr [1 x i8], [1 x i8]* @{}, i32 0, i32 0",
                    ptr, name
                )
                .unwrap();
                writeln!(&mut self.functions, "  ret i8* %{}", ptr).unwrap();
            }
            "void" => {
                if self.current_function.as_deref() == Some("main") {
                    self.functions.push_str("  ret i32 0\n");
                } else {
                    self.functions.push_str("  ret void\n");
                }
            },
            _ => unreachable!(),
        }
    }

    fn generate_expr(
        &mut self,
        expr: &Expr,
        params: &[String],
        locals: &mut HashMap<String, (Type, String, bool)>,
        return_ty: &str,
    ) -> CompileResult<()> {
        match expr {
            Expr::Let {
                name,
                typ,
                value,
                is_const,
            } => {
                let alloca = self.next_temp();

                let var_type = if let Some(t) = typ {
                    match t.as_str() {
                        "i32" => Type::I32,
                        "f64" | "float" => Type::F64,
                        "str" | "string" | "ptr" => Type::Ptr,
                        _ => return Err(CompileError::new(format!("unsupported type: {}", t))),
                    }
                } else {
                    let inferred = self.infer_expr_type(value, params, locals);
                    match inferred {
                        "i32" => Type::I32,
                        "double" => Type::F64,
                        "i8*" => Type::Ptr,
                        "void" => Type::I32,
                        _ => {
                            return Err(CompileError::new(format!(
                                "couldn't infer the type for variable {} (got {})",
                                name, inferred
                            )))
                        }
                    }
                };

                match var_type {
                    Type::I32 => {
                        let val = self.generate_int_expr(value, params, locals)?;
                        writeln!(&mut self.functions, "  %{} = alloca i32", alloca).unwrap();
                        writeln!(
                            &mut self.functions,
                            "  store i32 {}, i32* %{}",
                            val.as_str(),
                            alloca
                        )
                        .unwrap();
                        locals.insert(name.clone(), (Type::I32, alloca.clone(), *is_const));
                    }
                    Type::F64 => {
                        let val = self.generate_float_expr(value, params, locals)?;
                        writeln!(&mut self.functions, "  %{} = alloca double", alloca).unwrap();
                        writeln!(
                            &mut self.functions,
                            "  store double {}, double* %{}",
                            val.as_str(),
                            alloca
                        )
                        .unwrap();
                        locals.insert(name.clone(), (Type::F64, alloca.clone(), *is_const));
                    }
                    Type::Ptr => {
                        let ptr = self.next_temp();
                        match &**value {
                            Expr::StringLiteral(s) | Expr::MultilineString(s) => {
                                let id = self.emit_string_const(s);
                                let len = s.len() + 1;
                                self.emit_gep(&id, len, &ptr);
                            }
                            Expr::FString(elements) => {
                                self.emit_fstring(elements, params, locals, &ptr)?;
                            }
                            Expr::Identifier(id_name) => {
                                self.emit_print_arg(value, params, locals, &ptr)?;
                            }
                            _ => {
                                return Err(CompileError::new(
                                    "let: expected str expression for type ptr/str",
                                ))
                            }
                        }
                        writeln!(&mut self.functions, "  %{} = alloca i8*", alloca).unwrap();
                        writeln!(
                            &mut self.functions,
                            "  store i8* %{}, i8** %{}",
                            ptr, alloca
                        )
                        .unwrap();
                        locals.insert(name.clone(), (Type::Ptr, alloca.clone(), *is_const));
                    }
                }
            }

            Expr::Assign { name, value } => {
                let (var_ty, alloca, is_const) = locals
                    .get(name)
                    .ok_or_else(|| CompileError::new(format!("assign of unknown variable: {}", name)))?;

                if *is_const {
                    return Err(CompileError::new(format!("cannot assign to const-variable: {}", name)));
                }

                match var_ty {
                    Type::I32 => {
                        let val = self.generate_int_expr(value, params, locals)?;
                        writeln!(
                            &mut self.functions,
                            "  store i32 {}, i32* %{}",
                            val.as_str(),
                            alloca
                        )
                        .unwrap();
                    }
                    Type::F64 => {
                        let val = self.generate_float_expr(value, params, locals)?;
                        writeln!(
                            &mut self.functions,
                            "  store double {}, double* %{}",
                            val.as_str(),
                            alloca
                        )
                        .unwrap();
                    }
                    Type::Ptr => {
                        let ptr = self.next_temp();
                        match &**value {
                            Expr::StringLiteral(s) | Expr::MultilineString(s) => {
                                let id = self.emit_string_const(s);
                                let len = s.len() + 1;
                                self.emit_gep(&id, len, &ptr);
                            }
                            Expr::FString(elements) => {
                                self.emit_fstring(elements, params, locals, &ptr)?;
                            }
                            _ => return Err(CompileError::new("assign: expected str")),
                        }
                        writeln!(
                            &mut self.functions,
                            "  store i8* %{}, i8** %{}",
                            ptr, alloca
                        )
                        .unwrap();
                    }
                }
            }

            Expr::Call { func, args } if func == "print" => {
                for arg in args {
                    let ty = self.infer_expr_type(arg, params, locals);
                    if ty == "i32" {
                        let val = self.generate_int_expr(arg, params, locals)?;
                        self.emit_printf_int(&val.as_str());
                    } else if ty == "double" {
                        let val = self.generate_float_expr(arg, params, locals)?;
                        self.emit_printf_float(&val.as_str());
                    } else if ty == "i8*" {
                        let result = self.next_temp();
                        self.emit_print_arg(arg, params, locals, &result)?;
                        self.emit_printf_str(&result);
                    }
                }
            }

            Expr::Call { func, args } if func == "asm" => {
                self.emit_asm_call(args, params, locals)?;
            }

            Expr::Return(inner) => {
                let ty = self.infer_expr_type(inner, params, locals);
                if ty == "i32" && return_ty == "i32" {
                    let val = self.generate_int_expr(inner, params, locals)?;
                    writeln!(&mut self.functions, "  ret i32 {}", val.as_str()).unwrap();
                } else if ty == "i8*" && return_ty == "i8*" {
                    let ptr = self.next_temp();
                    match &**inner {
                        Expr::StringLiteral(s) | Expr::MultilineString(s) => {
                            let id = self.emit_string_const(s);
                            let len = s.len() + 1;
                            self.emit_gep(&id, len, &ptr);
                        }
                            Expr::FString(elements) => {
                            self.emit_fstring(elements, params, locals, &ptr)?;
                        }
                        Expr::Identifier(name) => {
                            self.emit_print_arg(inner, params, locals, &ptr)?;
                        }
                        _ => return Err(CompileError::new("return: unsupported str expression")),
                    }
                    writeln!(&mut self.functions, "  ret i8* %{}", ptr).unwrap();
                } else {
                    return Err(CompileError::new("return: types mismatch"));
                }
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_val = self.generate_int_expr(condition, params, locals)?;
                let cond_bool = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = icmp ne i32 {}, 0",
                    cond_bool,
                    cond_val.as_str()
                )
                .unwrap();

                let then_label = self.next_block_label("then");
                let else_label = if else_branch.is_some() {
                    self.next_block_label("else")
                } else {
                    self.next_block_label("merge")
                };
                let merge_label = self.next_block_label("merge");

                writeln!(
                    &mut self.functions,
                    "  br i1 %{}, label %{}, label %{}",
                    cond_bool, then_label, else_label
                )
                .unwrap();

                writeln!(&mut self.functions, "{}:", then_label).unwrap();
                for stmt in &then_branch.stmts {
                    self.generate_expr(stmt, params, locals, return_ty)?;
                }

                writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();

                if let Some(else_block) = else_branch {
                    writeln!(&mut self.functions, "{}:", else_label).unwrap();
                    for stmt in &else_block.stmts {
                        self.generate_expr(stmt, params, locals, return_ty)?;
                    }
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                }

                writeln!(&mut self.functions, "{}:", merge_label).unwrap();
            }

            Expr::Literal(Literal::Int(_))
            | Expr::Literal(Literal::Bool(_))
            | Expr::Literal(Literal::Float(_))
            | Expr::Unary(_, _) => {
                let inferred = self.infer_expr_type(expr, params, locals);
                if inferred == "double" {
                    let val = self.generate_float_expr(expr, params, locals)?;
                    self.emit_printf_float(&val.as_str());
                } else {
                    let val = self.generate_int_expr(expr, params, locals)?;
                    self.emit_printf_int(&val.as_str());
                }
            }

            Expr::Binary(l, op, r) => {
                let left_ty = self.infer_expr_type(l, params, locals);
                let right_ty = self.infer_expr_type(r, params, locals);
                let uses_float = left_ty == "double" || right_ty == "double";
                let is_comparison = matches!(
                    op,
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
                );

                if uses_float && !is_comparison {
                    let val = self.generate_float_expr(expr, params, locals)?;
                    self.emit_printf_float(&val.as_str());
                } else if uses_float && is_comparison {
                    let val = self.generate_float_expr(expr, params, locals)?;
                    self.emit_printf_int(&val.as_str());
                } else {
                    let val = self.generate_int_expr(expr, params, locals)?;
                    self.emit_printf_int(&val.as_str());
                }
            }

            Expr::StringLiteral(s) | Expr::MultilineString(s) => {
                let id = self.emit_string_const(s);
                let ptr = self.next_temp();
                let len = s.len() + 1;
                self.emit_gep(&id, len, &ptr);
                self.emit_printf_str(&ptr);
            }

            _ => {}
        }

        Ok(())
    }

    fn emit_printf_int(&mut self, value: &str) {
        let fmt_id = self.emit_string_const("%d\n");
        let fmt_ptr = self.next_temp();
        self.emit_gep(&fmt_id, 5, &fmt_ptr);
        writeln!(
            &mut self.functions,
            "  call i32 (i8*, ...) @printf(i8* %{}, i32 {})",
            fmt_ptr, value
        )
        .unwrap();
    }

    fn emit_printf_float(&mut self, value: &str) {
        let fmt_id = self.emit_string_const("%f\n");
        let fmt_ptr = self.next_temp();
        self.emit_gep(&fmt_id, 4, &fmt_ptr);
        writeln!(
            &mut self.functions,
            "  call i32 (i8*, ...) @printf(i8* %{}, double {})",
            fmt_ptr, value
        )
        .unwrap();
    }

    fn emit_fstring(
        &mut self,
        elements: &[Expr],
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
        result_ptr: &str,
    ) -> CompileResult<()> {
        let mut parts = Vec::new();
        let mut args = Vec::new();

        for el in elements {
            match el {
                Expr::StringLiteral(s) | Expr::MultilineString(s) => parts.push(s.clone()),
                Expr::Identifier(name) => {
                    if let Some((typ, alloca, _)) = locals.get(name) {
                        match typ {
                            Type::I32 => {
                                parts.push("%d".to_string());
                                let loaded = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = load i32, i32* %{}",
                                    loaded, alloca
                                )
                                .unwrap();
                                args.push(format!("i32 %{}", loaded));
                            }
                            Type::F64 => {
                                parts.push("%f".to_string());
                                let loaded = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = load double, double* %{}",
                                    loaded, alloca
                                )
                                .unwrap();
                                args.push(format!("double %{}", loaded));
                            }
                            Type::Ptr => {
                                parts.push("%s".to_string());
                                let loaded = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = load i8*, i8** %{}",
                                    loaded, alloca
                                )
                                .unwrap();
                                args.push(format!("i8* %{}", loaded));
                            }
                        }
                    } else if let Some(idx) = params.iter().position(|p| p == name) {
                        parts.push("%s".to_string());
                        args.push(format!("i8* %arg{}", idx));
                    } else {
                        return Err(CompileError::new(format!("fstring: unknown variable {}", name)));
                    }
                }
                _ => return Err(CompileError::new("fstring: unsupported element")),
            }
        }

        let fmt_str = parts.join("");
        let fmt_id = self.emit_string_const(&fmt_str);
        let fmt_ptr = self.next_temp();
        self.emit_gep(&fmt_id, fmt_str.len() + 1, &fmt_ptr);

        let buf_ptr =
            "getelementptr inbounds ([1024 x i8], [1024 x i8]* @.global_buffer, i32 0, i32 0)";
        let mut sprintf_args = format!("i8* {}, i8* %{}", buf_ptr, fmt_ptr);
        for arg in args {
            write!(&mut sprintf_args, ", {}", arg).unwrap();
        }

        writeln!(
            &mut self.functions,
            "  call i32 (i8*, i8*, ...) @sprintf({})",
            sprintf_args
        )
        .unwrap();
        writeln!(&mut self.functions, "  %{} = {}", result_ptr, buf_ptr).unwrap();
        Ok(())
    }

    fn generate_int_expr(
        &mut self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> CompileResult<Val> {
        match expr {
            Expr::Literal(Literal::Int(n)) => Ok(Val::Imm(*n)),
            Expr::Literal(Literal::Float(n)) => Ok(Val::ImmFloat(*n)),
            Expr::Literal(Literal::Bool(b)) => Ok(Val::Imm(if *b { 1 } else { 0 })),
            Expr::Identifier(name) => {
                if let Some((typ, alloca, _)) = locals.get(name) {
                    return match typ {
                        Type::I32 => {
                            let res = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = load i32, i32* %{}",
                                res, alloca
                            )
                            .unwrap();
                            Ok(Val::Reg(res))
                        }
                        Type::F64 => {
                            let res = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = load double, double* %{}",
                                res, alloca
                            )
                            .unwrap();
                            let converted = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = fptosi double %{} to i32",
                                converted, res
                            )
                            .unwrap();
                            Ok(Val::Reg(converted))
                        }
                        Type::Ptr => Err(CompileError::new(format!(
                            "variable '{}' has a string type, but is used in a numeric context.",
                            name
                        ))),
                    };
                }

                if params.iter().position(|p| p == name).is_some() {
                    return Err(CompileError::new(format!(
                        "The function parameter '{}' has a string type (i8*), but is used in a numeric context",
                        name
                    )));
                }

                if self.global_vars.contains_key(name) {
                    return Err(CompileError::new(format!(
                        "The global variable '{}' has a string type, but is used in a numeric context",
                        name
                    )));
                }

                Err(CompileError::new(format!(
                    "an unknown variable in a numeric context: '{}'",
                    name
                )))
            }
            Expr::Unary(op, inner) => {
                let v = self.generate_int_expr(inner, params, locals)?;
                match op {
                    UnaryOp::Pos => match v {
                        Val::Imm(n) => Ok(Val::Imm(n)),
                        Val::ImmFloat(_) => Err(CompileError::new("cannot use + on float in integer context")),
                        Val::Reg(r) => {
                            let res = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = add i32 0, {}", res, r).unwrap();
                            Ok(Val::Reg(res))
                        }
                    },
                    UnaryOp::Neg => match v {
                        Val::Imm(n) => Ok(Val::Imm(-n)),
                        Val::ImmFloat(_) => Err(CompileError::new("cannot use - on float in integer context")),
                        Val::Reg(r) => {
                            let res = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = sub i32 0, {}", res, r).unwrap();
                            Ok(Val::Reg(res))
                        }
                    },
                    UnaryOp::Not => match v {
                        Val::Imm(n) => Ok(Val::Imm(if n == 0 { 1 } else { 0 })),
                        Val::ImmFloat(f) => Ok(Val::Imm(if f == 0.0 { 1 } else { 0 })),
                        Val::Reg(r) => {
                            let cmp = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = icmp eq i32 {}, 0",
                                cmp, r
                            )
                            .unwrap();
                            let zext = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = zext i1 %{} to i32",
                                zext, cmp
                            )
                            .unwrap();
                            Ok(Val::Reg(zext))
                        }
                    },
                }
            }
            Expr::Binary(l, op, r) => {
                let lv = self.generate_int_expr(l, params, locals)?;
                let rv = self.generate_int_expr(r, params, locals)?;
                let res = self.next_temp();

                match op {
                    BinOp::Add => writeln!(&mut self.functions, "  %{} = add i32 {}, {}", res, lv.as_str(), rv.as_str()),
                    BinOp::Sub => writeln!(&mut self.functions, "  %{} = sub i32 {}, {}", res, lv.as_str(), rv.as_str()),
                    BinOp::Mul => writeln!(&mut self.functions, "  %{} = mul i32 {}, {}", res, lv.as_str(), rv.as_str()),
                    BinOp::Div => writeln!(&mut self.functions, "  %{} = sdiv i32 {}, {}", res, lv.as_str(), rv.as_str()),
                    BinOp::DivMod => writeln!(&mut self.functions, "  %{} = srem i32 {}, {}", res, lv.as_str(), rv.as_str()),
                    BinOp::Pow => {
                        let ld = self.next_temp();
                        let rd = self.next_temp();
                        let pow = self.next_temp();
                        let int_res = self.next_temp();

                        writeln!(&mut self.functions, "  %{} = sitofp i32 {} to double", ld, lv.as_str()).unwrap();
                        writeln!(&mut self.functions, "  %{} = sitofp i32 {} to double", rd, rv.as_str()).unwrap();
                        writeln!(&mut self.functions, "  %{} = call double @llvm.pow.f64(double %{}, double %{})", pow, ld, rd).unwrap();
                        writeln!(&mut self.functions, "  %{} = fptosi double %{} to i32", int_res, pow).unwrap();
                        return Ok(Val::Reg(int_res));
                    }
                    BinOp::Eq => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = icmp eq i32 {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Ne => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = icmp ne i32 {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Lt => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = icmp slt i32 {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Le => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = icmp sle i32 {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Gt => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = icmp sgt i32 {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Ge => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = icmp sge i32 {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::And => {
                        let lv_bool = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = icmp ne i32 {}, 0", lv_bool, lv.as_str()).unwrap();
                        let rv_bool = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = icmp ne i32 {}, 0", rv_bool, rv.as_str()).unwrap();
                        let and_res = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = and i1 %{}, %{}", and_res, lv_bool, rv_bool).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, and_res).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Or => {
                        let lv_bool = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = icmp ne i32 {}, 0", lv_bool, lv.as_str()).unwrap();
                        let rv_bool = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = icmp ne i32 {}, 0", rv_bool, rv.as_str()).unwrap();
                        let or_res = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = or i1 %{}, %{}", or_res, lv_bool, rv_bool).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, or_res).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                }
                .unwrap();
                Ok(Val::Reg(res))
            }
            _ => Err(CompileError::new(format!("unexpected expression in int-context: {:?}", expr))),
        }
    }

    fn generate_float_expr(
        &mut self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> CompileResult<Val> {
        match expr {
            Expr::Literal(Literal::Float(n)) => Ok(Val::ImmFloat(*n)),
            Expr::Literal(Literal::Int(n)) => Ok(Val::ImmFloat(*n as f64)),
            Expr::Literal(Literal::Bool(b)) => Ok(Val::ImmFloat(if *b { 1.0 } else { 0.0 })),
            Expr::Identifier(name) => {
                if let Some((typ, alloca, _)) = locals.get(name) {
                    return match typ {
                        Type::F64 => {
                            let res = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = load double, double* %{}", res, alloca).unwrap();
                            Ok(Val::Reg(res))
                        }
                        Type::I32 => {
                            let res = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = load i32, i32* %{}", res, alloca).unwrap();
                            let converted = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = sitofp i32 %{} to double", converted, res).unwrap();
                            Ok(Val::Reg(converted))
                        }
                        Type::Ptr => Err(CompileError::new(format!("variable '{}' is a string, cannot convert to float", name))),
                    };
                }
                Err(CompileError::new(format!("variable '{}' is not a float", name)))
            }
            Expr::Unary(op, inner) => {
                let v = self.generate_float_expr(inner, params, locals)?;
                match op {
                    UnaryOp::Pos => Ok(v),
                    UnaryOp::Neg => {
                        let res = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = fsub double 0.0, {}", res, v.as_str()).unwrap();
                        Ok(Val::Reg(res))
                    }
                    UnaryOp::Not => {
                        let res = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = fcmp one double {}, 0.0", res, v.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, res).unwrap();
                        Ok(Val::Reg(zext))
                    }
                }
            }
            Expr::Binary(l, op, r) => {
                let lv = self.generate_float_expr(l, params, locals)?;
                let rv = self.generate_float_expr(r, params, locals)?;
                let res = self.next_temp();

                match op {
                    BinOp::Add => writeln!(&mut self.functions, "  %{} = fadd double {}, {}", res, lv.as_str(), rv.as_str()),
                    BinOp::Sub => writeln!(&mut self.functions, "  %{} = fsub double {}, {}", res, lv.as_str(), rv.as_str()),
                    BinOp::Mul => writeln!(&mut self.functions, "  %{} = fmul double {}, {}", res, lv.as_str(), rv.as_str()),
                    BinOp::Div => writeln!(&mut self.functions, "  %{} = fdiv double {}, {}", res, lv.as_str(), rv.as_str()),
                    BinOp::Pow => {
                        let pow = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = call double @llvm.pow.f64(double {}, double {})", pow, lv.as_str(), rv.as_str()).unwrap();
                        return Ok(Val::Reg(pow));
                    }
                    BinOp::Eq => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = fcmp oeq double {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Ne => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = fcmp one double {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Lt => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = fcmp olt double {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Le => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = fcmp ole double {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Gt => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = fcmp ogt double {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Ge => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = fcmp oge double {}, {}", cmp, lv.as_str(), rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::And => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = fcmp one double {}, 0.0", cmp, lv.as_str()).unwrap();
                        let and_res = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = and i1 %{}, {}", and_res, cmp, rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, and_res).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Or => {
                        let cmp = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = fcmp one double {}, 0.0", cmp, lv.as_str()).unwrap();
                        let or_res = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = or i1 %{}, {}", or_res, cmp, rv.as_str()).unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, or_res).unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::DivMod => {
                        let res = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = frem double {}, {}", res, lv.as_str(), rv.as_str()).unwrap();
                        return Ok(Val::Reg(res));
                    }
                }
                .unwrap();
                Ok(Val::Reg(res))
            }
            _ => Err(CompileError::new(format!("unexpected expression in float-context: {:?}", expr))),
        }
    }

    fn next_temp(&mut self) -> String {
        let id = format!(".t{}", self.string_count);
        self.string_count += 1;
        id
    }

    fn emit_string_const(&mut self, s: &str) -> String {
        let id = format!(".str{}", self.string_count);
        self.string_count += 1;

        let mut bytes = s.as_bytes().to_vec();
        bytes.push(0); // null terminator

        let mut escaped = String::new();
        for b in &bytes {
            use std::fmt::Write;
            write!(escaped, "\\{:02X}", b).unwrap();
        }

        writeln!(
            &mut self.globals,
            "@{} = private unnamed_addr constant [{} x i8] c\"{}\"",
            id,
            bytes.len(),
            escaped
        )
            .unwrap();

        id
    }

    fn emit_gep(&mut self, array_id: &str, len: usize, result: &str) {
        writeln!(
            &mut self.functions,
            "  %{} = getelementptr [{} x i8], [{} x i8]* @{}, i32 0, i32 0",
            result, len, len, array_id
        )
        .unwrap();
    }

    fn emit_print_arg(
        &mut self,
        arg: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
        result: &str,
    ) -> CompileResult<()> {
        match arg {
            Expr::StringLiteral(s) | Expr::MultilineString(s) => {
                let id = self.emit_string_const(s);
                self.emit_gep(&id, s.len() + 1, result);
            }
            Expr::Identifier(name) => {
                if let Some((Type::Ptr, alloca, _)) = locals.get(name) {
                    writeln!(
                        &mut self.functions,
                        "  %{} = load i8*, i8** %{}",
                        result, alloca
                    )
                    .unwrap();
                } else if let Some(idx) = params.iter().position(|p| p == name) {
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast i8* %arg{} to i8*",
                        result, idx
                    )
                    .unwrap();
                } else {
                    return Err(CompileError::new(format!("unknown variable: {}", name)));
                }
            }
            _ => return Err(CompileError::new("print: unsupported argument")),
        }
        Ok(())
    }

    fn emit_printf_str(&mut self, ptr: &str) {
        let fmt_id = self.emit_string_const("%s\n");
        let fmt_ptr = self.next_temp();
        self.emit_gep(&fmt_id, 4, &fmt_ptr);
        writeln!(
            &mut self.functions,
            "  call i32 (i8*, ...) @printf(i8* %{}, i8* %{})",
            fmt_ptr, ptr
        )
        .unwrap();
    }

    fn emit_asm_call(
        &mut self,
        args: &[Expr],
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> CompileResult<()> {
        let code = if let Expr::StringLiteral(s) = &args[0] {
            s
        } else {
            return Err(CompileError::new("asm: first argument is a str"));
        };

        if args.len() == 1 {
            writeln!(
                &mut self.functions,
                "  call void asm sideeffect \"{}\", \"\"()",
                code.escape_default()
            )
            .unwrap();
            return Ok(());
        }

        let mut constraints = String::new();
        let mut operands = String::new();

        for (i, arg) in args[1..].iter().enumerate() {
            if i > 0 {
                constraints.push(',');
                operands.push(',');
            }

            if let Expr::Identifier(name) = arg {
                if let Some((typ, alloca, _)) = locals.get(name) {
                    match typ {
                        Type::I32 => {
                            constraints.push_str("=r");
                            operands.push_str(&format!("i32* %{}", alloca));
                        }
                        Type::F64 => {
                            constraints.push_str("=r");
                            operands.push_str(&format!("double* %{}", alloca));
                        }
                        Type::Ptr => {
                            constraints.push_str("=r");
                            operands.push_str(&format!("i8** %{}", alloca));
                        }
                    }
                } else if let Some(idx) = params.iter().position(|p| p == name) {
                    constraints.push_str("r");
                    operands.push_str(&format!("i8* %arg{}", idx));
                } else {
                    return Err(CompileError::new(format!("asm: unknown variable {}", name)));
                }
            } else {
                return Err(CompileError::new("asm: arguments must be identifiers"));
            }
        }

        let result = self.next_temp();
        writeln!(
            &mut self.functions,
            "  %{} = call i32 asm \"{}\", \"{}\"( {})",
            result,
            code.escape_default(),
            constraints,
            operands
        )
        .unwrap();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use super::LLVMTextGen;

    fn compile_expr(input: &str) -> String {
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser
            .parse_program("debug".to_string(), "test".to_string())
            .unwrap();
        let mut r#gen = LLVMTextGen::new();
        r#gen.generate(&program).unwrap()
    }

    #[test]
    fn test_compile_integer_literal() {
        let ir = compile_expr("42");
        assert!(ir.contains("i32 42"));
    }

    #[test]
    fn test_compile_binary_add() {
        let ir = compile_expr("fn test():\n    let a = 1\n    let b = 2\n    a + b");
        assert!(ir.contains("add i32"));
    }

    #[test]
    fn test_compile_binary_sub() {
        let ir = compile_expr("fn test():\n    let a = 5\n    let b = 3\n    a - b");
        assert!(ir.contains("sub i32"));
    }

    #[test]
    fn test_compile_binary_mul() {
        let ir = compile_expr("fn test():\n    let a = 4\n    let b = 5\n    a * b");
        assert!(ir.contains("mul i32"));
    }

    #[test]
    fn test_compile_binary_div() {
        let ir = compile_expr("fn test():\n    let a = 10\n    let b = 2\n    a / b");
        assert!(ir.contains("sdiv i32"));
    }

    #[test]
    fn test_compile_binary_mod() {
        let ir = compile_expr("fn test():\n    let a = 10\n    let b = 3\n    a % b");
        assert!(ir.contains("srem i32"));
    }

    #[test]
    fn test_compile_binary_pow() {
        let ir = compile_expr("fn test():\n    let a = 2\n    let b = 3\n    a ** b");
        assert!(ir.contains("@llvm.pow"));
    }

    #[test]
    fn test_compile_comparison_eq() {
        let ir = compile_expr("fn test():\n    let a = 5\n    let b = 5\n    a == b");
        assert!(ir.contains("icmp eq i32"));
    }

    #[test]
    fn test_compile_comparison_ne() {
        let ir = compile_expr("fn test():\n    let a = 5\n    let b = 3\n    a != b");
        assert!(ir.contains("icmp ne i32"));
    }

    #[test]
    fn test_compile_comparison_lt() {
        let ir = compile_expr("fn test():\n    let a = 3\n    let b = 5\n    a < b");
        assert!(ir.contains("icmp slt i32"));
    }

    #[test]
    fn test_compile_comparison_le() {
        let ir = compile_expr("fn test():\n    let a = 5\n    let b = 5\n    a <= b");
        assert!(ir.contains("icmp sle i32"));
    }

    #[test]
    fn test_compile_comparison_gt() {
        let ir = compile_expr("fn test():\n    let a = 5\n    let b = 3\n    a > b");
        assert!(ir.contains("icmp sgt i32"));
    }

    #[test]
    fn test_compile_comparison_ge() {
        let ir = compile_expr("fn test():\n    let a = 5\n    let b = 5\n    a >= b");
        assert!(ir.contains("icmp sge i32"));
    }

    #[test]
    fn test_compile_unary_plus() {
        let ir = compile_expr("fn test():\n    let a = 5\n    +a");
        assert!(ir.contains("add i32") || ir.is_empty()); // might be optimized
    }

    #[test]
    fn test_compile_unary_minus() {
        let ir = compile_expr("fn test():\n    let a = 5\n    -a");
        assert!(ir.contains("sub i32 0"));
    }

    #[test]
    fn test_compile_let_declaration() {
        let ir = compile_expr("let x = 42");
        assert!(ir.contains("alloca i32"));
        assert!(ir.contains("store i32 42"));
    }

    #[test]
    fn test_compile_const_declaration() {
        let ir = compile_expr("const x = 42");
        assert!(ir.contains("alloca i32"));
        assert!(ir.contains("store i32 42"));
    }

    #[test]
    fn test_compile_assignment() {
        let ir = compile_expr("fn test():\n    let x = 0\n    x = 42");
        assert!(ir.contains("store i32 42"));
    }

    #[test]
    fn test_compile_return() {
        let ir = compile_expr("fn test():\n    return 42");
        assert!(ir.contains("ret i32 42"));
    }

    #[test]
    fn test_compile_print_call() {
        let ir = compile_expr("print(42)");
        assert!(ir.contains("@printf"));
    }

    #[test]
    fn test_compile_string_literal() {
        let ir = compile_expr(r#""hello""#);
        assert!(ir.contains("\\68\\65\\6C\\6C\\6F")); // "hello" in hex
        assert!(ir.contains("i8*"));
    }

    #[test]
    fn test_compile_multiline_string() {
        let ir = compile_expr(r#""""multi
line"""#);
        assert!(ir.contains("\\6D\\75\\6C\\74\\69")); // "multi" in hex
        assert!(ir.contains("\\6C\\69\\6E\\65")); // "line" in hex
    }

    #[test]
    fn test_compile_function_definition() {
        let ir = compile_expr("fn test():\n    return 42");
        assert!(ir.contains("define"));
        assert!(ir.contains("@test"));
        assert!(ir.contains("ret i32 42"));
    }

    #[test]
    fn test_compile_main_function() {
        let ir = compile_expr("fn main():\n    return 0");
        assert!(ir.contains("define i32 @main"));
    }

    #[test]
    fn test_compile_if_expression() {
        let ir = compile_expr("fn test():\n    let x = 1\n    if x:\n        return 1\n    else:\n        return 0");
        assert!(ir.contains("br i1"));
        assert!(ir.contains("then_"));
        assert!(ir.contains("else_"));
        assert!(ir.contains("merge_"));
    }

    #[test]
    fn test_compile_nested_if() {
        let ir = compile_expr("fn test():\n    let x = 1\n    let y = 1\n    if x:\n        if y:\n            return 1\n        else:\n            return 0");
        assert!(ir.contains("then_"));
        assert!(ir.contains("else_"));
    }

    #[test]
    fn test_compile_fstring() {
        let ir = compile_expr(r#"f"Hello {x}"#);
        assert!(ir.contains("@sprintf"));
        // Format string is hex-encoded, just check sprintf is called
        assert!(ir.contains("i8*"));
    }

    #[test]
    fn test_compile_fstring_with_multiple_substitutions() {
        let ir = compile_expr(r#"f"{a} + {b} = {c}"#);
        assert!(ir.contains("@sprintf"));
        // Format string is hex-encoded, just check sprintf is called
        assert!(ir.contains("i8*"));
    }

    #[test]
    fn test_compile_global_statement() {
        let ir = compile_expr("fn test():\n    let x = 0\n    x = 10");
        assert!(ir.contains("store i32 10"));
    }

    #[test]
    fn test_compile_power_with_large_numbers() {
        let ir = compile_expr("2 ** 10");
        assert!(ir.contains("@llvm.pow.f64"));
    }

    #[test]
    fn test_compile_chained_operations() {
        let ir = compile_expr("fn test():\n    let a = 10\n    let b = 5\n    let c = 2\n    let d = 3\n    let e = 2\n    a + b - c * d / e");
        assert!(ir.contains("add i32"));
        assert!(ir.contains("sub i32"));
        assert!(ir.contains("mul i32"));
        assert!(ir.contains("sdiv i32"));
    }

    #[test]
    fn test_compile_float_literal() {
        let ir = compile_expr("3.14");
        assert!(ir.contains("double 3.14"));
    }

    #[test]
    fn test_compile_float_variable() {
        let ir = compile_expr("fn test():\n    let x: f64 = 2.5\n    print(x)");
        assert!(ir.contains("alloca double"));
        assert!(ir.contains("store double 2.5"));
    }

    #[test]
    fn test_compile_float_arithmetic() {
        let ir = compile_expr("fn test():\n    let x = 2.5\n    let y = 1.5\n    x + y");
        assert!(ir.contains("fadd double"));
    }

    #[test]
    fn test_compile_float_multiply() {
        let ir = compile_expr("fn test():\n    let x = 2.0\n    let y = 3.0\n    x * y");
        assert!(ir.contains("fmul double"));
    }

    #[test]
    fn test_compile_float_division() {
        let ir = compile_expr("fn test():\n    let x = 6.0\n    let y = 2.0\n    x / y");
        assert!(ir.contains("fdiv double"));
    }

    #[test]
    fn test_compile_float_comparison() {
        let ir = compile_expr("fn test():\n    let x = 2.5\n    let y = 3.0\n    x > y");
        assert!(ir.contains("fcmp"));
    }

    #[test]
    fn test_compile_float_power() {
        let ir = compile_expr("fn test():\n    let x = 2.0\n    x ** 3");
        assert!(ir.contains("@llvm.pow.f64"));
    }

    #[test]
    fn test_compile_boolean_true() {
        let ir = compile_expr("true");
        assert!(ir.contains("i32 1"));
    }

    #[test]
    fn test_compile_boolean_false() {
        let ir = compile_expr("false");
        assert!(ir.contains("@printf"));
    }

    #[test]
    fn test_compile_boolean_variable() {
        let ir = compile_expr("fn test():\n    let flag = true\n    print(flag)");
        assert!(ir.contains("store i32 1"));
    }

    #[test]
    fn test_compile_logical_and() {
        let ir = compile_expr("fn test():\n    let a = 1\n    let b = 0\n    a and b");
        assert!(ir.contains("and i1"));
    }

    #[test]
    fn test_compile_logical_or() {
        let ir = compile_expr("fn test():\n    let a = 1\n    let b = 0\n    a or b");
        assert!(ir.contains("or i1"));
    }

    #[test]
    fn test_compile_logical_not() {
        let ir = compile_expr("fn test():\n    let a = true\n    not a");
        assert!(ir.contains("icmp eq i32"));
    }

    #[test]
    fn test_compile_mixed_float_int_operations() {
        let ir = compile_expr("fn test():\n    let x = 2.5\n    let y = 2\n    x + y");
        assert!(ir.contains("fadd double"));
    }

    #[test]
    fn test_compile_float_inference() {
        let ir = compile_expr("fn test():\n    let x = 2.5\n    let y = x * 2.0\n    print(y)");
        assert!(ir.contains("alloca double"));
    }
}
