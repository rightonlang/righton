use crate::ast::*;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq)]
enum VarType {
    String { id: String, len: usize },
}

#[derive(Debug, Clone)]
enum Type {
    I32,
    Ptr,
}

#[derive(Debug, Clone)]
enum Val {
    Imm(i32),
    Reg(String),
}

impl Val {
    fn as_str(&self) -> String {
        match self {
            Val::Imm(n) => n.to_string(),
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

    pub fn generate(&mut self, program: &Program) -> String {
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
            self.generate_expr(global, &[], &mut HashMap::new(), "void");
        }
        for func in &program.functions {
            self.generate_function(func);
        }

        ir.push_str(&self.globals);
        ir.push_str(&self.functions);
        ir
    }

    fn next_block_label(&mut self, prefix: &str) -> String {
        let id = self.block_count;
        self.block_count += 1;
        format!("{}_{}", prefix, id)
    }

    fn generate_function(&mut self, func: &FunctionDef) {
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
            self.generate_expr(expr, &func.params, &mut locals, return_type);
        }

        if !has_return {
            self.emit_default_return(return_type);
        }

        writeln!(&mut self.functions, "}}\n").unwrap();
        self.current_function = None;
    }

    fn infer_expr_type(
        &self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> &'static str {
        match expr {
            Expr::Literal(Literal::Int(_)) => "i32",
            Expr::StringLiteral(_) | Expr::MultilineString(_) | Expr::FString(_) => "i8*",
            Expr::Identifier(name) => {
                if let Some((typ, _, _)) = locals.get(name) {
                    match typ {
                        Type::I32 => "i32",
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
            Expr::Binary(_, op, _) if *op == BinOp::Pow => "i32",
            Expr::Binary(_, _, _) => "i32",
            Expr::Unary(_, inner) => self.infer_expr_type(inner, params, locals),
            Expr::If { .. } => "void",
            _ => "void",
        }
    }

    fn infer_return_type(&self, body: &[Expr]) -> &'static str {
        if let Some(name) = &self.current_function {
            if name == "main" {
                return "i32";
            }
        }
        body.iter()
            .find_map(|expr| {
                if let Expr::Return(inner) = expr {
                    Some(self.infer_expr_type(inner, &[], &HashMap::new()))
                } else {
                    None
                }
            })
            .unwrap_or("void")
    }

    fn emit_default_return(&mut self, ty: &str) {
        match ty {
            "i32" => self.functions.push_str("  ret i32 0\n"),
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
    ) {
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
                        "str" | "string" | "ptr" => Type::Ptr,
                        _ => panic!("unsupported type: {}", t),
                    }
                } else {
                    let inferred = self.infer_expr_type(value, params, locals);
                    match inferred {
                        "i32" => Type::I32,
                        "i8*" => Type::Ptr,
                        _ => panic!("couldn't output the type for the variable {}", name),
                    }
                };

                match var_type {
                    Type::I32 => {
                        let val = self.generate_int_expr(value, params, locals);
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
                    Type::Ptr => {
                        let ptr = self.next_temp();
                        match &**value {
                            Expr::StringLiteral(s) | Expr::MultilineString(s) => {
                                let id = self.emit_string_const(s);
                                let len = s.len() + 1;
                                self.emit_gep(&id, len, &ptr);
                            }
                            Expr::FString(elements) => {
                                self.emit_fstring(elements, params, locals, &ptr);
                            }
                            Expr::Identifier(id_name) => {
                                self.emit_print_arg(value, params, locals, &ptr);
                            }
                            _ => panic!("let: expected str expression for type ptr/str"),
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
                    .unwrap_or_else(|| panic!("assign of unknown variable: {}", name));

                if *is_const {
                    panic!("cannot assign to const-variable: {}", name);
                }

                match var_ty {
                    Type::I32 => {
                        let val = self.generate_int_expr(value, params, locals);
                        writeln!(
                            &mut self.functions,
                            "  store i32 {}, i32* %{}",
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
                                self.emit_fstring(elements, params, locals, &ptr);
                            }
                            _ => panic!("assign: expected str"),
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
                        let val = self.generate_int_expr(arg, params, locals);
                        self.emit_printf_int(&val.as_str());
                    } else if ty == "i8*" {
                        let result = self.next_temp();
                        self.emit_print_arg(arg, params, locals, &result);
                        self.emit_printf_str(&result);
                    }
                }
            }

            Expr::Call { func, args } if func == "asm" => {
                self.emit_asm_call(args, params, locals);
            }

            Expr::Return(inner) => {
                let ty = self.infer_expr_type(inner, params, locals);
                if ty == "i32" && return_ty == "i32" {
                    let val = self.generate_int_expr(inner, params, locals);
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
                            self.emit_fstring(elements, params, locals, &ptr);
                        }
                        Expr::Identifier(name) => {
                            self.emit_print_arg(inner, params, locals, &ptr);
                        }
                        _ => panic!("return: unsupported str expression"),
                    }
                    writeln!(&mut self.functions, "  ret i8* %{}", ptr).unwrap();
                } else {
                    panic!("return: types mismatch");
                }
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_val = self.generate_int_expr(condition, params, locals);
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
                    self.generate_expr(stmt, params, locals, return_ty);
                }

                writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();

                if let Some(else_block) = else_branch {
                    writeln!(&mut self.functions, "{}:", else_label).unwrap();
                    for stmt in &else_block.stmts {
                        self.generate_expr(stmt, params, locals, return_ty);
                    }
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                }

                writeln!(&mut self.functions, "{}:", merge_label).unwrap();
            }

            Expr::Literal(Literal::Int(_)) | Expr::Unary(_, _) | Expr::Binary(_, _, _) => {
                let val = self.generate_int_expr(expr, params, locals);
                self.emit_printf_int(&val.as_str());
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

    fn emit_fstring(
        &mut self,
        elements: &[Expr],
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
        result_ptr: &str,
    ) {
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
                        panic!("fstring: unknown variable {}", name);
                    }
                }
                _ => panic!("fstring: unsupported element"),
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
    }

    fn generate_int_expr(
        &mut self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> Val {
        match expr {
            Expr::Literal(Literal::Int(n)) => Val::Imm(*n),

            Expr::Identifier(name) => {
                if let Some((typ, alloca, _)) = locals.get(name) {
                    match typ {
                        Type::I32 => {
                            let res = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = load i32, i32* %{}",
                                res, alloca
                            )
                            .unwrap();
                            return Val::Reg(res);
                        }
                        Type::Ptr => {
                            panic!(
                                "variable '{}' has a string type, but is used in a numeric context.",
                                name
                            );
                        }
                    }
                }

                if let Some(idx) = params.iter().position(|p| p == name) {
                    panic!(
                        "The function parameter '{}' has a string type (i8*), but is used in a numeric context",
                        name
                    );
                }

                if self.global_vars.contains_key(name) {
                    panic!(
                        "The global variable '{}' has a string type, but is used in a numeric context",
                        name
                    );
                }

                panic!("an unknown variable in a numeric context: '{}'", name);
            }

            Expr::Unary(op, inner) => {
                let v = self.generate_int_expr(inner, params, locals);
                match op {
                    UnaryOp::Pos => v,
                    UnaryOp::Neg => match v {
                        Val::Imm(n) => Val::Imm(-n),
                        Val::Reg(r) => {
                            let res = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = sub i32 0, {}", res, r).unwrap();
                            Val::Reg(res)
                        }
                    },
                }
            }

            Expr::Binary(l, op, r) => {
                let lv = self.generate_int_expr(l, params, locals);
                let rv = self.generate_int_expr(r, params, locals);
                let res = self.next_temp();

                match op {
                    BinOp::Add => writeln!(
                        &mut self.functions,
                        "  %{} = add i32 {}, {}",
                        res,
                        lv.as_str(),
                        rv.as_str()
                    ),
                    BinOp::Sub => writeln!(
                        &mut self.functions,
                        "  %{} = sub i32 {}, {}",
                        res,
                        lv.as_str(),
                        rv.as_str()
                    ),
                    BinOp::Mul => writeln!(
                        &mut self.functions,
                        "  %{} = mul i32 {}, {}",
                        res,
                        lv.as_str(),
                        rv.as_str()
                    ),
                    BinOp::Div => writeln!(
                        &mut self.functions,
                        "  %{} = sdiv i32 {}, {}",
                        res,
                        lv.as_str(),
                        rv.as_str()
                    ),
                    BinOp::DivMod => writeln!(
                        &mut self.functions,
                        "  %{} = srem i32 {}, {}",
                        res,
                        lv.as_str(),
                        rv.as_str()
                    ),
                    BinOp::Pow => {
                        let ld = self.next_temp();
                        let rd = self.next_temp();
                        let pow = self.next_temp();
                        let int_res = self.next_temp();

                        writeln!(
                            &mut self.functions,
                            "  %{} = sitofp i32 {} to double",
                            ld,
                            lv.as_str()
                        )
                        .unwrap();
                        writeln!(
                            &mut self.functions,
                            "  %{} = sitofp i32 {} to double",
                            rd,
                            rv.as_str()
                        )
                        .unwrap();
                        writeln!(
                            &mut self.functions,
                            "  %{} = call double @llvm.pow.f64(double %{}, double %{})",
                            pow, ld, rd
                        )
                        .unwrap();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fptosi double %{} to i32",
                            int_res, pow
                        )
                        .unwrap();
                        return Val::Reg(int_res);
                    }
                    BinOp::Eq => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = icmp eq i32 {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Val::Reg(zext);
                    }
                    BinOp::Ne => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = icmp ne i32 {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Val::Reg(zext);
                    }
                    BinOp::Lt => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = icmp slt i32 {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Val::Reg(zext);
                    }
                    BinOp::Le => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = icmp sle i32 {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Val::Reg(zext);
                    }
                    BinOp::Gt => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = icmp sgt i32 {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Val::Reg(zext);
                    }
                    BinOp::Ge => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = icmp sge i32 {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Val::Reg(zext);
                    }
                }
                .unwrap();
                Val::Reg(res)
            }

            _ => panic!("unexpected expression in int-context: {:?}", expr),
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
    ) {
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
                    panic!("unknown variable: {}", name);
                }
            }
            _ => panic!("print: unsupported argument"),
        }
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
    ) {
        let code = if let Expr::StringLiteral(s) = &args[0] {
            s
        } else {
            panic!("asm: first argument is a str");
        };

        if args.len() == 1 {
            writeln!(
                &mut self.functions,
                "  call void asm sideeffect \"{}\", \"\"()",
                code.escape_default()
            )
            .unwrap();
            return;
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
                        Type::Ptr => {
                            constraints.push_str("=r");
                            operands.push_str(&format!("i8** %{}", alloca));
                        }
                    }
                } else if let Some(idx) = params.iter().position(|p| p == name) {
                    constraints.push_str("r");
                    operands.push_str(&format!("i8* %arg{}", idx));
                } else {
                    panic!("asm: unknown variable {}", name);
                }
            } else {
                panic!("asm: arguments must be identifiers");
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
    }
}
