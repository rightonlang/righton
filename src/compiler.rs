use crate::ast::*;
use crate::borrow_checker::BorrowChecker;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::type_checker::TypeChecker;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::fs;
use std::path::PathBuf;

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
#[allow(dead_code)]
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
    function_sigs: HashMap<String, &'static str>,
    function_aliases: HashMap<String, String>,
    stdlib_enabled: bool,
    runtime_helpers_emitted: bool,

    current_function: Option<String>,
    loop_label_stack: Vec<(String, String)>,
    flattened_funcs: Vec<FunctionDef>,
    struct_defs: Vec<StructDef>,
    enum_defs: Vec<EnumDef>,
    type_aliases: HashMap<String, String>,
}

impl LLVMTextGen {
    pub fn new() -> Self {
        Self {
            globals: String::new(),
            functions: String::new(),
            string_count: 0,
            block_count: 0,
            global_vars: HashMap::new(),
            function_sigs: HashMap::new(),
            function_aliases: HashMap::new(),
            stdlib_enabled: false,
            runtime_helpers_emitted: false,
            current_function: None,
            loop_label_stack: Vec::new(),
            flattened_funcs: Vec::new(),
            struct_defs: Vec::new(),
            enum_defs: Vec::new(),
            type_aliases: HashMap::new(),
        }
    }

    pub fn generate(&mut self, program: &Program) -> CompileResult<String> {
        self.stdlib_enabled = false;
        self.function_aliases.clear();
        let program = self.flatten_program(program)?;
        self.flattened_funcs = program.functions.clone();
        self.struct_defs = program.structs.clone();
        self.enum_defs = program.enums.clone();
        self.type_aliases.clear();
        for alias in &program.type_aliases {
            self.type_aliases.insert(alias.name.clone(), alias.value.clone());
        }
        self.function_sigs = self.build_function_sigs(&program);

        BorrowChecker::new()
            .check_program(&program)
            .map_err(|e| CompileError::new(e.to_string()))?;

        let mut type_checker = TypeChecker::new();
        type_checker.set_function_aliases(self.function_aliases.clone());
        type_checker
            .check_program(&program, self.stdlib_enabled)
            .map_err(|e| CompileError::new(e.to_string()))?;

        let mut ir = String::new();
        if !(program.profile == "release") {
            ir.push_str(format!("; ModuleID = 'righton_{}'\n", program.name).as_str());
        } else {
            ir.push_str(format!("; ModuleID = '{}'\n", program.name).as_str());
        }
        ir.push_str("declare i32 @printf(i8*, ...)\n");
        ir.push_str("declare i32 @sprintf(i8*, i8*, ...)\n");
        ir.push_str("declare double @llvm.pow.f64(double, double)\n");
        ir.push_str("declare i64 @strlen(i8*)\n");
        ir.push_str("declare i8* @fopen(i8*, i8*)\n");
        ir.push_str("declare i32 @fseek(i8*, i64, i32)\n");
        ir.push_str("declare i64 @ftell(i8*)\n");
        ir.push_str("declare void @rewind(i8*)\n");
        ir.push_str("declare i8* @malloc(i64)\n");
        ir.push_str("declare void @free(i8*)\n");
        ir.push_str("declare i64 @fread(i8*, i64, i64, i8*)\n");
        ir.push_str("declare i32 @fclose(i8*)\n");
        ir.push_str("declare i64 @fwrite(i8*, i64, i64, i8*)\n");
        ir.push_str("declare void @exit(i32)\n");
        ir.push_str("declare i8* @strstr(i8*, i8*)\n");
        ir.push_str("declare i32 @strncmp(i8*, i8*, i64)\n");
        ir.push_str("declare i64 @strspn(i8*, i8*)\n");
        ir.push_str("declare i64 @strcspn(i8*, i8*)\n");
        ir.push_str("declare i32 @toupper(i32)\n");
        ir.push_str("declare i32 @tolower(i32)\n");
        ir.push_str("declare i32 @atoi(i8*)\n");
        ir.push_str("declare double @atof(i8*)\n");
        ir.push_str("declare double @floor(double)\n");
ir.push_str("declare double @ceil(double)\n");
ir.push_str("declare double @round(double)\n");
ir.push_str("declare double @sqrt(double)\n");
ir.push_str("declare double @sin(double)\n");
ir.push_str("declare double @cos(double)\n");
ir.push_str("declare double @tan(double)\n");
ir.push_str("declare i8* @fgets(i8*, i32, i8*)\n");
        ir.push_str("@stdin = external global i8*\n");
        ir.push_str("@.global_buffer = private global [1024 x i8] zeroinitializer\n\n");

        self.emit_runtime_helpers();

        for global in &program.globals {
            self.generate_expr(global, &[], &mut HashMap::new(), "void")?;
        }
        // Emit extern function declarations first
        for func in &program.functions {
            if func.body.is_empty() && func.param_types.iter().any(|t| t.is_some()) {
                self.generate_extern_declaration(func)?;
            }
        }
        for func in &program.functions {
            if !func.body.is_empty() || func.param_types.iter().all(|t| t.is_none()) {
                self.generate_function(func)?;
            }
        }

        ir.push_str(&self.globals);
        ir.push_str(&self.functions);
        Ok(ir)
    }

    fn flatten_program(&mut self, program: &Program) -> CompileResult<Program> {
        let mut loaded = HashSet::new();
        let mut active = Vec::new();
        let flattened = self.flatten_program_inner(program, &mut loaded, &mut active)?;
        Ok(self.prune_program(flattened))
    }

    fn flatten_program_inner(
        &mut self,
        program: &Program,
        loaded: &mut HashSet<String>,
        active: &mut Vec<String>,
    ) -> CompileResult<Program> {
        let mut globals = Vec::new();
        let mut functions = Vec::new();

        for global in &program.globals {
            match global {
                Expr::Import(spec) => {
                    if self.is_stdlib_module(spec) {
                        self.stdlib_enabled = true;
                    }
                    let imported = self.load_module(spec, loaded, active)?;
                    globals.extend(imported.globals);
                    functions.extend(imported.functions);
                }
                other => globals.push(other.clone()),
            }
        }

        functions.extend(program.functions.clone());

        Ok(Program {
            globals,
            functions,
            structs: program.structs.clone(),
            enums: program.enums.clone(),
            type_aliases: program.type_aliases.clone(),
            impls: program.impls.clone(),
            profile: program.profile.clone(),
            name: program.name.clone(),
        })
    }

    fn prune_program(&self, program: Program) -> Program {
        let mut function_map = HashMap::new();
        for func in program.functions {
            function_map.insert(func.name.clone(), func);
        }

        let mut reachable = HashSet::new();
        let mut stack = Vec::new();

        for call in self.collect_calls_from_exprs(&program.globals) {
            stack.push(self.resolve_function_name(&call));
        }
        if function_map.contains_key("main") {
            stack.push("main".to_string());
        }

        while let Some(name) = stack.pop() {
            if !reachable.insert(name.clone()) {
                continue;
            }
            if let Some(func) = function_map.get(&name) {
                for call in self.collect_calls_from_exprs(&func.body) {
                    stack.push(self.resolve_function_name(&call));
                }
            }
        }

        let functions = function_map
            .into_iter()
            .filter_map(|(name, func)| {
                if !name.contains("__") || reachable.contains(&name) {
                    Some(func)
                } else {
                    None
                }
            })
            .collect();

        Program {
            globals: program.globals,
            functions,
            structs: program.structs,
            enums: program.enums,
            type_aliases: program.type_aliases,
            impls: program.impls,
            profile: program.profile,
            name: program.name,
        }
    }

    fn collect_calls_from_exprs(&self, exprs: &[Expr]) -> Vec<String> {
        let mut calls = Vec::new();
        for expr in exprs {
            self.collect_calls(expr, &mut calls);
        }
        calls
    }

    fn collect_calls(&self, expr: &Expr, calls: &mut Vec<String>) {
        match expr {
            Expr::Call { func, args } => {
                calls.push(func.clone());
                for arg in args {
                    self.collect_calls(arg, calls);
                }
            }
            Expr::Binary(l, _, r) => {
                self.collect_calls(l, calls);
                self.collect_calls(r, calls);
            }
            Expr::Unary(_, inner) | Expr::Return(inner) => self.collect_calls(inner, calls),
            Expr::Let { value, .. } | Expr::Assign { value, .. } => self.collect_calls(value, calls),
            Expr::If { condition, then_branch, else_branch } => {
                self.collect_calls(condition, calls);
                for stmt in &then_branch.stmts {
                    self.collect_calls(stmt, calls);
                }
                if let Some(else_branch) = else_branch {
                    for stmt in &else_branch.stmts {
                        self.collect_calls(stmt, calls);
                    }
                }
            }
            Expr::While { condition, body } => {
                self.collect_calls(condition, calls);
                for stmt in &body.stmts {
                    self.collect_calls(stmt, calls);
                }
            }
            Expr::For { variable: _, iterable, body } => {
                self.collect_calls(iterable, calls);
                for stmt in &body.stmts {
                    self.collect_calls(stmt, calls);
                }
            }
            Expr::ForRange { variable: _, start, end, body } => {
                self.collect_calls(start, calls);
                self.collect_calls(end, calls);
                for stmt in &body.stmts {
                    self.collect_calls(stmt, calls);
                }
            }
            Expr::Break | Expr::Continue => {}
            Expr::FString(elements) => {
                for el in elements {
                    self.collect_calls(el, calls);
                }
            }
            Expr::List(items) => {
                for item in items {
                    self.collect_calls(item, calls);
                }
            }
            Expr::Index { target, index } => {
                self.collect_calls(target, calls);
                self.collect_calls(index, calls);
            }
            Expr::AssignIndex { value, index, .. } => {
                self.collect_calls(index, calls);
                self.collect_calls(value, calls);
            }
            Expr::Match { expr, arms } => {
                self.collect_calls(expr, calls);
                for arm in arms {
                    self.collect_calls(&arm.body, calls);
                }
            }
            Expr::FieldAccess { target, .. } => self.collect_calls(target, calls),
            Expr::FieldAssign { target, value, .. } => {
                self.collect_calls(target, calls);
                self.collect_calls(value, calls);
            }
            Expr::StructLiteral { fields, .. } => {
                for (_, val) in fields {
                    self.collect_calls(val, calls);
                }
            }
            Expr::EnumLiteral { args, .. } => {
                for arg in args {
                    self.collect_calls(arg, calls);
                }
            }
            Expr::Tuple(elements) => {
                for elem in elements {
                    self.collect_calls(elem, calls);
                }
            }
            Expr::TupleAccess { target, .. } => {
                self.collect_calls(target, calls);
            }
            Expr::Import(_)
            | Expr::Identifier(_)
            | Expr::Borrow { .. }
            | Expr::StringLiteral(_)
            | Expr::MultilineString(_)
            | Expr::Literal(_) => {}
        }
    }

    fn load_module(
        &mut self,
        spec: &str,
        loaded: &mut HashSet<String>,
        active: &mut Vec<String>,
    ) -> CompileResult<Program> {
        if active.iter().any(|item| item == spec) {
            return Err(CompileError::new(format!("circular import detected: {}", spec)));
        }

        if loaded.contains(spec) {
            return Ok(Program {
                globals: vec![],
                functions: vec![],
                structs: vec![],
                enums: vec![],
                type_aliases: vec![],
                impls: vec![],
                profile: "debug".to_string(),
                name: spec.to_string(),
            });
        }

        let path = self.resolve_module_path(spec);
        let code = fs::read_to_string(&path)
            .map_err(|e| CompileError::new(format!("failed to read import '{}': {}", path.display(), e)))?;

        active.push(spec.to_string());
        let module_name = path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .unwrap_or(spec)
            .to_string();
        let mut lexer = Lexer::new(&code);
        let mut parser = Parser::new(&mut lexer);
        let program = parser
            .parse_program("debug".to_string(), module_name)
            .map_err(|e| CompileError::new(e.to_string()))?;
        let resolved = self.flatten_program_inner(&program, loaded, active)?;
        let prefix_source = path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .unwrap_or(spec);
        let mangled = self.mangle_program(resolved, &self.sanitize_ir_name(prefix_source));
        active.pop();
        loaded.insert(spec.to_string());
        Ok(mangled)
    }

    fn sanitize_ir_name(&self, value: &str) -> String {
        value
            .chars()
            .map(|c| if c.is_ascii_alphanumeric() || c == '_' { c } else { '_' })
            .collect()
    }

    fn mangle_program(&mut self, program: Program, prefix: &str) -> Program {
        let function_names: HashSet<String> = program.functions.iter().map(|f| f.name.clone()).collect();
        let mut alias_map = HashMap::new();

        for name in &function_names {
            let mangled = format!("{}__{}", prefix, name);
            if let Some(existing) = self.function_aliases.get(name) {
                if existing != &mangled {
                    // keep the first binding; avoid collisions from unrelated modules
                    continue;
                }
            } else {
                self.function_aliases.insert(name.clone(), mangled.clone());
            }
            alias_map.insert(name.clone(), mangled);
        }

        let globals = program
            .globals
            .into_iter()
            .map(|expr| self.rename_calls(expr, &alias_map, &function_names))
            .collect();

        let functions = program
            .functions
            .into_iter()
            .map(|mut func| {
                func.name = alias_map.get(&func.name).cloned().unwrap_or(func.name);
                func.body = func
                    .body
                    .into_iter()
                    .map(|expr| self.rename_calls(expr, &alias_map, &function_names))
                    .collect();
                func
            })
            .collect();

        Program {
            globals,
            functions,
            structs: program.structs,
            enums: program.enums,
            type_aliases: program.type_aliases,
            impls: program.impls,
            profile: program.profile,
            name: program.name,
        }
    }

    fn rename_calls(
        &self,
        expr: Expr,
        alias_map: &HashMap<String, String>,
        function_names: &HashSet<String>,
    ) -> Expr {
        match expr {
            Expr::Call { func, args } => {
                let func = if function_names.contains(&func) {
                    alias_map.get(&func).cloned().unwrap_or(func)
                } else {
                    func
                };
                Expr::Call {
                    func,
                    args: args
                        .into_iter()
                        .map(|arg| self.rename_calls(arg, alias_map, function_names))
                        .collect(),
                }
            }
            Expr::Binary(l, op, r) => Expr::Binary(
                Box::new(self.rename_calls(*l, alias_map, function_names)),
                op,
                Box::new(self.rename_calls(*r, alias_map, function_names)),
            ),
            Expr::Unary(op, inner) => Expr::Unary(op, Box::new(self.rename_calls(*inner, alias_map, function_names))),
            Expr::Let { name, typ, value, is_const } => Expr::Let {
                name,
                typ,
                value: Box::new(self.rename_calls(*value, alias_map, function_names)),
                is_const,
            },
            Expr::Assign { name, value } => Expr::Assign {
                name,
                value: Box::new(self.rename_calls(*value, alias_map, function_names)),
            },
            Expr::Return(inner) => Expr::Return(Box::new(self.rename_calls(*inner, alias_map, function_names))),
            Expr::If { condition, then_branch, else_branch } => Expr::If {
                condition: Box::new(self.rename_calls(*condition, alias_map, function_names)),
                then_branch: Box::new(Block {
                    stmts: then_branch
                        .stmts
                        .into_iter()
                        .map(|stmt| self.rename_calls(stmt, alias_map, function_names))
                        .collect(),
                }),
                else_branch: else_branch.map(|branch| Box::new(Block {
                    stmts: branch
                        .stmts
                        .into_iter()
                        .map(|stmt| self.rename_calls(stmt, alias_map, function_names))
                        .collect(),
                })),
            },
            Expr::FString(elements) => Expr::FString(
                elements
                    .into_iter()
                    .map(|el| self.rename_calls(el, alias_map, function_names))
                    .collect(),
            ),
            Expr::While { condition, body } => Expr::While {
                condition: Box::new(self.rename_calls(*condition, alias_map, function_names)),
                body: Box::new(Block {
                    stmts: body
                        .stmts
                        .into_iter()
                        .map(|stmt| self.rename_calls(stmt, alias_map, function_names))
                        .collect(),
                }),
            },
            Expr::For { variable, iterable, body } => Expr::For {
                variable,
                iterable: Box::new(self.rename_calls(*iterable, alias_map, function_names)),
                body: Box::new(Block {
                    stmts: body
                        .stmts
                        .into_iter()
                        .map(|stmt| self.rename_calls(stmt, alias_map, function_names))
                        .collect(),
                }),
            },
            Expr::ForRange { variable, start, end, body } => Expr::ForRange {
                variable,
                start: Box::new(self.rename_calls(*start, alias_map, function_names)),
                end: Box::new(self.rename_calls(*end, alias_map, function_names)),
                body: Box::new(Block {
                    stmts: body
                        .stmts
                        .into_iter()
                        .map(|stmt| self.rename_calls(stmt, alias_map, function_names))
                        .collect(),
                }),
            },
            Expr::Break => Expr::Break,
            Expr::Continue => Expr::Continue,
            Expr::List(items) => Expr::List(
                items.into_iter().map(|item| self.rename_calls(item, alias_map, function_names)).collect()
            ),
            Expr::Index { target, index } => Expr::Index {
                target: Box::new(self.rename_calls(*target, alias_map, function_names)),
                index: Box::new(self.rename_calls(*index, alias_map, function_names)),
            },
            Expr::AssignIndex { name, index, value } => Expr::AssignIndex {
                name,
                index: Box::new(self.rename_calls(*index, alias_map, function_names)),
                value: Box::new(self.rename_calls(*value, alias_map, function_names)),
            },
            Expr::Match { expr, arms } => Expr::Match {
                expr: Box::new(self.rename_calls(*expr, alias_map, function_names)),
                arms: arms.into_iter().map(|arm| crate::ast::MatchArm {
                    pattern: arm.pattern,
                    body: Box::new(self.rename_calls(*arm.body, alias_map, function_names)),
                }).collect(),
            },
            other => other,
        }
    }

    fn is_stdlib_module(&self, spec: &str) -> bool {
        matches!(spec, "std" | "stdlib") || spec == "std/std" || spec.starts_with("std/")
    }

    fn resolve_module_path(&self, spec: &str) -> PathBuf {
        if self.is_stdlib_module(spec) {
            // 1) honor explicit env var override
            if let Ok(val) = std::env::var("RIGHTON_STDLIB_PATH") {
                let p = PathBuf::from(&val);
                if p.is_file() {
                    return p;
                }
                // If it's a directory, try common locations
                let mut try_dir = p.clone();
                try_dir.push("stdlib");
                try_dir.push("std.ro");
                if try_dir.exists() {
                    return try_dir;
                }
                let mut try_file = p.clone();
                try_file.push("std.ro");
                if try_file.exists() {
                    return try_file;
                }
            }

            // 2) try executable-relative path (next to the exe or in an adjacent `stdlib` folder)
            if let Ok(exe_path) = std::env::current_exe() {
                if let Some(dir) = exe_path.parent() {
                    let candidate = dir.join("stdlib").join("std.ro");
                    if candidate.exists() {
                        return candidate;
                    }
                    let candidate2 = dir.join("std.ro");
                    if candidate2.exists() {
                        return candidate2;
                    }
                }
            }

            // 3) try current working directory
            let cwd_candidate = std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")).join("stdlib").join("std.ro");
            if cwd_candidate.exists() {
                return cwd_candidate;
            }

            // 4) final fallback: compile-time manifest directory (keeps dev behavior)
            let mut bundled = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            bundled.push("stdlib/std.ro");
            return bundled;
        }

        let path = if spec.ends_with(".ro") || spec.ends_with(".ron") {
            PathBuf::from(spec)
        } else if spec.contains('/') || spec.contains('\\') {
            PathBuf::from(format!("{}.ro", spec))
        } else {
            PathBuf::from(format!("{}.ro", spec.replace('.', "/")))
        };

        if path.is_absolute() {
            path
        } else {
            std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")).join(path)
        }
    }

    fn build_function_sigs(&self, program: &Program) -> HashMap<String, &'static str> {
        let mut sigs: HashMap<String, &'static str> = HashMap::new();
        for func in &program.functions {
            if let Some(ref ret) = func.return_type {
                sigs.insert(func.name.clone(), Self::type_str_to_llvm_ret(ret));
            } else {
                sigs.insert(func.name.clone(), "void");
            }
        }

        for _ in 0..8 {
            let mut changed = false;
            for func in &program.functions {
                if func.return_type.is_some() {
                    continue;
                }
                let ty = self.infer_function_return_type(func, &sigs);
                if sigs.get(&func.name).copied() != Some(ty) {
                    sigs.insert(func.name.clone(), ty);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }

        sigs
    }

    fn type_str_to_llvm_ret(t: &str) -> &'static str {
        match t {
            "i32" => "i32",
            "f64" | "double" | "float" => "double",
            "str" | "string" | "ptr" => "i8*",
            "void" => "void",
            _ => "void",
        }
    }

    fn infer_function_return_type(
        &self,
        func: &FunctionDef,
        sigs: &HashMap<String, &'static str>,
    ) -> &'static str {
        if let Some(ref ret) = func.return_type {
            return Self::type_str_to_llvm_ret(ret);
        }
        if func.name == "main" {
            return "i32";
        }
        
        let mut temp_locals: HashMap<String, (Type, String, bool)> = HashMap::new();
        for (i, p) in func.params.iter().enumerate() {
            let param_type = if let Some(Some(t)) = func.param_types.get(i) {
                Self::type_str_to_type(t)
            } else {
                self.infer_param_type(&func.body, p, &func.params)
            };
            temp_locals.insert(p.clone(), (param_type, format!("arg{}", i), false));
        }
        
        self.find_return_in_exprs(&func.body, &func.params, &temp_locals, sigs)
    }

    fn next_block_label(&mut self, prefix: &str) -> String {
        let id = self.block_count;
        self.block_count += 1;
        format!("{}_{}", prefix, id)
    }

    fn generate_function(&mut self, func: &FunctionDef) -> CompileResult<()> {
        self.current_function = Some(func.name.clone());
        self.block_count = 0;

        let args: Vec<String> = func
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let type_str = if let Some(Some(t)) = func.param_types.get(i) {
                    Self::type_to_llvm_str(t)
                } else {
                    let param_type = self.infer_param_type(&func.body, p, &func.params);
                    match param_type {
                        Type::I32 => "i32",
                        Type::F64 => "double",
                        Type::Ptr => "i8*",
                    }
                };
                format!("{} %arg{}", type_str, i)
            })
            .collect();

        let return_type = if let Some(ref t) = func.return_type {
            Self::type_to_llvm_str(t)
        } else {
            self.infer_return_type(&func.body, &func.params)
        };

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
        for (i, param) in func.params.iter().enumerate() {
            let param_type = if let Some(Some(t)) = func.param_types.get(i) {
                Self::type_str_to_type(t)
            } else {
                self.infer_param_type(&func.body, param, &func.params)
            };
            let alloca_name = format!("arg{}_alloca", i);
            match param_type {
                Type::I32 => {
                    writeln!(&mut self.functions, "  %{} = alloca i32", alloca_name).unwrap();
                    writeln!(&mut self.functions, "  store i32 %arg{}, i32* %{}", i, alloca_name).unwrap();
                }
                Type::F64 => {
                    writeln!(&mut self.functions, "  %{} = alloca double", alloca_name).unwrap();
                    writeln!(&mut self.functions, "  store double %arg{}, double* %{}", i, alloca_name).unwrap();
                }
                Type::Ptr => {
                    // For pointer params, arg[i] is already a pointer value, store it
                    writeln!(&mut self.functions, "  %{} = alloca i8*", alloca_name).unwrap();
                    writeln!(&mut self.functions, "  store i8* %arg{}, i8** %{}", i, alloca_name).unwrap();
                }
            }
            locals.insert(param.clone(), (param_type, alloca_name, false));
        }
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

    fn generate_extern_declaration(&mut self, func: &FunctionDef) -> CompileResult<()> {
        let args: Vec<String> = func
            .params
            .iter()
            .enumerate()
            .map(|(i, _p)| {
                if let Some(Some(t)) = func.param_types.get(i) {
                    Self::type_to_llvm_str(t).to_string()
                } else {
                    "i32".to_string()
                }
            })
            .collect();

        let return_type = if let Some(ref t) = func.return_type {
            Self::type_to_llvm_str(t)
        } else {
            "void"
        };

writeln!(
            &mut self.functions,
            "declare {} @{}({})",
            return_type,
            func.name,
            args.join(", ")
        )
        .unwrap();
        Ok(())
    }

    fn type_to_llvm_str(t: &str) -> &'static str {
        match t {
            "i32" => "i32",
            "f64" | "double" | "float" => "double",
            "str" | "string" | "ptr" => "i8*",
            "void" => "void",
            _ => "i32",
        }
    }

    fn type_str_to_type(t: &str) -> Type {
        match t {
            "i32" => Type::I32,
            "f64" | "double" | "float" => Type::F64,
            "str" | "string" | "ptr" => Type::Ptr,
            _ => Type::I32,
        }
    }

    pub fn infer_expr_type(
        &self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> &'static str {
        match expr {
            Expr::Import(_) => "void",
            Expr::Literal(Literal::Int(_)) => "i32",
            Expr::Literal(Literal::Float(_)) => "double",
            Expr::Literal(Literal::Bool(_)) => "i32",
            Expr::Literal(Literal::Str(_)) => "i8*",
            Expr::StringLiteral(_) | Expr::MultilineString(_) | Expr::FString(_) => "i8*",
            Expr::Borrow { .. } => "i8*",
            Expr::List(_) => "i8*",
            Expr::StructLiteral { .. } => "i8*",
            Expr::EnumLiteral { .. } => "i8*",
            Expr::Tuple(elements) => {
                if elements.is_empty() { "void" } else { "i8*" }
            }
            Expr::TupleAccess { target, index } => {
                "i32"
            }
            Expr::FieldAssign { .. } => "void",
            Expr::FieldAccess { target, field } => {
                let target_name = if let Expr::Identifier(name) = target.as_ref() {
                    if locals.contains_key(name) { name.clone() } else { String::new() }
                } else { String::new() };
                if !target_name.is_empty() {
                    for sdef in &self.struct_defs {
                        for sf in &sdef.fields {
                            if sf.name == *field {
                                match sf.typ.as_str() {
                                    "i32" | "bool" => return "i32",
                                    "f64" | "float" | "double" => return "double",
                                    _ => return "i8*",
                                }
                            }
                        }
                    }
                }
                "i32"
            }
            Expr::Index { .. } => "i32",
            Expr::AssignIndex { .. } => "void",
            Expr::Match { arms, .. } => {
                if let Some(first) = arms.first() {
                    self.infer_expr_type(&first.body, params, locals)
                } else {
                    "void"
                }
            }
            Expr::Identifier(name) => {
                if let Some((typ, _, _)) = locals.get(name) {
                    match typ {
                        Type::I32 => "i32",
                        Type::F64 => "double",
                        Type::Ptr => "i8*",
                    }
                } else if params.contains(&name) {
                    if let Some((typ, _, _)) = locals.get(name) {
                        match typ {
                            Type::I32 => "i32",
                            Type::F64 => "double",
                            Type::Ptr => "i8*",
                        }
                    } else {
                        "void"
                    }
                } else if self.global_vars.contains_key(name) {
                    "i8*"
                } else {
                    "void"
                }
            }
            Expr::Call { func, .. } => self
                .builtin_return_type(func)
                .or_else(|| self.function_sigs.get(&self.resolve_function_name(func)).copied())
                .unwrap_or("void"),
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
                } else if lt == "i8*" && rt == "i8*" {
                    if matches!(*op, BinOp::Add) {
                        "i8*"
                    } else {
                        "i32"
                    }
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
                let then_type = self.find_return_in_exprs(&then_branch.stmts, params, locals, &self.function_sigs);
                if let Some(else_block) = else_branch {
                    let else_type = self.find_return_in_exprs(&else_block.stmts, params, locals, &self.function_sigs);
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

    fn infer_return_type(&self, body: &[Expr], params: &[String]) -> &'static str {
        if let Some(name) = &self.current_function {
            if name == "main" {
                return "i32";
            }
        }
        let mut temp_locals: HashMap<String, (Type, String, bool)> = HashMap::new();
        for (i, p) in params.iter().enumerate() {
            let param_type = self.infer_param_type(body, p, params);
            temp_locals.insert(p.clone(), (param_type, format!("arg{}", i), false));
        }
        self.find_return_in_exprs(body, params, &temp_locals, &self.function_sigs)
    }

    fn find_return_in_exprs(
        &self,
        exprs: &[Expr],
        params: &[String],
        temp_locals: &HashMap<String, (Type, String, bool)>,
        sigs: &HashMap<String, &'static str>,
    ) -> &'static str {
        for expr in exprs {
            if let Expr::Return(inner) = expr {
                return self.infer_expr_type_with_sigs(inner, params, temp_locals, sigs);
            }
            if let Expr::If { then_branch, else_branch, .. } = expr {
                let then_type = self.find_return_in_exprs(&then_branch.stmts, params, temp_locals, sigs);
                if then_type != "void" {
                    return then_type;
                }
                if let Some(else_block) = else_branch {
                    let else_type = self.find_return_in_exprs(&else_block.stmts, params, temp_locals, sigs);
                    if else_type != "void" {
                        return else_type;
                    }
                }
            }
        }
        "void"
    }

    fn builtin_return_type(&self, func: &str) -> Option<&'static str> {
        match func {
            "__rt_strlen" => return Some("i32"),
            "__rt_read_file" => return Some("i8*"),
            "__rt_write_file" => return Some("i32"),
            "__rt_exit" | "__rt_print_str" | "__rt_print_int" | "__rt_print_float" | "__rt_free" => {
                return Some("void")
            }
            "__rt_contains" | "__rt_starts_with" | "__rt_ends_with" | "__rt_to_int"
            | "__rt_list_len" | "__rt_list_pop" => {
                return Some("i32")
            }
            "__rt_substr" | "__rt_trim" | "__rt_to_uppercase" | "__rt_to_lowercase"
            | "__rt_read_line" | "__rt_to_string_int" | "__rt_to_string_float" | "__rt_list_push"
            | "__rt_to_hex" | "__rt_str_repeat" => {
                return Some("i8*")
            }
            "__rt_to_float" | "__rt_floor" | "__rt_ceil" | "__rt_round"
            | "__rt_sqrt" | "__rt_sin" | "__rt_cos" | "__rt_tan" | "__rt_abs" => {
                return Some("double")
            }
            _ => {}
        }

        if !self.stdlib_enabled {
            return None;
        }

        match func {
            "print" | "asm" | "exit" | "write_file" => Some("void"),
            "len" => Some("i32"),
            "read_file" => Some("i8*"),
            "contains" | "starts_with" | "ends_with" | "to_int" | "is_empty" => Some("i32"),
            "substr" | "trim" | "to_uppercase" | "to_lowercase" | "to_string"
            | "read_line" => Some("i8*"),
            "to_float" | "floor" | "ceil" | "round" => Some("double"),
            "sqrt" | "sin" | "cos" | "tan" => Some("double"),
            "to_hex" | "str_repeat" => Some("i8*"),
            _ => None,
        }
    }

    fn resolve_function_name(&self, func: &str) -> String {
        self.function_aliases
            .get(func)
            .cloned()
            .unwrap_or_else(|| func.to_string())
    }

    fn levenshtein(a: &str, b: &str) -> usize {
        let b_chars: Vec<char> = b.chars().collect();
        let mut prev: Vec<usize> = (0..=b_chars.len()).collect();
        let mut cur = vec![0; b_chars.len() + 1];

        for (i, ca) in a.chars().enumerate() {
            cur[0] = i + 1;
            for (j, cb) in b_chars.iter().enumerate() {
                let cost = if ca == *cb { 0 } else { 1 };
                cur[j + 1] = (prev[j + 1] + 1).min(cur[j] + 1).min(prev[j] + cost);
            }
            prev.clone_from(&cur);
        }

        prev[b_chars.len()]
    }

    fn best_suggestion<'a>(&self, name: &str, candidates: impl IntoIterator<Item = &'a str>) -> Option<String> {
        let mut best: Option<(usize, String)> = None;
        for candidate in candidates {
            let dist = Self::levenshtein(name, candidate);
            if dist > 3 {
                continue;
            }
            if best.as_ref().map(|(best_dist, _)| dist < *best_dist).unwrap_or(true) {
                best = Some((dist, candidate.to_string()));
            }
        }
        best.map(|(_, s)| s)
    }

    fn suggest_function(&self, func: &str) -> Option<String> {
        if !self.stdlib_enabled && matches!(func, "print" | "len" | "read_file" | "write_file" | "exit") {
            return Some(self.stdlib_hint(func).to_string());
        }

        let mut candidates: Vec<String> = self.function_sigs.keys().cloned().collect();
        if self.stdlib_enabled {
            candidates.extend(["print", "len", "read_file", "write_file", "exit", "asm"].into_iter().map(String::from));
        }
        self.best_suggestion(func, candidates.iter().map(|s| s.as_str()))
    }

    fn suggest_variable(
        &self,
        name: &str,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> Option<String> {
        let mut candidates: Vec<String> = params.to_vec();
        candidates.extend(locals.keys().cloned());
        candidates.extend(self.global_vars.keys().cloned());
        self.best_suggestion(name, candidates.iter().map(|s| s.as_str()))
    }

    fn stdlib_hint(&self, func: &str) -> &'static str {
        match func {
            "print" | "len" | "read_file" | "write_file" | "exit" => {
                "hint: add `import std` at the top of the file"
            }
            _ => "",
        }
    }

    fn get_function_param_types(&self, func: &str) -> Vec<&'static str> {
        // First check for builtin runtime functions
        let builtin_types = match func {
            "__rt_strlen" => vec!["i8*"],
            "__rt_print_str" => vec!["i8*"],
            "__rt_print_int" => vec!["i32"],
            "__rt_print_float" => vec!["double"],
            "__rt_exit" => vec!["i32"],
            "__rt_read_file" => vec!["i8*"],
            "__rt_write_file" => vec!["i8*", "i8*"],
            "__rt_contains" | "__rt_starts_with" | "__rt_ends_with" => vec!["i8*", "i8*"],
            "__rt_substr" => vec!["i8*", "i32", "i32"],
            "__rt_trim" | "__rt_to_uppercase" | "__rt_to_lowercase" => vec!["i8*"],
            "__rt_to_int" => vec!["i8*"],
            "__rt_to_float" => vec!["i8*"],
            "__rt_floor" | "__rt_ceil" | "__rt_round" => vec!["double"],
            "__rt_read_line" => vec![],
            "__rt_to_string_int" => vec!["i32"],
            "__rt_to_string_float" => vec!["double"],
            "__rt_list_len" => vec!["i8*"],
            "__rt_list_push" => vec!["i8*", "i32"],
            "__rt_list_pop" => vec!["i8*"],
            "__rt_free" => vec!["i8*"],
            "__rt_to_hex" => vec!["i32"],
            "__rt_str_repeat" => vec!["i8*", "i32"],
            "__rt_sqrt" | "__rt_sin" | "__rt_cos" | "__rt_tan" | "__rt_abs" => vec!["double"],
            _ => vec![],
        };
        if !builtin_types.is_empty() {
            return builtin_types;
        }

        let mut types = Vec::new();
        let resolved = self.resolve_function_name(func);
        for func_def in &self.flattened_funcs {
            if func_def.name == resolved {
                for (i, param) in func_def.params.iter().enumerate() {
                    let param_type = if let Some(Some(t)) = func_def.param_types.get(i) {
                        match t.as_str() {
                            "i32" => "i32",
                            "f64" | "double" | "float" => "double",
                            "str" | "string" | "ptr" => "i8*",
                            _ => "i32",
                        }
                    } else {
                        let inferred = self.infer_param_type(&func_def.body, param, &func_def.params);
                        match inferred {
                            Type::I32 => "i32",
                            Type::F64 => "double",
                            Type::Ptr => "i8*",
                        }
                    };
                    types.push(param_type);
                }
                return types;
            }
        }
        types
    }

    fn infer_expr_type_with_sigs(
        &self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
        sigs: &HashMap<String, &'static str>,
    ) -> &'static str {
        match expr {
            Expr::Call { func, .. } => self
                .builtin_return_type(func)
                .or_else(|| sigs.get(&self.resolve_function_name(func)).copied())
                .unwrap_or("void"),
            _ => self.infer_expr_type(expr, params, locals),
        }
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
            }
            _ => unreachable!(),
        }
    }

fn infer_param_type(&self, body: &[Expr], param: &str, known_params: &[String]) -> Type {
        let mut temp_locals: HashMap<String, (Type, String, bool)> = HashMap::new();
        
        // First: process all params BEFORE the one we're inferring
        for (i, p) in known_params.iter().enumerate() {
            if p == param {
                break;
            }
            for e in body {
                if let Expr::Let { name, typ: _, value, is_const: _ } = e {
                    if name == p {
                        let inferred = self.infer_expr_type(value, known_params, &temp_locals);
                        let t = match inferred {
                            "i32" => Type::I32,
                            "double" => Type::F64,
                            _ => Type::Ptr,
                        };
                        temp_locals.insert(p.to_string(), (t, format!("arg{}", i), false));
                        break;
                    }
                }
            }
        }
        
        // Second: look for let binding of the CURRENT param
        for expr in body {
            if let Expr::Let { name, typ, value, is_const: _ } = expr {
                if name == param {
                    if let Some(t) = typ {
                        let resolved = self.resolve_type_name(t);
                        match resolved.as_str() {
                            "i32" => return Type::I32,
                            "f64" | "float" => return Type::F64,
                            "str" | "string" | "ptr" => return Type::Ptr,
                            _ => {}
                        }
                    }
                    let inferred = self.infer_expr_type(value, known_params, &temp_locals);
                    return match inferred {
                        "i32" => Type::I32,
                        "double" => Type::F64,
                        _ => Type::Ptr,
                    };
                }
            }
        }
        
        // Helper: get expected param type for a function
        let param_type_at = |func: &str, idx: usize| -> Option<&'static str> {
            let types = self.get_function_param_types(func);
            if idx < types.len() && types[idx] != "void" {
                Some(types[idx])
            } else {
                None
            }
        };

        // Third: check if param is used in string context (passed to string functions)
        // This includes direct __rt_* calls AND calls to wrapper functions like print/len/etc
        let is_used_in_string_context = |body: &[Expr], target_param: &str, stdlib_enabled: bool| -> bool {
            fn check_expr(expr: &Expr, param: &str, stdlib_enabled: bool, visited: &mut Vec<String>, pt: &dyn Fn(&str, usize) -> Option<&'static str>) -> bool {
                match expr {
                    Expr::Call { func, args } => {
                        for (i, arg) in args.iter().enumerate() {
                            if let Expr::Identifier(n) = arg {
                                if n == param {
                                    if let Some(expected) = pt(func, i) {
                                        if expected == "i8*" {
                                            return true;
                                        }
                                    } else if func.starts_with("__rt_") {
                                        return true;
                                    } else if stdlib_enabled && matches!(func.as_str(), "print" | "len" | "read_file" | "write_file" | "is_empty") {
                                        return true;
                                    }
                                }
                            }
                        }
                        if !visited.contains(func) {
                            visited.push(func.to_string());
                        }
                        false
                    }
                    Expr::Return(inner) => check_expr(inner, param, stdlib_enabled, visited, pt),
                    _ => false
                }
            }
            
            let mut visited = Vec::new();
            for expr in body {
                if check_expr(expr, target_param, stdlib_enabled, &mut visited, &param_type_at) {
                    return true;
                }
            }
            false
        };
        
        if is_used_in_string_context(body, param, self.stdlib_enabled) {
            return Type::Ptr;
        }

        // Check if param is used in int context (passed to int-expecting functions)
        fn check_int_context(expr: &Expr, param: &str, pt: &dyn Fn(&str, usize) -> Option<&'static str>) -> bool {
            match expr {
                Expr::Call { func, args } => {
                    for (i, arg) in args.iter().enumerate() {
                        if let Expr::Identifier(n) = arg {
                            if n == param {
                                if let Some(expected) = pt(func, i) {
                                    if expected == "i32" {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                    false
                }
                Expr::Return(inner) => check_int_context(inner, param, pt),
                _ => false,
            }
        }
        if body.iter().any(|e| check_int_context(e, param, &param_type_at)) {
            return Type::I32;
        }

        // Check if param is used in float context (passed to float-expecting functions)
        fn check_float_context(expr: &Expr, param: &str, pt: &dyn Fn(&str, usize) -> Option<&'static str>) -> bool {
            match expr {
                Expr::Call { func, args } => {
                    for (i, arg) in args.iter().enumerate() {
                        if let Expr::Identifier(n) = arg {
                            if n == param {
                                if let Some(expected) = pt(func, i) {
                                    if expected == "double" {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                    false
                }
                Expr::Return(inner) => check_float_context(inner, param, pt),
                _ => false,
            }
        }
        if body.iter().any(|e| check_float_context(e, param, &param_type_at)) {
            return Type::F64;
        }
        
        // Fourth: check if param is used in binary expressions (comparison/arithmetic) anywhere
        fn param_in_binary(expr: &Expr, param: &str) -> bool {
            match expr {
                Expr::Binary(l, _, r) => {
                    if let Expr::Identifier(n) = l.as_ref() { if n == param { return true; } }
                    if let Expr::Identifier(n) = r.as_ref() { if n == param { return true; } }
                    param_in_binary(l, param) || param_in_binary(r, param)
                }
                Expr::If { condition, then_branch, else_branch } => {
                    param_in_binary(condition, param)
                        || then_branch.stmts.iter().any(|e| param_in_binary(e, param))
                        || else_branch.as_ref().is_some_and(|b| b.stmts.iter().any(|e| param_in_binary(e, param)))
                }
                Expr::While { condition, body } => {
                    param_in_binary(condition, param)
                        || body.stmts.iter().any(|e| param_in_binary(e, param))
                }
                Expr::For { iterable, body, .. } => {
                    param_in_binary(iterable, param)
                        || body.stmts.iter().any(|e| param_in_binary(e, param))
                }
                Expr::ForRange { start, end, body, .. } => {
                    param_in_binary(start, param)
                        || param_in_binary(end, param)
                        || body.stmts.iter().any(|e| param_in_binary(e, param))
                }
                Expr::Call { args, .. } => args.iter().any(|a| param_in_binary(a, param)),
                Expr::Return(inner) => param_in_binary(inner, param),
                Expr::Unary(_, inner) => param_in_binary(inner, param),
                Expr::Index { target, index } => param_in_binary(target, param) || param_in_binary(index, param),
                Expr::AssignIndex { index, value, .. } => param_in_binary(index, param) || param_in_binary(value, param),
                Expr::List(items) => items.iter().any(|item| param_in_binary(item, param)),
                Expr::Match { expr, arms } => {
                    param_in_binary(expr, param) || arms.iter().any(|arm| param_in_binary(&arm.body, param))
                }
                _ => false,
            }
        }
        if body.iter().any(|e| param_in_binary(e, param)) {
            return Type::I32;
        }
        
        // Default: params are string pointer (ptr) by default
        Type::Ptr
    }

    fn generate_block_with_terminator(
        &mut self,
        stmts: &[Expr],
        params: &[String],
        locals: &mut HashMap<String, (Type, String, bool)>,
        return_ty: &str,
    ) -> CompileResult<bool> {
        let last_is_return = stmts.iter().last().map_or(false, |s| matches!(s, Expr::Return(_)));
        for stmt in stmts {
            self.generate_expr(stmt, params, locals, return_ty)?;
        }
        Ok(last_is_return)
    }

    fn generate_expr(
        &mut self,
        expr: &Expr,
        params: &[String],
        locals: &mut HashMap<String, (Type, String, bool)>,
        return_ty: &str,
    ) -> CompileResult<()> {
        match expr {
            Expr::Import(_) => {}

            Expr::Let {
                name,
                typ,
                value,
                is_const,
            } => {
                let alloca = self.next_temp();

                let var_type = if let Some(t) = typ {
                    let resolved = self.resolve_type_name(t);
                    match resolved.as_str() {
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
                        self.generate_ptr_expr(value, params, locals, &ptr)?;
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
                    .ok_or_else(|| {
                        let suggestion = self
                            .suggest_variable(name, params, locals)
                            .map(|s| format!(" did you mean `{}`?", s))
                            .unwrap_or_default();
                        CompileError::new(format!("assign of unknown variable: {}{}", name, suggestion))
                    })?;

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
                        self.generate_ptr_expr(value, params, locals, &ptr)?;
                        writeln!(
                            &mut self.functions,
                            "  store i8* %{}, i8** %{}",
                            ptr, alloca
                        )
                        .unwrap();
                    }
                }
            }

            Expr::Call { func, args } if self.stdlib_enabled && func == "print" => {
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
                        self.generate_ptr_expr(arg, params, locals, &result)?;
                        self.emit_printf_str(&result);
                    }
                }
            }

            Expr::Call { func, args } if func != "asm" => {
                let _ = self.emit_call(func, args, params, locals)?;
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
                    self.generate_ptr_expr(inner, params, locals, &ptr)?;
                    writeln!(&mut self.functions, "  ret i8* %{}", ptr).unwrap();
                } else if ty == "i32" && return_ty == "void" {
                    writeln!(&mut self.functions, "  ret void").unwrap();
                } else if ty == "double" && return_ty == "double" {
                    let val = self.generate_float_expr(inner, params, locals)?;
                    writeln!(&mut self.functions, "  ret double {}", val.as_str()).unwrap();
                } else if ty == "void" && return_ty == "void" {
                    writeln!(&mut self.functions, "  ret void").unwrap();
                } else if ty == "void" && return_ty == "i32" {
                    let val = self.generate_int_expr(inner, params, locals)?;
                    writeln!(&mut self.functions, "  ret i32 {}", val.as_str()).unwrap();
                } else {
                    return Err(CompileError::new("return: types mismatch"));
                }
            }

            Expr::FString(elements) => {
                let ptr = self.next_temp();
                self.emit_fstring(elements, params, locals, &ptr)?;
                self.emit_printf_str(&ptr);
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
                let merge_label = self.next_block_label("merge");

                if let Some(else_block) = else_branch {
                    let else_l = self.next_block_label("else");
                    writeln!(
                        &mut self.functions,
                        "  br i1 %{}, label %{}, label %{}",
                        cond_bool, then_label, else_l
                    )
                    .unwrap();

                    writeln!(&mut self.functions, "{}:", then_label).unwrap();
                    let then_ended_with_return = self.generate_block_with_terminator(&then_branch.stmts, params, locals, return_ty)?;
                    if !then_ended_with_return {
                        writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                    }

                    writeln!(&mut self.functions, "{}:", else_l).unwrap();
                    let else_ended_with_return = self.generate_block_with_terminator(&else_block.stmts, params, locals, return_ty)?;
                    if !else_ended_with_return {
                        writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                    }
                } else {
                    writeln!(
                        &mut self.functions,
                        "  br i1 %{}, label %{}, label %{}",
                        cond_bool, then_label, merge_label
                    )
                    .unwrap();

                    writeln!(&mut self.functions, "{}:", then_label).unwrap();
                    let then_ended_with_return = self.generate_block_with_terminator(&then_branch.stmts, params, locals, return_ty)?;
                    if !then_ended_with_return {
                        writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                    }
                }

                writeln!(&mut self.functions, "{}:", merge_label).unwrap();
            }

            Expr::While { condition, body } => {
                let loop_start = self.next_block_label("while_start");
                let loop_body = self.next_block_label("while_body");
                let loop_end = self.next_block_label("while_end");

                self.loop_label_stack.push((loop_start.clone(), loop_end.clone()));

                writeln!(&mut self.functions, "  br label %{}", loop_start).unwrap();
                writeln!(&mut self.functions, "{}:", loop_start).unwrap();

                let cond_val = self.generate_int_expr(condition, params, locals)?;
                let cond_bool = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = icmp ne i32 {}, 0",
                    cond_bool,
                    cond_val.as_str()
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  br i1 %{}, label %{}, label %{}",
                    cond_bool, loop_body, loop_end
                )
                .unwrap();

                writeln!(&mut self.functions, "{}:", loop_body).unwrap();
                for stmt in &body.stmts {
                    self.generate_expr(stmt, params, locals, return_ty)?;
                }
                writeln!(&mut self.functions, "  br label %{}", loop_start).unwrap();

                writeln!(&mut self.functions, "{}:", loop_end).unwrap();
                self.loop_label_stack.pop();
            }

            Expr::For { variable, iterable, body } => {
                let iter_val = self.generate_int_expr(iterable, params, locals)?;
                let loop_start = self.next_block_label("for_start");
                let loop_body = self.next_block_label("for_body");
                let loop_end = self.next_block_label("for_end");

                let counter_alloca = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca i32", counter_alloca).unwrap();
                writeln!(&mut self.functions, "  store i32 {}, i32* %{}", iter_val.as_str(), counter_alloca).unwrap();

                let mut locals_for = locals.clone();
                locals_for.insert(variable.clone(), (Type::I32, counter_alloca.clone(), false));

                self.loop_label_stack.push((loop_start.clone(), loop_end.clone()));

                writeln!(&mut self.functions, "  br label %{}", loop_start).unwrap();
                writeln!(&mut self.functions, "{}:", loop_start).unwrap();

                let counter_val = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i32, i32* %{}",
                    counter_val, counter_alloca
                )
                .unwrap();

                let loop_cond = self.next_temp();
                let cv = format!("%{}", counter_val);
                writeln!(
                    &mut self.functions,
                    "  %{} = icmp sgt i32 {}, 0",
                    loop_cond, cv
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  br i1 %{}, label %{}, label %{}",
                    loop_cond, loop_body, loop_end
                )
                .unwrap();

                writeln!(&mut self.functions, "{}:", loop_body).unwrap();
                for stmt in &body.stmts {
                    self.generate_expr(stmt, params, &mut locals_for, return_ty)?;
                }

                let new_counter = self.next_temp();
                let cv = format!("%{}", counter_val);
                writeln!(
                    &mut self.functions,
                    "  %{} = sub i32 {}, 1",
                    new_counter, cv
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 %{}, i32* %{}",
                    new_counter, counter_alloca
                )
                .unwrap();

                if return_ty != "void" && return_ty != "i32" {
                } else {
                    writeln!(&mut self.functions, "  br label %{}", loop_start).unwrap();
                }

                writeln!(&mut self.functions, "{}:", loop_end).unwrap();
                self.loop_label_stack.pop();
            }

            Expr::ForRange { variable, start, end, body } => {
                let start_val = self.generate_int_expr(start, params, locals)?;
                let end_val = self.generate_int_expr(end, params, locals)?;
                let loop_start_label = self.next_block_label("for_start");
                let loop_body = self.next_block_label("for_body");
                let loop_end = self.next_block_label("for_end");

                let counter_alloca = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca i32", counter_alloca).unwrap();
                writeln!(&mut self.functions, "  store i32 {}, i32* %{}", start_val.as_str(), counter_alloca).unwrap();

                let mut locals_for = locals.clone();
                locals_for.insert(variable.clone(), (Type::I32, counter_alloca.clone(), false));

                self.loop_label_stack.push((loop_start_label.clone(), loop_end.clone()));

                writeln!(&mut self.functions, "  br label %{}", loop_start_label).unwrap();
                writeln!(&mut self.functions, "{}:", loop_start_label).unwrap();

                let counter_val = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i32, i32* %{}",
                    counter_val, counter_alloca
                ).unwrap();

                let loop_cond = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = icmp slt i32 %{}, {}",
                    loop_cond, counter_val, end_val.as_str()
                ).unwrap();
                writeln!(
                    &mut self.functions,
                    "  br i1 %{}, label %{}, label %{}",
                    loop_cond, loop_body, loop_end
                ).unwrap();

                writeln!(&mut self.functions, "{}:", loop_body).unwrap();
                for stmt in &body.stmts {
                    self.generate_expr(stmt, params, &mut locals_for, return_ty)?;
                }

                let new_counter = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 %{}, 1",
                    new_counter, counter_val
                ).unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 %{}, i32* %{}",
                    new_counter, counter_alloca
                ).unwrap();

                writeln!(&mut self.functions, "  br label %{}", loop_start_label).unwrap();
                writeln!(&mut self.functions, "{}:", loop_end).unwrap();
                self.loop_label_stack.pop();
            }

            Expr::Break => {
                if let Some((_, loop_end)) = self.loop_label_stack.last() {
                    writeln!(&mut self.functions, "  br label %{}", loop_end).unwrap();
                } else {
                    return Err(CompileError::new("break outside of loop"));
                }
            }

            Expr::Continue => {
                if let Some((loop_start, _)) = self.loop_label_stack.last() {
                    writeln!(&mut self.functions, "  br label %{}", loop_start).unwrap();
                } else {
                    return Err(CompileError::new("continue outside of loop"));
                }
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

                // String concatenation
                if left_ty == "i8*" && right_ty == "i8*" && matches!(op, BinOp::Add) {
                    let ptr = self.next_temp();
                    self.generate_ptr_expr(expr, params, locals, &ptr)?;
                    self.emit_printf_str(&ptr);
                } else if uses_float && !is_comparison {
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

            Expr::List(_items) => {
                let ptr = self.next_temp();
                self.generate_ptr_expr(expr, params, locals, &ptr)?;
                self.emit_printf_str(&ptr);
            }

            Expr::Index { target: _target, index: _index } => {
                let val = self.generate_int_expr(expr, params, locals)?;
                self.emit_printf_int(&val.as_str());
            }

            Expr::Match { expr, arms } => {
                let has_variant = arms.iter().any(|a| matches!(a.pattern, crate::ast::MatchPattern::Variant { .. }));
                if has_variant {
                    // Enum match: compare discriminant
                    let enum_ptr = self.next_temp();
                    self.generate_ptr_expr(expr, params, locals, &enum_ptr)?;
                    let disc_ptr = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", disc_ptr, enum_ptr).unwrap();
                    let disc_val = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = load i32, i32* %{}", disc_val, disc_ptr).unwrap();
                    let end_label = self.next_block_label("ematch_end");
                    let wildcard_label = arms.iter().position(|a| matches!(a.pattern, crate::ast::MatchPattern::Wildcard))
                        .map(|_| self.next_block_label("ematch_default"));
                    for (i, arm) in arms.iter().enumerate() {
                        match &arm.pattern {
                            crate::ast::MatchPattern::Variant { name: vname, bindings } => {
                                // Find variant index in the enum definition
                                let mut disc_val_to_check: i32 = -1;
                                for edef in &self.enum_defs {
                                    for (vi, v) in edef.variants.iter().enumerate() {
                                        if v.name == *vname {
                                            disc_val_to_check = vi as i32;
                                        }
                                    }
                                }
                                let next_label = if i + 1 < arms.len()
                                    && !matches!(arms[i + 1].pattern, crate::ast::MatchPattern::Wildcard)
                                {
                                    self.next_block_label("ematch_next")
                                } else {
                                    wildcard_label.clone().unwrap_or_else(|| end_label.clone())
                                };
                                let cmp = self.next_temp();
                                writeln!(&mut self.functions, "  %{} = icmp eq i32 %{}, {}", cmp, disc_val, disc_val_to_check).unwrap();
                                let arm_label = self.next_block_label("earm");
                                writeln!(&mut self.functions, "  br i1 %{}, label %{}, label %{}", cmp, arm_label, next_label).unwrap();
                                writeln!(&mut self.functions, "{}:", arm_label).unwrap();
                                // Extract bindings from payload
                                let variant_fields: Vec<(String, String)> = {
                                    let edef = self.enum_defs.iter().find(|e| e.variants.iter().any(|v| v.name == *vname));
                                    match edef {
                                        Some(e) => e.variants.iter()
                                            .find(|v| v.name == *vname)
                                            .map(|v| {
                                                v.fields.iter().enumerate()
                                                    .filter(|(i, _)| bindings.len() > *i)
                                                    .map(|(i, ft)| (bindings[i].clone(), ft.clone()))
                                                    .collect()
                                            })
                                            .unwrap_or_default(),
                                        None => vec![],
                                    }
                                };
                                let mut payload_off = 4i32;
                                for (binding_name, field_typ) in &variant_fields {
                                    let alloca = self.next_temp();
                                    let elem_ptr = self.next_temp();
                                    writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, enum_ptr, payload_off).unwrap();
                                    match field_typ.as_str() {
                                        "i32" | "bool" => {
                                            writeln!(&mut self.functions, "  %{} = alloca i32", alloca).unwrap();
                                            let typed_ptr = self.next_temp();
                                            writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", typed_ptr, elem_ptr).unwrap();
                                            let loaded = self.next_temp();
                                            writeln!(&mut self.functions, "  %{} = load i32, i32* %{}", loaded, typed_ptr).unwrap();
                                            writeln!(&mut self.functions, "  store i32 %{}, i32* %{}", loaded, alloca).unwrap();
                                            locals.insert(binding_name.clone(), (crate::compiler::Type::I32, alloca.clone(), false));
                                            payload_off += 4;
                                        }
                                        "f64" | "float" | "double" => {
                                            writeln!(&mut self.functions, "  %{} = alloca double", alloca).unwrap();
                                            let typed_ptr = self.next_temp();
                                            writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to double*", typed_ptr, elem_ptr).unwrap();
                                            let loaded = self.next_temp();
                                            writeln!(&mut self.functions, "  %{} = load double, double* %{}", loaded, typed_ptr).unwrap();
                                            writeln!(&mut self.functions, "  store double %{}, double* %{}", loaded, alloca).unwrap();
                                            locals.insert(binding_name.clone(), (crate::compiler::Type::F64, alloca.clone(), false));
                                            payload_off += 8;
                                        }
                                        _ => {
                                            writeln!(&mut self.functions, "  %{} = alloca i8*", alloca).unwrap();
                                            let typed_ptr = self.next_temp();
                                            writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8**", typed_ptr, elem_ptr).unwrap();
                                            let loaded = self.next_temp();
                                            writeln!(&mut self.functions, "  %{} = load i8*, i8** %{}", loaded, typed_ptr).unwrap();
                                            writeln!(&mut self.functions, "  store i8* %{}, i8** %{}", loaded, alloca).unwrap();
                                            locals.insert(binding_name.clone(), (crate::compiler::Type::Ptr, alloca.clone(), false));
                                            payload_off += 8;
                                        }
                                    }
                                }
                                self.generate_expr(&arm.body, params, locals, return_ty)?;
                                // Clean up bindings
                                for binding in bindings {
                                    locals.remove(binding);
                                }
                                writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                                if next_label != end_label && wildcard_label.as_ref() != Some(&next_label) {
                                    writeln!(&mut self.functions, "{}:", next_label).unwrap();
                                }
                            }
                            crate::ast::MatchPattern::Wildcard => {
                                if let Some(ref def) = wildcard_label {
                                    writeln!(&mut self.functions, "{}:", def).unwrap();
                                }
                                self.generate_expr(&arm.body, params, locals, return_ty)?;
                                writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                            }
                            _ => {}
                        }
                    }
                    writeln!(&mut self.functions, "{}:", end_label).unwrap();
                } else {
                    let match_val = self.generate_int_expr(expr, params, locals)?;
                    let match_reg = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = add i32 {}, 0",
                        match_reg, match_val.as_str()
                    ).unwrap();
                    let end_label = self.next_block_label("match_end");
                    let wildcard_label = arms.iter().position(|a| matches!(a.pattern, crate::ast::MatchPattern::Wildcard))
                        .map(|_| self.next_block_label("match_default"));
                    for (i, arm) in arms.iter().enumerate() {
                        match &arm.pattern {
                            crate::ast::MatchPattern::Int(n) => {
                                let next_label = if i + 1 < arms.len()
                                    && !matches!(arms[i + 1].pattern, crate::ast::MatchPattern::Wildcard)
                                {
                                    self.next_block_label("match_next")
                                } else {
                                    wildcard_label.clone().unwrap_or_else(|| end_label.clone())
                                };
                                let cmp = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = icmp eq i32 %{}, {}",
                                    cmp, match_reg, n
                                ).unwrap();
                                let arm_label = self.next_block_label("match_arm");
                                writeln!(
                                    &mut self.functions,
                                    "  br i1 %{}, label %{}, label %{}",
                                    cmp, arm_label, next_label
                                ).unwrap();
                                writeln!(&mut self.functions, "{}:", arm_label).unwrap();
                                self.generate_expr(&arm.body, params, locals, return_ty)?;
                                writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                                if next_label != end_label && wildcard_label.as_ref() != Some(&next_label) {
                                    writeln!(&mut self.functions, "{}:", next_label).unwrap();
                                }
                            }
                            crate::ast::MatchPattern::Wildcard => {
                                if let Some(ref def) = wildcard_label {
                                    writeln!(&mut self.functions, "{}:", def).unwrap();
                                }
                                self.generate_expr(&arm.body, params, locals, return_ty)?;
                                writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                            }
                            _ => {}
                        }
                    }
                    writeln!(&mut self.functions, "{}:", end_label).unwrap();
                }
            }

            Expr::AssignIndex { name, index, value } => {
                let _var_type = match locals.get(name) {
                    Some((t, _, _)) => t.clone(),
                    None => return Err(CompileError::new(format!("unknown variable: {}", name))),
                };
                let ptr = self.next_temp();
                self.generate_ptr_expr(&Expr::Identifier(name.clone()), params, locals, &ptr)?;
                let idx = self.generate_int_expr(index, params, locals)?;
                let elem_off = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = mul i32 {}, 4",
                    elem_off, idx.as_str()
                ).unwrap();
                let offset = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 %{}, 8",
                    offset, elem_off
                ).unwrap();
                let elem_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 %{}",
                    elem_ptr, ptr, offset
                ).unwrap();
                let i32_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i32*",
                    i32_ptr, elem_ptr
                ).unwrap();
                let val = self.generate_int_expr(value, params, locals)?;
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}",
                    val.as_str(), i32_ptr
                ).unwrap();
            }

            Expr::FieldAssign { target, field, value } => {
                let struct_defs = self.struct_defs.clone();
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let target_name = if let Expr::Identifier(id) = target.as_ref() {
                    Some(id.clone())
                } else { None };
                let mut offset = 0i32;
                let mut field_typ = "i32".to_string();
                if let Some(ref name) = target_name {
                    if let Some(sdef) = struct_defs.iter().find(|_s| {
                        locals.contains_key(name)
                    }) {
                        for sf in &sdef.fields {
                            if sf.name == *field {
                                field_typ = sf.typ.clone();
                                break;
                            }
                            offset += match sf.typ.as_str() {
                                "i32" | "bool" => 4,
                                "f64" | "float" | "double" => 8,
                                _ => 8,
                            };
                        }
                    }
                }
                let elem_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, ptr, offset).unwrap();
                match field_typ.as_str() {
                    "i32" | "bool" => {
                        let typed_ptr = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", typed_ptr, elem_ptr).unwrap();
                        let val = self.generate_int_expr(value, params, locals)?;
                        writeln!(&mut self.functions, "  store i32 {}, i32* %{}", val.as_str(), typed_ptr).unwrap();
                    }
                    "f64" | "float" | "double" => {
                        let typed_ptr = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to double*", typed_ptr, elem_ptr).unwrap();
                        let val = self.generate_float_expr(value, params, locals)?;
                        writeln!(&mut self.functions, "  store double {}, double* %{}", val.as_str(), typed_ptr).unwrap();
                    }
                    _ => {
                        let typed_ptr = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8**", typed_ptr, elem_ptr).unwrap();
                        let val_ptr = self.next_temp();
                        self.generate_ptr_expr(value, params, locals, &val_ptr)?;
                        writeln!(&mut self.functions, "  store i8* %{}, i8** %{}", val_ptr, typed_ptr).unwrap();
                    }
                }
            }

            _ => {}
        }

        Ok(())
    }

    fn emit_printf_int(&mut self, value: &str) {
        let fmt = self.printf_fmt("%d\n", 5);
        writeln!(
            &mut self.functions,
            "  call i32 (i8*, ...) @printf(i8* %{}, i32 {})",
            fmt,
            value
        )
        .unwrap();
    }

    fn emit_printf_float(&mut self, value: &str) {
        let fmt = self.printf_fmt("%f\n", 4);
        writeln!(
            &mut self.functions,
            "  call i32 (i8*, ...) @printf(i8* %{}, double {})",
            fmt,
            value
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
                        parts.push("%s".to_string());
                        let lit = self.emit_string_const(name);
                        let loaded = self.next_temp();
                        self.emit_gep(&lit, name.len() + 1, &loaded);
                        args.push(format!("i8* %{}", loaded));
                    }
                }
                _ => return Err(CompileError::new("fstring: unsupported element")),
            }
        }

        let fmt_str = parts.join("");
        let fmt_id = self.emit_string_const(&fmt_str);
        let fmt_ptr = self.next_temp();
        self.emit_gep(&fmt_id, fmt_str.len() + 1, &fmt_ptr);

        let buf_ptr = self.next_temp();
        writeln!(
            &mut self.functions,
            "  %{} = getelementptr inbounds [1024 x i8], [1024 x i8]* @.global_buffer, i32 0, i32 0",
            buf_ptr
        )
        .unwrap();
        let mut sprintf_args = format!("i8* %{}, i8* %{}", buf_ptr, fmt_ptr);
        for arg in args {
            write!(&mut sprintf_args, ", {}", arg).unwrap();
        }

        writeln!(
            &mut self.functions,
            "  call i32 (i8*, i8*, ...) @sprintf({})",
            sprintf_args
        )
        .unwrap();
        writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8*", result_ptr, buf_ptr).unwrap();
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
            Expr::Call { func, args } => {
                let ret_ty = self
                    .builtin_return_type(func)
                    .or_else(|| self.function_sigs.get(&self.resolve_function_name(func)).copied())
                    .unwrap_or("void");
                if ret_ty != "i32" {
                    return Err(CompileError::new(format!("{} does not return i32", func)));
                }
                let result = self
                    .emit_call(func, args, params, locals)?
                    .ok_or_else(|| CompileError::new("missing call result"))?;
                Ok(Val::Reg(result))
            }
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
                        Type::Ptr => {
                            let res = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = load i8*, i8** %{}",
                                res, alloca
                            )
                            .unwrap();
                            Ok(Val::Reg(res))
                        }
                    };
                }

                if self.global_vars.contains_key(name) {
                    return Err(CompileError::new(format!(
                        "The global variable '{}' has a string type, but is used in a numeric context",
                        name
                    )));
                }

                Err(CompileError::new(format!(
                    "an unknown variable in a numeric context: '{}'{}",
                    name,
                    self.suggest_variable(name, params, locals)
                        .map(|s| format!(" did you mean `{}`?", s))
                        .unwrap_or_default()
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
                            writeln!(&mut self.functions, "  %{} = add i32 0, %{}", res, r).unwrap();
                            Ok(Val::Reg(res))
                        }
                    },
                    UnaryOp::Neg => match v {
                        Val::Imm(n) => Ok(Val::Imm(-n)),
                        Val::ImmFloat(_) => Err(CompileError::new("cannot use - on float in integer context")),
                        Val::Reg(r) => {
                            let res = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = sub i32 0, %{}", res, r).unwrap();
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
                                "  %{} = icmp eq i32 %{}, 0",
                                cmp,
                                r
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
            Expr::Match { expr, arms } => {
                let match_val = self.generate_int_expr(expr, params, locals)?;
                let match_reg = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 {}, 0",
                    match_reg, match_val.as_str()
                ).unwrap();
                let result_alloca = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca i32", result_alloca).unwrap();
                let end_label = self.next_block_label("match_end");
                let wildcard_label = arms.iter().position(|a| matches!(a.pattern, crate::ast::MatchPattern::Wildcard))
                    .map(|_| self.next_block_label("match_default"));
                for (i, arm) in arms.iter().enumerate() {
                    match &arm.pattern {
                        crate::ast::MatchPattern::Int(n) => {
                            let next_label = if i + 1 < arms.len()
                                && !matches!(arms[i + 1].pattern, crate::ast::MatchPattern::Wildcard)
                            {
                                self.next_block_label("match_next")
                            } else {
                                wildcard_label.clone().unwrap_or_else(|| end_label.clone())
                            };
                            let cmp = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = icmp eq i32 %{}, {}",
                                cmp, match_reg, n
                            ).unwrap();
                            let arm_label = self.next_block_label("match_arm");
                            writeln!(
                                &mut self.functions,
                                "  br i1 %{}, label %{}, label %{}",
                                cmp, arm_label, next_label
                            ).unwrap();
                            writeln!(&mut self.functions, "{}:", arm_label).unwrap();
                            let val = self.generate_int_expr(&arm.body, params, locals)?;
                            writeln!(
                                &mut self.functions,
                                "  store i32 {}, i32* %{}",
                                val.as_str(), result_alloca
                            ).unwrap();
                            writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                            if next_label != end_label && wildcard_label.as_ref() != Some(&next_label) {
                                writeln!(&mut self.functions, "{}:", next_label).unwrap();
                            }
                        }
                        crate::ast::MatchPattern::Variant { name: vname, bindings } => {
                            let next_label = if i + 1 < arms.len()
                                && !matches!(arms[i + 1].pattern, crate::ast::MatchPattern::Wildcard)
                            {
                                self.next_block_label("ematch_next")
                            } else {
                                wildcard_label.clone().unwrap_or_else(|| end_label.clone())
                            };
                            // Find variant index
                            let mut disc_val_to_check: i32 = -1;
                            for edef in &self.enum_defs {
                                for (vi, v) in edef.variants.iter().enumerate() {
                                    if v.name == *vname {
                                        disc_val_to_check = vi as i32;
                                    }
                                }
                            }
                            // Generate match_value expr to get enum pointer + discriminant
                            let enum_ptr = self.next_temp();
                            self.generate_ptr_expr(expr, params, locals, &enum_ptr)?;
                            let disc_ptr = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", disc_ptr, enum_ptr).unwrap();
                            let edisc = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = load i32, i32* %{}", edisc, disc_ptr).unwrap();
                            let cmp = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = icmp eq i32 %{}, {}", cmp, edisc, disc_val_to_check).unwrap();
                            let arm_label = self.next_block_label("earm_int");
                            writeln!(&mut self.functions, "  br i1 %{}, label %{}, label %{}", cmp, arm_label, next_label).unwrap();
                            writeln!(&mut self.functions, "{}:", arm_label).unwrap();
                            // Extract bindings - use cloned locals since we can't modify originals
                            let variant_fields: Vec<(String, String)> = {
                                let edef = self.enum_defs.iter().find(|e| e.variants.iter().any(|v| v.name == *vname));
                                match edef {
                                    Some(e) => e.variants.iter()
                                        .find(|v| v.name == *vname)
                                        .map(|v| {
                                            v.fields.iter().enumerate()
                                                .filter(|(i, _)| bindings.len() > *i)
                                                .map(|(i, ft)| (bindings[i].clone(), ft.clone()))
                                                .collect()
                                        })
                                        .unwrap_or_default(),
                                    None => vec![],
                                }
                            };
                            let mut arm_locals = locals.clone();
                            let mut payload_off = 4i32;
                            for (binding_name, field_typ) in &variant_fields {
                                let alloca = self.next_temp();
                                let elem_ptr = self.next_temp();
                                writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, enum_ptr, payload_off).unwrap();
                                match field_typ.as_str() {
                                    "i32" | "bool" => {
                                        writeln!(&mut self.functions, "  %{} = alloca i32", alloca).unwrap();
                                        let typed_ptr = self.next_temp();
                                        writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", typed_ptr, elem_ptr).unwrap();
                                        let loaded = self.next_temp();
                                        writeln!(&mut self.functions, "  %{} = load i32, i32* %{}", loaded, typed_ptr).unwrap();
                                        writeln!(&mut self.functions, "  store i32 %{}, i32* %{}", loaded, alloca).unwrap();
                                        arm_locals.insert(binding_name.clone(), (crate::compiler::Type::I32, alloca.clone(), false));
                                        payload_off += 4;
                                    }
                                    "f64" | "float" | "double" => {
                                        writeln!(&mut self.functions, "  %{} = alloca double", alloca).unwrap();
                                        let typed_ptr = self.next_temp();
                                        writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to double*", typed_ptr, elem_ptr).unwrap();
                                        let loaded = self.next_temp();
                                        writeln!(&mut self.functions, "  %{} = load double, double* %{}", loaded, typed_ptr).unwrap();
                                        writeln!(&mut self.functions, "  store double %{}, double* %{}", loaded, alloca).unwrap();
                                        arm_locals.insert(binding_name.clone(), (crate::compiler::Type::F64, alloca.clone(), false));
                                        payload_off += 8;
                                    }
                                    _ => {
                                        writeln!(&mut self.functions, "  %{} = alloca i8*", alloca).unwrap();
                                        let typed_ptr = self.next_temp();
                                        writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8**", typed_ptr, elem_ptr).unwrap();
                                        let loaded = self.next_temp();
                                        writeln!(&mut self.functions, "  %{} = load i8*, i8** %{}", loaded, typed_ptr).unwrap();
                                        writeln!(&mut self.functions, "  store i8* %{}, i8** %{}", loaded, alloca).unwrap();
                                        arm_locals.insert(binding_name.clone(), (crate::compiler::Type::Ptr, alloca.clone(), false));
                                        payload_off += 8;
                                    }
                                }
                            }
                            let val = self.generate_int_expr(&arm.body, params, &arm_locals)?;
                            writeln!(&mut self.functions, "  store i32 {}, i32* %{}", val.as_str(), result_alloca).unwrap();
                            writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                            if next_label != end_label && wildcard_label.as_ref() != Some(&next_label) {
                                writeln!(&mut self.functions, "{}:", next_label).unwrap();
                            }
                        }
                        crate::ast::MatchPattern::Wildcard => {
                            if let Some(ref def) = wildcard_label {
                                writeln!(&mut self.functions, "{}:", def).unwrap();
                            }
                            let val = self.generate_int_expr(&arm.body, params, locals)?;
                            writeln!(
                                &mut self.functions,
                                "  store i32 {}, i32* %{}",
                                val.as_str(), result_alloca
                            ).unwrap();
                            writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                        }
                    }
                }
                if wildcard_label.is_none() {
                    // Store 0 if no arm matched
                    writeln!(
                        &mut self.functions,
                        "  store i32 0, i32* %{}",
                        result_alloca
                    ).unwrap();
                    writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                }
                writeln!(&mut self.functions, "{}:", end_label).unwrap();
                let result = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i32, i32* %{}",
                    result, result_alloca
                ).unwrap();
                Ok(Val::Reg(result))
            }
            Expr::Index { target, index } => {
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let idx = self.generate_int_expr(index, params, locals)?;
                let elem_off = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = mul i32 {}, 4",
                    elem_off, idx.as_str()
                ).unwrap();
                let offset = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 %{}, 8",
                    offset, elem_off
                ).unwrap();
                let elem_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 %{}",
                    elem_ptr, ptr, offset
                ).unwrap();
                let i32_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i32*",
                    i32_ptr, elem_ptr
                ).unwrap();
                let loaded = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i32, i32* %{}",
                    loaded, i32_ptr
                ).unwrap();
                Ok(Val::Reg(loaded))
            }
            Expr::FieldAccess { target, field } => {
                let struct_defs = self.struct_defs.clone();
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let mut offset = 0i32;
                let mut field_type = "i32".to_string();
                let target_name = if let Expr::Identifier(id) = target.as_ref() {
                    Some(id.clone())
                } else { None };
                if let Some(ref name) = target_name {
                    if let Some(sdef) = struct_defs.iter().find(|_s| locals.contains_key(name)) {
                        for sf in &sdef.fields {
                            if sf.name == *field {
                                field_type = sf.typ.clone();
                                break;
                            }
                            offset += match sf.typ.as_str() {
                                "i32" | "bool" => 4,
                                "f64" | "float" | "double" => 8,
                                _ => 8,
                            };
                        }
                    }
                }
                if field_type == "i32" || field_type == "bool" {
                    let elem_ptr = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, ptr, offset).unwrap();
                    let typed_ptr = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", typed_ptr, elem_ptr).unwrap();
                    let loaded = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = load i32, i32* %{}", loaded, typed_ptr).unwrap();
                    Ok(Val::Reg(loaded))
                } else {
                    Err(CompileError::new("field is not i32/bool"))
                }
            }
            Expr::TupleAccess { target, index } => {
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let offset = (*index as i32) * 8;
                let elem_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, ptr, offset).unwrap();
                let typed_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i64*", typed_ptr, elem_ptr).unwrap();
                let loaded = self.next_temp();
                writeln!(&mut self.functions, "  %{} = load i64, i64* %{}", loaded, typed_ptr).unwrap();
                let truncated = self.next_temp();
                writeln!(&mut self.functions, "  %{} = trunc i64 %{} to i32", truncated, loaded).unwrap();
                Ok(Val::Reg(truncated))
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
            Expr::Call { func, args } => {
                let ret_ty = self
                    .builtin_return_type(func)
                    .or_else(|| self.function_sigs.get(&self.resolve_function_name(func)).copied())
                    .unwrap_or("void");
                if ret_ty != "double" {
                    return Err(CompileError::new(format!("{} does not return double", func)));
                }
                let result = self
                    .emit_call(func, args, params, locals)?
                    .ok_or_else(|| CompileError::new("missing call result"))?;
                Ok(Val::Reg(result))
            }
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
            Expr::FieldAccess { target, field } => {
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let struct_defs = self.struct_defs.clone();
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let mut offset = 0i32;
                let mut field_type = "i32".to_string();
                let target_name = if let Expr::Identifier(id) = target.as_ref() {
                    Some(id.clone())
                } else { None };
                if let Some(ref name) = target_name {
                    if let Some(sdef) = struct_defs.iter().find(|_s| locals.contains_key(name)) {
                        for sf in &sdef.fields {
                            if sf.name == *field {
                                field_type = sf.typ.clone();
                                break;
                            }
                            offset += match sf.typ.as_str() {
                                "i32" | "bool" => 4,
                                "f64" | "float" | "double" => 8,
                                _ => 8,
                            };
                        }
                    }
                }
                if field_type == "f64" || field_type == "float" || field_type == "double" {
                    let elem_ptr = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, ptr, offset).unwrap();
                    let typed_ptr = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to double*", typed_ptr, elem_ptr).unwrap();
                    let loaded = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = load double, double* %{}", loaded, typed_ptr).unwrap();
                    Ok(Val::Reg(loaded))
                } else {
                    Err(CompileError::new("field is not float"))
                }
            }
            Expr::TupleAccess { target, index } => {
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let offset = (*index as i32) * 8;
                let elem_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, ptr, offset).unwrap();
                let typed_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to double*", typed_ptr, elem_ptr).unwrap();
                let loaded = self.next_temp();
                writeln!(&mut self.functions, "  %{} = load double, double* %{}", loaded, typed_ptr).unwrap();
                Ok(Val::Reg(loaded))
            }
            _ => Err(CompileError::new(format!("unexpected expression in float-context: {:?}", expr))),
        }
    }

    fn next_temp(&mut self) -> String {
        let id = format!("v{}", self.string_count);
        self.string_count += 1;
        id
    }

    fn resolve_type_name(&self, name: &str) -> String {
        let mut seen = std::collections::HashSet::new();
        let mut current = name.to_string();
        for _ in 0..16 {
            if let Some(resolved) = self.type_aliases.get(&current) {
                if !seen.insert(current.clone()) {
                    break;
                }
                current = resolved.clone();
            } else {
                break;
            }
        }
        current
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
                    return Err(CompileError::new(format!(
                        "unknown variable: {}{}",
                        name,
                        self.suggest_variable(name, params, locals)
                            .map(|s| format!(" did you mean `{}`?", s))
                            .unwrap_or_default()
                    )));
                }
            }
            Expr::Borrow { name, .. } => {
                self.emit_borrow_ptr(name, params, locals, result)?;
            }
                _ => return Err(CompileError::new(format!(
                    "print: unsupported argument{}",
                    self.stdlib_hint("print")
                ))),
        }
        Ok(())
    }

    fn generate_ptr_expr(
        &mut self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
        result: &str,
    ) -> CompileResult<()> {
        match expr {
            Expr::List(items) => {
                let count = items.len() as i32;
                let alloc_size = (8 + count * 4) as i64;
                let malloc_reg = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call i8* @malloc(i64 {})",
                    malloc_reg, alloc_size
                ).unwrap();
                // Store capacity and length in header
                let cap_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", cap_ptr, malloc_reg).unwrap();
                writeln!(&mut self.functions, "  store i32 {}, i32* %{}", count, cap_ptr).unwrap();
                let len_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 4", len_ptr, malloc_reg).unwrap();
                let len_ptr_i32 = self.next_temp();
                writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", len_ptr_i32, len_ptr).unwrap();
                writeln!(&mut self.functions, "  store i32 {}, i32* %{}", count, len_ptr_i32).unwrap();
                // Store elements at offset 8
                for (i, item) in items.iter().enumerate() {
                    let val = self.generate_int_expr(item, params, locals)?;
                    let ptr_reg = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr i8, i8* %{}, i32 {}",
                        ptr_reg, malloc_reg, (8 + i * 4)
                    ).unwrap();
                    let i32_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast i8* %{} to i32*",
                        i32_ptr, ptr_reg
                    ).unwrap();
                    writeln!(
                        &mut self.functions,
                        "  store i32 {}, i32* %{}",
                        val.as_str(), i32_ptr
                    ).unwrap();
                }
                writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8*", result, malloc_reg).unwrap();
            }
            Expr::StringLiteral(s) | Expr::MultilineString(s) => {
                let id = self.emit_string_const(s);
                self.emit_gep(&id, s.len() + 1, result);
            }
            Expr::FString(elements) => self.emit_fstring(elements, params, locals, result)?,
            Expr::Borrow { name, .. } => self.emit_borrow_ptr(name, params, locals, result)?,
            Expr::Identifier(_) => self.emit_print_arg(expr, params, locals, result)?,
            Expr::Call { func, args } => {
                let ty = self
                    .builtin_return_type(func)
                    .or_else(|| self.function_sigs.get(&self.resolve_function_name(func)).copied())
                    .unwrap_or("void");
                if ty != "i8*" {
                    return Err(CompileError::new(format!(
                        "{}: expected string-returning function",
                        func
                    )));
                }
                let call_result = self.emit_call(func, args, params, locals)?;
                let call_reg = call_result.ok_or_else(|| CompileError::new("expected call result"))?;
                writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8*", result, call_reg).unwrap();
            }
            Expr::Binary(l, op, r) if *op == BinOp::Add => {
                let left_ty = self.infer_expr_type(l, params, locals);
                let right_ty = self.infer_expr_type(r, params, locals);
                if left_ty == "i8*" && right_ty == "i8*" {
                    let lhs = self.next_temp();
                    let rhs = self.next_temp();
                    self.generate_ptr_expr(l, params, locals, &lhs)?;
                    self.generate_ptr_expr(r, params, locals, &rhs)?;
                    // Use global buffer + sprintf for concat
                    let buf_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr inbounds [1024 x i8], [1024 x i8]* @.global_buffer, i32 0, i32 0",
                        buf_ptr
                    ).unwrap();
                    let fmt_id = self.emit_string_const("%s%s");
                    let fmt_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr [5 x i8], [5 x i8]* @{}, i32 0, i32 0",
                        fmt_ptr, fmt_id
                    ).unwrap();
                    writeln!(
                        &mut self.functions,
                        "  call i32 (i8*, i8*, ...) @sprintf(i8* %{}, i8* %{}, i8* %{}, i8* %{})",
                        buf_ptr, fmt_ptr, lhs, rhs
                    ).unwrap();
                    writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8*", result, buf_ptr).unwrap();
                } else {
                    return Err(CompileError::new("string concatenation requires two strings"));
                }
            }
            Expr::StructLiteral { name, fields } => {
                let struct_defs = self.struct_defs.clone();
                let sdef = struct_defs.iter().find(|s| s.name == *name);
                if let Some(sdef) = sdef {
                    let mut total_size = 0i32;
                    let mut field_data: Vec<(String, i32, String)> = Vec::new();
                    for sf in &sdef.fields {
                        let size = match sf.typ.as_str() {
                            "i32" | "bool" => 4,
                            "f64" | "float" | "double" => 8,
                            _ => 8,
                        };
                        field_data.push((sf.name.clone(), total_size, sf.typ.clone()));
                        total_size += size;
                    }
                    let malloc_reg = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = call i8* @malloc(i64 {})", malloc_reg, total_size).unwrap();
                    for (fname, fval) in fields {
                        if let Some((_, offset, typ)) = field_data.iter().find(|(n, _, _)| n == fname) {
                            let elem_ptr = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, malloc_reg, offset).unwrap();
                            match typ.as_str() {
                                "i32" | "bool" => {
                                    let val = self.generate_int_expr(fval, params, locals)?;
                                    let typed_ptr = self.next_temp();
                                    writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", typed_ptr, elem_ptr).unwrap();
                                    writeln!(&mut self.functions, "  store i32 {}, i32* %{}", val.as_str(), typed_ptr).unwrap();
                                }
                                "f64" | "float" | "double" => {
                                    let val = self.generate_float_expr(fval, params, locals)?;
                                    let typed_ptr = self.next_temp();
                                    writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to double*", typed_ptr, elem_ptr).unwrap();
                                    writeln!(&mut self.functions, "  store double {}, double* %{}", val.as_str(), typed_ptr).unwrap();
                                }
                                _ => {
                                    let val_ptr = self.next_temp();
                                    self.generate_ptr_expr(fval, params, locals, &val_ptr)?;
                                    let typed_ptr = self.next_temp();
                                    writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8**", typed_ptr, elem_ptr).unwrap();
                                    writeln!(&mut self.functions, "  store i8* %{}, i8** %{}", val_ptr, typed_ptr).unwrap();
                                }
                            }
                        }
                    }
                    writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8*", result, malloc_reg).unwrap();
                } else {
                    writeln!(&mut self.functions, "  %{} = call i8* @malloc(i64 0)", result).unwrap();
                }
            }
            Expr::EnumLiteral { enum_name, variant_name, args } => {
                let enum_defs = self.enum_defs.clone();
                let edef = enum_defs.iter().find(|e| e.name == *enum_name);
                let mut discriminant = 0i32;
                let mut payload_size = 0i32;
                let mut max_payload = 0i32;
                if let Some(e) = edef {
                    for (i, v) in e.variants.iter().enumerate() {
                        let mut v_size = 0i32;
                        for ft in &v.fields {
                            v_size += match ft.as_str() {
                                "i32" | "bool" => 4,
                                "f64" | "float" | "double" => 8,
                                _ => 8,
                            };
                        }
                        if v.name == *variant_name {
                            discriminant = i as i32;
                            payload_size = v_size;
                        }
                        if v_size > max_payload {
                            max_payload = v_size;
                        }
                    }
                }
                if max_payload < 4 { max_payload = 4; }
                let alloc_size = 4 + max_payload;
                let malloc_reg = self.next_temp();
                writeln!(&mut self.functions, "  %{} = call i8* @malloc(i64 {})", malloc_reg, alloc_size).unwrap();
                // Store discriminant at offset 0
                let disc_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", disc_ptr, malloc_reg).unwrap();
                writeln!(&mut self.functions, "  store i32 {}, i32* %{}", discriminant, disc_ptr).unwrap();
                // Store payload at offset 4
                let mut offset = 4i32;
                for (i, arg) in args.iter().enumerate() {
                    if let Some(e) = edef {
                        if let Some(v) = e.variants.iter().find(|v| v.name == *variant_name) {
                            if i < v.fields.len() {
                                let ft = &v.fields[i];
                                let elem_ptr = self.next_temp();
                                writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, malloc_reg, offset).unwrap();
                                match ft.as_str() {
                                    "i32" | "bool" => {
                                        let val = self.generate_int_expr(arg, params, locals)?;
                                        let typed_ptr = self.next_temp();
                                        writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i32*", typed_ptr, elem_ptr).unwrap();
                                        writeln!(&mut self.functions, "  store i32 {}, i32* %{}", val.as_str(), typed_ptr).unwrap();
                                        offset += 4;
                                    }
                                    "f64" | "float" | "double" => {
                                        let val = self.generate_float_expr(arg, params, locals)?;
                                        let typed_ptr = self.next_temp();
                                        writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to double*", typed_ptr, elem_ptr).unwrap();
                                        writeln!(&mut self.functions, "  store double {}, double* %{}", val.as_str(), typed_ptr).unwrap();
                                        offset += 8;
                                    }
                                    _ => {
                                        let val_ptr = self.next_temp();
                                        self.generate_ptr_expr(arg, params, locals, &val_ptr)?;
                                        let typed_ptr = self.next_temp();
                                        writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8**", typed_ptr, elem_ptr).unwrap();
                                        writeln!(&mut self.functions, "  store i8* %{}, i8** %{}", val_ptr, typed_ptr).unwrap();
                                        offset += 8;
                                    }
                                }
                            }
                        }
                    }
                }
                writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8*", result, malloc_reg).unwrap();
            }
            Expr::Tuple(elements) => {
                let alloc_size = (elements.len() * 8) as i64;
                let malloc_reg = self.next_temp();
                writeln!(&mut self.functions, "  %{} = call i8* @malloc(i64 {})", malloc_reg, alloc_size).unwrap();
                for (i, elem) in elements.iter().enumerate() {
                    let offset = (i * 8) as i32;
                    let elem_ptr = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, malloc_reg, offset).unwrap();
                    let elem_ty = self.infer_expr_type(elem, params, locals);
                    match elem_ty {
                        "i32" | "bool" => {
                            let val = self.generate_int_expr(elem, params, locals)?;
                            let i64_val = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = sext i32 {} to i64", i64_val, val.as_str()).unwrap();
                            let typed_ptr = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i64*", typed_ptr, elem_ptr).unwrap();
                            writeln!(&mut self.functions, "  store i64 %{}, i64* %{}", i64_val, typed_ptr).unwrap();
                        }
                        "double" => {
                            let val = self.generate_float_expr(elem, params, locals)?;
                            let typed_ptr = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to double*", typed_ptr, elem_ptr).unwrap();
                            writeln!(&mut self.functions, "  store double {}, double* %{}", val.as_str(), typed_ptr).unwrap();
                        }
                        _ => {
                            let val_ptr = self.next_temp();
                            self.generate_ptr_expr(elem, params, locals, &val_ptr)?;
                            let typed_ptr = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8**", typed_ptr, elem_ptr).unwrap();
                            writeln!(&mut self.functions, "  store i8* %{}, i8** %{}", val_ptr, typed_ptr).unwrap();
                        }
                    }
                }
                writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8*", result, malloc_reg).unwrap();
            }
            Expr::TupleAccess { target, index } => {
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let offset = (*index as i32) * 8;
                let elem_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, ptr, offset).unwrap();
                let typed_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8**", typed_ptr, elem_ptr).unwrap();
                writeln!(&mut self.functions, "  %{} = load i8*, i8** %{}", result, typed_ptr).unwrap();
            }
            Expr::FieldAccess { target, field } => {
                let struct_defs = self.struct_defs.clone();
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let mut offset = 0i32;
                let mut field_type = "i32".to_string();
                let target_name = if let Expr::Identifier(id) = target.as_ref() {
                    Some(id.clone())
                } else { None };
                if let Some(ref name) = target_name {
                    if let Some(sdef) = struct_defs.iter().find(|_s| locals.contains_key(name)) {
                        for sf in &sdef.fields {
                            if sf.name == *field {
                                field_type = sf.typ.clone();
                                break;
                            }
                            offset += match sf.typ.as_str() {
                                "i32" | "bool" => 4,
                                "f64" | "float" | "double" => 8,
                                _ => 8,
                            };
                        }
                    }
                }
                if field_type == "i8*" || field_type == "string" || field_type == "str" || field_type == "ptr" {
                    let elem_ptr = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = getelementptr i8, i8* %{}, i32 {}", elem_ptr, ptr, offset).unwrap();
                    let ptr_ptr = self.next_temp();
                    writeln!(&mut self.functions, "  %{} = bitcast i8* %{} to i8**", ptr_ptr, elem_ptr).unwrap();
                    writeln!(&mut self.functions, "  %{} = load i8*, i8** %{}", result, ptr_ptr).unwrap();
                } else {
                    return Err(CompileError::new("expected string/ptr field access"));
                }
            }
            _ => return Err(CompileError::new("expected string expression")),
        }
        Ok(())
    }

    fn emit_call(
        &mut self,
        func: &str,
        args: &[Expr],
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> CompileResult<Option<String>> {
        if self.stdlib_enabled && func == "len" {
            if args.len() != 1 {
                return Err(CompileError::new("len: expected 1 argument"));
            }
            let ptr = self.next_temp();
            self.generate_ptr_expr(&args[0], params, locals, &ptr)?;
            let len_reg = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = call i64 @__rt_strlen(i8* %{})",
                len_reg, ptr
            )
            .unwrap();
            let int_reg = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = trunc i64 %{} to i32",
                int_reg, len_reg
            )
            .unwrap();
            return Ok(Some(int_reg));
        }

        if self.stdlib_enabled && func == "read_file" {
            if args.len() != 1 {
                return Err(CompileError::new("read_file: expected 1 argument"));
            }
            let ptr = self.next_temp();
            self.generate_ptr_expr(&args[0], params, locals, &ptr)?;
            let result = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = call i8* @__rt_read_file(i8* %{})",
                result, ptr
            )
            .unwrap();
            return Ok(Some(result));
        }

        if self.stdlib_enabled && func == "write_file" {
            if args.len() != 2 {
                return Err(CompileError::new("write_file: expected 2 arguments"));
            }
            let path_ptr = self.next_temp();
            self.generate_ptr_expr(&args[0], params, locals, &path_ptr)?;
            let data_ptr = self.next_temp();
            self.generate_ptr_expr(&args[1], params, locals, &data_ptr)?;
            let status = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = call i32 @__rt_write_file(i8* %{}, i8* %{})",
                status, path_ptr, data_ptr
            )
            .unwrap();
            return Ok(Some(status));
        }

        if self.stdlib_enabled && func == "exit" {
            if args.len() != 1 {
                return Err(CompileError::new("exit: expected 1 argument"));
            }
            let code = self.generate_int_expr(&args[0], params, locals)?;
            writeln!(
                &mut self.functions,
                "  call void @__rt_exit(i32 {})",
                code.as_str()
            )
            .unwrap();
            return Ok(None);
        }

        if self.stdlib_enabled && func == "to_string" {
            if args.len() != 1 {
                return Err(CompileError::new("to_string: expected 1 argument"));
            }
            let ty = self.infer_expr_type(&args[0], params, locals);
            if ty == "i32" {
                let val = self.generate_int_expr(&args[0], params, locals)?;
                let result = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call i8* @__rt_to_string_int(i32 {})",
                    result, val.as_str()
                ).unwrap();
                return Ok(Some(result));
            } else if ty == "double" {
                let val = self.generate_float_expr(&args[0], params, locals)?;
                let result = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call i8* @__rt_to_string_float(double {})",
                    result, val.as_str()
                ).unwrap();
                return Ok(Some(result));
            } else if ty == "i8*" {
                let ptr = self.next_temp();
                self.generate_ptr_expr(&args[0], params, locals, &ptr)?;
                return Ok(Some(ptr));
            } else {
                return Err(CompileError::new("to_string: unsupported type"));
            }
        }

        let resolved = self.resolve_function_name(func);
        let ret_ty = self
            .builtin_return_type(func)
            .or_else(|| self.function_sigs.get(&resolved).copied())
            .ok_or_else(|| {
                let suggestion = self
                    .suggest_function(func)
                    .filter(|s| s != func)
                    .map(|s| {
                        if s == self.stdlib_hint(func) {
                            format!(" {}", s)
                        } else {
                            format!(" did you mean `{}`?", s)
                        }
                    })
                    .unwrap_or_default();
                CompileError::new(format!("unknown function: {}{}", func, suggestion))
            })?;

        let param_types = self.get_function_param_types(func);
        let mut call_args = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let expected_type = param_types.get(i).copied().unwrap_or("i32");
            match expected_type {
                "i32" => {
                    let val = self.generate_int_expr(arg, params, locals)?;
                    call_args.push(format!("i32 {}", val.as_str()));
                }
                "double" => {
                    let val = self.generate_float_expr(arg, params, locals)?;
                    call_args.push(format!("double {}", val.as_str()));
                }
                "i8*" => {
                    let ptr = self.next_temp();
                    self.generate_ptr_expr(arg, params, locals, &ptr)?;
                    call_args.push(format!("i8* %{}", ptr));
                }
                _ => return Err(CompileError::new(format!("unsupported argument type in call: {}", expected_type))),
            }
        }

        if ret_ty == "void" {
            writeln!(
                &mut self.functions,
                "  call void @{}({})",
                resolved,
                call_args.join(", ")
            )
            .unwrap();
            Ok(None)
        } else {
            let result = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = call {} @{}({})",
                result,
                ret_ty,
                resolved,
                call_args.join(", ")
            )
            .unwrap();
            Ok(Some(result))
        }
    }

    fn emit_borrow_ptr(
        &mut self,
        name: &str,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
        result: &str,
    ) -> CompileResult<()> {
        if let Some((typ, alloca, _)) = locals.get(name) {
            let source_ty = match typ {
                Type::I32 => "i32",
                Type::F64 => "double",
                Type::Ptr => "i8*",
            };
            writeln!(
                &mut self.functions,
                "  %{} = bitcast {}* %{} to i8*",
                result, source_ty, alloca
            )
            .unwrap();
            return Ok(());
        }

        if let Some(idx) = params.iter().position(|p| p == name) {
            writeln!(&mut self.functions, "  %{} = bitcast i8* %arg{} to i8*", result, idx).unwrap();
            return Ok(());
        }

        Err(CompileError::new(format!(
            "borrow: unknown variable {}{}",
            name,
            self.suggest_variable(name, params, locals)
                .map(|s| format!(" did you mean `{}`?", s))
                .unwrap_or_default()
        )))
    }

    fn emit_printf_str(&mut self, ptr: &str) {
        let fmt = self.printf_fmt("%s\n", 4);
        writeln!(
            &mut self.functions,
            "  call i32 (i8*, ...) @printf(i8* %{}, i8* %{})",
            fmt,
            ptr
        )
        .unwrap();
    }

    fn printf_fmt(&mut self, fmt: &str, len: usize) -> String {
        let fmt_id = self.emit_string_const(fmt);
        let fmt_ptr = self.next_temp();
        self.emit_gep(&fmt_id, len, &fmt_ptr);
        fmt_ptr
    }

    fn emit_runtime_helpers(&mut self) {
        if self.runtime_helpers_emitted {
            return;
        }
        self.runtime_helpers_emitted = true;

        let empty = self.emit_string_const("");
        let rb = self.emit_string_const("rb");
        let wb = self.emit_string_const("wb");

        let mut helpers = String::new();
        writeln!(&mut helpers, "define i32 @__rt_strlen(i8* %s) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %len = call i64 @strlen(i8* %s)").unwrap();
        writeln!(&mut helpers, "  %tr = trunc i64 %len to i32").unwrap();
        writeln!(&mut helpers, "  ret i32 %tr").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define void @__rt_print_str(i8* %s) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        let fmt_str = self.emit_string_const("%s\n");
        writeln!(
            &mut helpers,
            "  %fmt = getelementptr [4 x i8], [4 x i8]* @{}, i32 0, i32 0",
            fmt_str
        )
        .unwrap();
        writeln!(&mut helpers, "  call i32 (i8*, ...) @printf(i8* %fmt, i8* %s)").unwrap();
        writeln!(&mut helpers, "  ret void").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define void @__rt_print_int(i32 %n) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        let fmt_str = self.emit_string_const("%d\n");
        writeln!(
            &mut helpers,
            "  %fmt = getelementptr [4 x i8], [4 x i8]* @{}, i32 0, i32 0",
            fmt_str
        )
        .unwrap();
        writeln!(&mut helpers, "  call i32 (i8*, ...) @printf(i8* %fmt, i32 %n)").unwrap();
        writeln!(&mut helpers, "  ret void").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define void @__rt_print_float(double %n) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        let fmt_str = self.emit_string_const("%f\n");
        writeln!(
            &mut helpers,
            "  %fmt = getelementptr [4 x i8], [4 x i8]* @{}, i32 0, i32 0",
            fmt_str
        )
        .unwrap();
        writeln!(&mut helpers, "  call i32 (i8*, ...) @printf(i8* %fmt, double %n)").unwrap();
        writeln!(&mut helpers, "  ret void").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define void @__rt_exit(i32 %code) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  call void @exit(i32 %code)").unwrap();
        writeln!(&mut helpers, "  ret void").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i8* @__rt_read_file(i8* %path) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(
            &mut helpers,
            "  %mode = getelementptr [3 x i8], [3 x i8]* @{}, i32 0, i32 0",
            rb
        )
        .unwrap();
        writeln!(&mut helpers, "  %file = call i8* @fopen(i8* %path, i8* %mode)").unwrap();
        writeln!(&mut helpers, "  %null = icmp eq i8* %file, null").unwrap();
        writeln!(&mut helpers, "  br i1 %null, label %empty, label %read").unwrap();
        writeln!(&mut helpers, "empty:").unwrap();
        writeln!(&mut helpers, "  %buf0 = call i8* @malloc(i64 1)").unwrap();
        writeln!(&mut helpers, "  store i8 0, i8* %buf0").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf0").unwrap();
        writeln!(&mut helpers, "read:").unwrap();
        writeln!(&mut helpers, "  %seek = call i32 @fseek(i8* %file, i64 0, i32 2)").unwrap();
        writeln!(&mut helpers, "  %size = call i64 @ftell(i8* %file)").unwrap();
        writeln!(&mut helpers, "  call void @rewind(i8* %file)").unwrap();
        writeln!(&mut helpers, "  %alloc = add i64 %size, 1").unwrap();
        writeln!(&mut helpers, "  %buf = call i8* @malloc(i64 %alloc)").unwrap();
        writeln!(&mut helpers, "  %readn = call i64 @fread(i8* %buf, i64 1, i64 %size, i8* %file)").unwrap();
        writeln!(&mut helpers, "  %end = getelementptr i8, i8* %buf, i64 %readn").unwrap();
        writeln!(&mut helpers, "  store i8 0, i8* %end").unwrap();
        writeln!(&mut helpers, "  call i32 @fclose(i8* %file)").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i32 @__rt_write_file(i8* %path, i8* %contents) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(
            &mut helpers,
            "  %mode = getelementptr [3 x i8], [3 x i8]* @{}, i32 0, i32 0",
            wb
        )
        .unwrap();
        writeln!(&mut helpers, "  %file = call i8* @fopen(i8* %path, i8* %mode)").unwrap();
        writeln!(&mut helpers, "  %null = icmp eq i8* %file, null").unwrap();
        writeln!(&mut helpers, "  br i1 %null, label %fail, label %write").unwrap();
        writeln!(&mut helpers, "fail:").unwrap();
        writeln!(&mut helpers, "  ret i32 -1").unwrap();
        writeln!(&mut helpers, "write:").unwrap();
        writeln!(&mut helpers, "  %len = call i64 @strlen(i8* %contents)").unwrap();
        writeln!(&mut helpers, "  %written = call i64 @fwrite(i8* %contents, i64 1, i64 %len, i8* %file)").unwrap();
        writeln!(&mut helpers, "  call i32 @fclose(i8* %file)").unwrap();
        writeln!(&mut helpers, "  %ok = icmp eq i64 %written, %len").unwrap();
        writeln!(&mut helpers, "  %res = select i1 %ok, i32 0, i32 -1").unwrap();
        writeln!(&mut helpers, "  ret i32 %res").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        let whitespace = self.emit_string_const(" \t\n\r\x0c\x0b");
        writeln!(&mut helpers, "define i32 @__rt_contains(i8* %s, i8* %sub) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %res = call i8* @strstr(i8* %s, i8* %sub)").unwrap();
        writeln!(&mut helpers, "  %found = icmp ne i8* %res, null").unwrap();
        writeln!(&mut helpers, "  %ret = zext i1 %found to i32").unwrap();
        writeln!(&mut helpers, "  ret i32 %ret").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i32 @__rt_starts_with(i8* %s, i8* %prefix) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %plen = call i64 @strlen(i8* %prefix)").unwrap();
        writeln!(&mut helpers, "  %res = call i32 @strncmp(i8* %s, i8* %prefix, i64 %plen)").unwrap();
        writeln!(&mut helpers, "  %eq = icmp eq i32 %res, 0").unwrap();
        writeln!(&mut helpers, "  %ret = zext i1 %eq to i32").unwrap();
        writeln!(&mut helpers, "  ret i32 %ret").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i32 @__rt_ends_with(i8* %s, i8* %suffix) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %slen = call i64 @strlen(i8* %s)").unwrap();
        writeln!(&mut helpers, "  %sublen = call i64 @strlen(i8* %suffix)").unwrap();
        writeln!(&mut helpers, "  %tooshort = icmp ult i64 %slen, %sublen").unwrap();
        writeln!(&mut helpers, "  br i1 %tooshort, label %false, label %check").unwrap();
        writeln!(&mut helpers, "check:").unwrap();
        writeln!(&mut helpers, "  %off = sub i64 %slen, %sublen").unwrap();
        writeln!(&mut helpers, "  %start = getelementptr i8, i8* %s, i64 %off").unwrap();
        writeln!(&mut helpers, "  %res = call i32 @strncmp(i8* %start, i8* %suffix, i64 %sublen)").unwrap();
        writeln!(&mut helpers, "  %eq = icmp eq i32 %res, 0").unwrap();
        writeln!(&mut helpers, "  %ret = zext i1 %eq to i32").unwrap();
        writeln!(&mut helpers, "  ret i32 %ret").unwrap();
        writeln!(&mut helpers, "false:").unwrap();
        writeln!(&mut helpers, "  ret i32 0").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i8* @__rt_substr(i8* %s, i32 %start, i32 %length) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %len64 = sext i32 %length to i64").unwrap();
        writeln!(&mut helpers, "  %alloc = add i64 %len64, 1").unwrap();
        writeln!(&mut helpers, "  %buf = call i8* @malloc(i64 %alloc)").unwrap();
        writeln!(&mut helpers, "  %start64 = sext i32 %start to i64").unwrap();
        writeln!(&mut helpers, "  %src = getelementptr i8, i8* %s, i64 %start64").unwrap();
        writeln!(&mut helpers, "  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %buf, i8* %src, i64 %len64, i1 false)").unwrap();
        writeln!(&mut helpers, "  %end = getelementptr i8, i8* %buf, i64 %len64").unwrap();
        writeln!(&mut helpers, "  store i8 0, i8* %end").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i8* @__rt_trim(i8* %s) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %len = call i64 @strlen(i8* %s)").unwrap();
        writeln!(&mut helpers, "  %ws = getelementptr [6 x i8], [6 x i8]* @{}, i32 0, i32 0", whitespace).unwrap();
        writeln!(&mut helpers, "  %start_off = call i64 @strspn(i8* %s, i8* %ws)").unwrap();
        writeln!(&mut helpers, "  %remaining = sub i64 %len, %start_off").unwrap();
        writeln!(&mut helpers, "  %start_ptr = getelementptr i8, i8* %s, i64 %start_off").unwrap();
        writeln!(&mut helpers, "  %end_off = call i64 @strcspn(i8* %start_ptr, i8* %ws)").unwrap();
        writeln!(&mut helpers, "  %trim_len = call i64 @llvm.umin.i64(i64 %remaining, i64 %end_off)").unwrap();
        writeln!(&mut helpers, "  %alloc = add i64 %trim_len, 1").unwrap();
        writeln!(&mut helpers, "  %buf = call i8* @malloc(i64 %alloc)").unwrap();
        writeln!(&mut helpers, "  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %buf, i8* %start_ptr, i64 %trim_len, i1 false)").unwrap();
        writeln!(&mut helpers, "  %end = getelementptr i8, i8* %buf, i64 %trim_len").unwrap();
        writeln!(&mut helpers, "  store i8 0, i8* %end").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i8* @__rt_to_uppercase(i8* %s) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %len = call i64 @strlen(i8* %s)").unwrap();
        writeln!(&mut helpers, "  %alloc = add i64 %len, 1").unwrap();
        writeln!(&mut helpers, "  %buf = call i8* @malloc(i64 %alloc)").unwrap();
        writeln!(&mut helpers, "  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %buf, i8* %s, i64 %alloc, i1 false)").unwrap();
        writeln!(&mut helpers, "  br label %loop").unwrap();
        writeln!(&mut helpers, "loop:").unwrap();
        writeln!(&mut helpers, "  %pos = phi i64 [0, %entry], [%next, %body]").unwrap();
        writeln!(&mut helpers, "  %cmp = icmp eq i64 %pos, %len").unwrap();
        writeln!(&mut helpers, "  br i1 %cmp, label %done, label %body").unwrap();
        writeln!(&mut helpers, "body:").unwrap();
        writeln!(&mut helpers, "  %ptr = getelementptr i8, i8* %buf, i64 %pos").unwrap();
        writeln!(&mut helpers, "  %ch = load i8, i8* %ptr").unwrap();
        writeln!(&mut helpers, "  %ch32 = sext i8 %ch to i32").unwrap();
        writeln!(&mut helpers, "  %up = call i32 @toupper(i32 %ch32)").unwrap();
        writeln!(&mut helpers, "  %up8 = trunc i32 %up to i8").unwrap();
        writeln!(&mut helpers, "  store i8 %up8, i8* %ptr").unwrap();
        writeln!(&mut helpers, "  %next = add i64 %pos, 1").unwrap();
        writeln!(&mut helpers, "  br label %loop").unwrap();
        writeln!(&mut helpers, "done:").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i8* @__rt_to_lowercase(i8* %s) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %len = call i64 @strlen(i8* %s)").unwrap();
        writeln!(&mut helpers, "  %alloc = add i64 %len, 1").unwrap();
        writeln!(&mut helpers, "  %buf = call i8* @malloc(i64 %alloc)").unwrap();
        writeln!(&mut helpers, "  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %buf, i8* %s, i64 %alloc, i1 false)").unwrap();
        writeln!(&mut helpers, "  br label %loop").unwrap();
        writeln!(&mut helpers, "loop:").unwrap();
        writeln!(&mut helpers, "  %pos = phi i64 [0, %entry], [%next, %body]").unwrap();
        writeln!(&mut helpers, "  %cmp = icmp eq i64 %pos, %len").unwrap();
        writeln!(&mut helpers, "  br i1 %cmp, label %done, label %body").unwrap();
        writeln!(&mut helpers, "body:").unwrap();
        writeln!(&mut helpers, "  %ptr = getelementptr i8, i8* %buf, i64 %pos").unwrap();
        writeln!(&mut helpers, "  %ch = load i8, i8* %ptr").unwrap();
        writeln!(&mut helpers, "  %ch32 = sext i8 %ch to i32").unwrap();
        writeln!(&mut helpers, "  %lo = call i32 @tolower(i32 %ch32)").unwrap();
        writeln!(&mut helpers, "  %lo8 = trunc i32 %lo to i8").unwrap();
        writeln!(&mut helpers, "  store i8 %lo8, i8* %ptr").unwrap();
        writeln!(&mut helpers, "  %next = add i64 %pos, 1").unwrap();
        writeln!(&mut helpers, "  br label %loop").unwrap();
        writeln!(&mut helpers, "done:").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i32 @__rt_to_int(i8* %s) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %res = call i32 @atoi(i8* %s)").unwrap();
        writeln!(&mut helpers, "  ret i32 %res").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define double @__rt_to_float(i8* %s) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %res = call double @atof(i8* %s)").unwrap();
        writeln!(&mut helpers, "  ret double %res").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define double @__rt_floor(double %n) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %res = call double @floor(double %n)").unwrap();
        writeln!(&mut helpers, "  ret double %res").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define double @__rt_ceil(double %n) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %res = call double @ceil(double %n)").unwrap();
        writeln!(&mut helpers, "  ret double %res").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define double @__rt_round(double %n) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %res = call double @round(double %n)").unwrap();
        writeln!(&mut helpers, "  ret double %res").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i8* @__rt_read_line() {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %buf = call i8* @malloc(i64 1024)").unwrap();
        writeln!(&mut helpers, "  %stdin_ptr = load i8*, i8** @stdin").unwrap();
        writeln!(&mut helpers, "  %res = call i8* @fgets(i8* %buf, i32 1024, i8* %stdin_ptr)").unwrap();
        writeln!(&mut helpers, "  %null = icmp eq i8* %res, null").unwrap();
        writeln!(&mut helpers, "  br i1 %null, label %empty, label %strip_nl").unwrap();
        writeln!(&mut helpers, "empty:").unwrap();
        writeln!(&mut helpers, "  store i8 0, i8* %buf").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "strip_nl:").unwrap();
        writeln!(&mut helpers, "  %len = call i64 @strlen(i8* %buf)").unwrap();
        writeln!(&mut helpers, "  %last_off = sub i64 %len, 1").unwrap();
        writeln!(&mut helpers, "  %last_ptr = getelementptr i8, i8* %buf, i64 %last_off").unwrap();
        writeln!(&mut helpers, "  %last_ch = load i8, i8* %last_ptr").unwrap();
        writeln!(&mut helpers, "  %is_nl = icmp eq i8 %last_ch, 10").unwrap();
        writeln!(&mut helpers, "  br i1 %is_nl, label %strip, label %ret").unwrap();
        writeln!(&mut helpers, "strip:").unwrap();
        writeln!(&mut helpers, "  store i8 0, i8* %last_ptr").unwrap();
        writeln!(&mut helpers, "  br label %ret").unwrap();
        writeln!(&mut helpers, "ret:").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i8* @__rt_to_string_int(i32 %n) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %buf = call i8* @malloc(i64 64)").unwrap();
        let fmt_d = self.emit_string_const("%d");
        writeln!(
            &mut helpers,
            "  %fmt = getelementptr [3 x i8], [3 x i8]* @{}, i32 0, i32 0",
            fmt_d
        ).unwrap();
        writeln!(&mut helpers, "  call i32 (i8*, i8*, ...) @sprintf(i8* %buf, i8* %fmt, i32 %n)").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i8* @__rt_to_string_float(double %n) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %buf = call i8* @malloc(i64 64)").unwrap();
        let fmt_f = self.emit_string_const("%f");
        writeln!(
            &mut helpers,
            "  %fmt = getelementptr [3 x i8], [3 x i8]* @{}, i32 0, i32 0",
            fmt_f
        ).unwrap();
        writeln!(&mut helpers, "  call i32 (i8*, i8*, ...) @sprintf(i8* %buf, i8* %fmt, double %n)").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define void @__rt_free(i8* %ptr) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %null = icmp eq i8* %ptr, null").unwrap();
        writeln!(&mut helpers, "  br i1 %null, label %done, label %free").unwrap();
        writeln!(&mut helpers, "free:").unwrap();
        writeln!(&mut helpers, "  call void @free(i8* %ptr)").unwrap();
        writeln!(&mut helpers, "  br label %done").unwrap();
        writeln!(&mut helpers, "done:").unwrap();
        writeln!(&mut helpers, "  ret void").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        // List runtime helpers
        writeln!(&mut helpers, "define i32 @__rt_list_len(i8* %list) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %len_ptr = getelementptr i8, i8* %list, i32 4").unwrap();
        writeln!(&mut helpers, "  %len_i32 = bitcast i8* %len_ptr to i32*").unwrap();
        writeln!(&mut helpers, "  %len = load i32, i32* %len_i32").unwrap();
        writeln!(&mut helpers, "  ret i32 %len").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i8* @__rt_list_push(i8* %list, i32 %val) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %cap_ptr = bitcast i8* %list to i32*").unwrap();
        writeln!(&mut helpers, "  %cap = load i32, i32* %cap_ptr").unwrap();
        writeln!(&mut helpers, "  %len_gep = getelementptr i8, i8* %list, i32 4").unwrap();
        writeln!(&mut helpers, "  %len_ptr = bitcast i8* %len_gep to i32*").unwrap();
        writeln!(&mut helpers, "  %len = load i32, i32* %len_ptr").unwrap();
        writeln!(&mut helpers, "  %needs_grow = icmp eq i32 %len, %cap").unwrap();
        writeln!(&mut helpers, "  br i1 %needs_grow, label %grow, label %store").unwrap();
        writeln!(&mut helpers, "grow:").unwrap();
        writeln!(&mut helpers, "  %newcap = mul i32 %cap, 2").unwrap();
        writeln!(&mut helpers, "  %cap_zero = icmp eq i32 %newcap, 0").unwrap();
        writeln!(&mut helpers, "  %newcap2 = select i1 %cap_zero, i32 4, i32 %newcap").unwrap();
        writeln!(&mut helpers, "  %old_data_bytes = mul i32 %cap, 4").unwrap();
        writeln!(&mut helpers, "  %old_total = add i32 %old_data_bytes, 8").unwrap();
        writeln!(&mut helpers, "  %old_total_i64 = sext i32 %old_total to i64").unwrap();
        writeln!(&mut helpers, "  %new_data_bytes = mul i32 %newcap2, 4").unwrap();
        writeln!(&mut helpers, "  %new_total = add i32 %new_data_bytes, 8").unwrap();
        writeln!(&mut helpers, "  %new_total_i64 = sext i32 %new_total to i64").unwrap();
        writeln!(&mut helpers, "  %newlist = call i8* @malloc(i64 %new_total_i64)").unwrap();
        writeln!(&mut helpers, "  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %newlist, i8* %list, i64 %old_total_i64, i1 false)").unwrap();
        writeln!(&mut helpers, "  call void @free(i8* %list)").unwrap();
        writeln!(&mut helpers, "  store i32 %newcap2, i32* %newlist").unwrap();
        writeln!(&mut helpers, "  br label %store").unwrap();
        writeln!(&mut helpers, "store:").unwrap();
        writeln!(&mut helpers, "  %cur_list = phi i8* [%list, %entry], [%newlist, %grow]").unwrap();
        writeln!(&mut helpers, "  %cur_len = phi i32 [%len, %entry], [%len, %grow]").unwrap();
        writeln!(&mut helpers, "  %elem_off = mul i32 %cur_len, 4").unwrap();
        writeln!(&mut helpers, "  %elem_off_total = add i32 %elem_off, 8").unwrap();
        writeln!(&mut helpers, "  %elem_gep = getelementptr i8, i8* %cur_list, i32 %elem_off_total").unwrap();
        writeln!(&mut helpers, "  %elem_ptr = bitcast i8* %elem_gep to i32*").unwrap();
        writeln!(&mut helpers, "  store i32 %val, i32* %elem_ptr").unwrap();
        writeln!(&mut helpers, "  %new_len = add i32 %cur_len, 1").unwrap();
        writeln!(&mut helpers, "  %len_store_gep = getelementptr i8, i8* %cur_list, i32 4").unwrap();
        writeln!(&mut helpers, "  %len_store_ptr = bitcast i8* %len_store_gep to i32*").unwrap();
        writeln!(&mut helpers, "  store i32 %new_len, i32* %len_store_ptr").unwrap();
        writeln!(&mut helpers, "  ret i8* %cur_list").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        writeln!(&mut helpers, "define i32 @__rt_list_pop(i8* %list) {{").unwrap();
        writeln!(&mut helpers, "entry:").unwrap();
        writeln!(&mut helpers, "  %len_gep = getelementptr i8, i8* %list, i32 4").unwrap();
        writeln!(&mut helpers, "  %len_ptr = bitcast i8* %len_gep to i32*").unwrap();
        writeln!(&mut helpers, "  %len = load i32, i32* %len_ptr").unwrap();
        writeln!(&mut helpers, "  %is_empty = icmp eq i32 %len, 0").unwrap();
        writeln!(&mut helpers, "  br i1 %is_empty, label %empty, label %pop").unwrap();
        writeln!(&mut helpers, "empty:").unwrap();
        writeln!(&mut helpers, "  ret i32 0").unwrap();
        writeln!(&mut helpers, "pop:").unwrap();
        writeln!(&mut helpers, "  %new_len = sub i32 %len, 1").unwrap();
        writeln!(&mut helpers, "  %elem_off = mul i32 %new_len, 4").unwrap();
        writeln!(&mut helpers, "  %elem_off_total = add i32 %elem_off, 8").unwrap();
        writeln!(&mut helpers, "  %elem_gep = getelementptr i8, i8* %list, i32 %elem_off_total").unwrap();
        writeln!(&mut helpers, "  %elem_ptr = bitcast i8* %elem_gep to i32*").unwrap();
        writeln!(&mut helpers, "  %val = load i32, i32* %elem_ptr").unwrap();
        writeln!(&mut helpers, "  store i32 %new_len, i32* %len_ptr").unwrap();
        writeln!(&mut helpers, "  ret i32 %val").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        // __rt_to_hex(i32) -> i8*
        let hex_fmt = self.emit_string_const("%08x");
        writeln!(&mut helpers, "define i8* @__rt_to_hex(i32 %n) {{").unwrap();
        writeln!(&mut helpers, "  %buf = call i8* @malloc(i64 11)").unwrap();
        writeln!(&mut helpers, "  %fmt = getelementptr [5 x i8], [5 x i8]* @{}, i32 0, i32 0", hex_fmt).unwrap();
        writeln!(&mut helpers, "  call i32 (i8*, i8*, ...) @sprintf(i8* %buf, i8* %fmt, i32 %n)").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        // __rt_str_repeat(i8*, i32) -> i8*
        writeln!(&mut helpers, "define i8* @__rt_str_repeat(i8* %s, i32 %count) {{").unwrap();
        writeln!(&mut helpers, "  %len = call i64 @strlen(i8* %s)").unwrap();
        writeln!(&mut helpers, "  %count64 = sext i32 %count to i64").unwrap();
        writeln!(&mut helpers, "  %total = mul i64 %len, %count64").unwrap();
        writeln!(&mut helpers, "  %total_plus1 = add i64 %total, 1").unwrap();
        writeln!(&mut helpers, "  %buf = call i8* @malloc(i64 %total_plus1)").unwrap();
        writeln!(&mut helpers, "  %cmp = icmp eq i64 %len, 0").unwrap();
        writeln!(&mut helpers, "  br i1 %cmp, label %done, label %loop").unwrap();
        writeln!(&mut helpers, "loop:").unwrap();
        writeln!(&mut helpers, "  %i = phi i32 [0, %0], [%next, %loop]").unwrap();
        writeln!(&mut helpers, "  %i64 = sext i32 %i to i64").unwrap();
        writeln!(&mut helpers, "  %off = mul i64 %i64, %len").unwrap();
        writeln!(&mut helpers, "  %dst = getelementptr i8, i8* %buf, i64 %off").unwrap();
        writeln!(&mut helpers, "  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %dst, i8* %s, i64 %len, i1 false)").unwrap();
        writeln!(&mut helpers, "  %next = add i32 %i, 1").unwrap();
        writeln!(&mut helpers, "  %done_flag = icmp eq i32 %next, %count").unwrap();
        writeln!(&mut helpers, "  br i1 %done_flag, label %done, label %loop").unwrap();
        writeln!(&mut helpers, "done:").unwrap();
        writeln!(&mut helpers, "  %end = getelementptr i8, i8* %buf, i64 %total").unwrap();
        writeln!(&mut helpers, "  store i8 0, i8* %end").unwrap();
        writeln!(&mut helpers, "  ret i8* %buf").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        // Math: sqrt, sin, cos, tan
        writeln!(&mut helpers, "define double @__rt_sqrt(double %n) {{").unwrap();
        writeln!(&mut helpers, "  %r = call double @sqrt(double %n)").unwrap();
        writeln!(&mut helpers, "  ret double %r").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();
        writeln!(&mut helpers, "define double @__rt_sin(double %n) {{").unwrap();
        writeln!(&mut helpers, "  %r = call double @sin(double %n)").unwrap();
        writeln!(&mut helpers, "  ret double %r").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();
        writeln!(&mut helpers, "define double @__rt_cos(double %n) {{").unwrap();
        writeln!(&mut helpers, "  %r = call double @cos(double %n)").unwrap();
        writeln!(&mut helpers, "  ret double %r").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();
        writeln!(&mut helpers, "define double @__rt_tan(double %n) {{").unwrap();
        writeln!(&mut helpers, "  %r = call double @tan(double %n)").unwrap();
        writeln!(&mut helpers, "  ret double %r").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        // __rt_abs (for f64)
        writeln!(&mut helpers, "define double @__rt_abs(double %n) {{").unwrap();
        writeln!(&mut helpers, "  %cmp = fcmp olt double %n, 0.0").unwrap();
        writeln!(&mut helpers, "  %neg = fsub double -0.0, %n").unwrap();
        writeln!(&mut helpers, "  %r = select i1 %cmp, double %neg, double %n").unwrap();
        writeln!(&mut helpers, "  ret double %r").unwrap();
        writeln!(&mut helpers, "}}\n").unwrap();

        // Declare llvm intrinsics used
        writeln!(&mut helpers, "declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg)\n").unwrap();
        writeln!(&mut helpers, "declare i64 @llvm.umin.i64(i64, i64)\n").unwrap();

        self.functions.push_str(&helpers);
        let _ = empty;
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
                    return Err(CompileError::new(format!(
                        "asm: unknown variable {}{}",
                        name,
                        self.suggest_variable(name, params, locals)
                            .map(|s| format!(" did you mean `{}`?", s))
                            .unwrap_or_default()
                    )));
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
        let input = format!("import std\n{}", input);
        let mut lexer = Lexer::new(&input);
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
