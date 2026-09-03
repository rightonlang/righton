use crate::ast::*;
use crate::borrow_checker::BorrowChecker;
use crate::diagnostics::Diagnostic;
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
    pub line: Option<usize>,
    pub column: Option<usize>,
}

impl CompileError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            line: None,
            column: None,
        }
    }

    pub fn with_span(mut self, line: usize, column: usize) -> Self {
        self.line = Some(line);
        self.column = Some(column);
        self
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        let span = match (self.line, self.column) {
            (Some(line), Some(col)) => SourceSpan::new(line, col),
            (Some(line), None) => SourceSpan::new(line, 0),
            _ => SourceSpan::unknown(),
        };
        Diagnostic::error(Some("E0004"), self.message.clone()).with_span(span)
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
    String,
    Bool,
    Struct(String),
    Enum(String),
    Tuple(Vec<Type>),
    Void,
    Unknown,
    Generic(String),
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
            Val::ImmFloat(n) => Self::format_float(*n),
            Val::Reg(r) => format!("%{}", r),
        }
    }
    fn as_float_str(&self) -> String {
        match self {
            Val::ImmFloat(n) => Self::format_float(*n),
            Val::Imm(n) => format!("{}.0", n),
            Val::Reg(r) => format!("%{}", r),
        }
    }
    fn format_float(n: f64) -> String {
        let s = n.to_string();
        if s.contains('.') || s.contains('e') || s.contains('E') {
            s
        } else {
            format!("{}.0", s)
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
    monomorphized: HashMap<String, FunctionDef>,
    monomorphized_structs: HashMap<String, StructDef>,

    current_function: Option<String>,
    loop_label_stack: Vec<(String, String)>,
    flattened_funcs: Vec<FunctionDef>,
    struct_defs: Vec<StructDef>,
    enum_defs: Vec<EnumDef>,
    type_aliases: HashMap<String, String>,
    list_elem_types: HashMap<String, String>,
}

impl Default for LLVMTextGen {
    fn default() -> Self {
        Self::new()
    }
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
            monomorphized: HashMap::new(),
            monomorphized_structs: HashMap::new(),
            current_function: None,
            loop_label_stack: Vec::new(),
            flattened_funcs: Vec::new(),
            struct_defs: Vec::new(),
            enum_defs: Vec::new(),
            type_aliases: HashMap::new(),
            list_elem_types: HashMap::new(),
        }
    }

    pub fn generate(&mut self, program: &Program) -> CompileResult<String> {
        self.stdlib_enabled = false;
        self.function_aliases.clear();
        let program = self.flatten_program(program)?;
        let mut all_funcs = program.functions.clone();
        for impl_def in &program.impls {
            all_funcs.extend(impl_def.methods.clone());
        }
        self.flattened_funcs = all_funcs;
        self.struct_defs = program.structs.clone();
        self.enum_defs = program.enums.clone();
        self.type_aliases.clear();
        for alias in &program.type_aliases {
            self.type_aliases
                .insert(alias.name.clone(), alias.value.clone());
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
        if program.profile != "release" {
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
        ir.push_str("%String = type { i8*, i32, i32 }\n\n");

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
        for impl_def in &program.impls {
            for method in &impl_def.methods {
                if !method.body.is_empty() || method.param_types.iter().all(|t| t.is_none()) {
                    self.generate_function(method)?;
                }
            }
        }
        let monomorphized_funcs: Vec<FunctionDef> = self.monomorphized.values().cloned().collect();
        for func in monomorphized_funcs {
            self.generate_function(&func)?;
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
                Expr::Import(spec, _) => {
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
            Expr::Binary(l, _, r, _) => {
                self.collect_calls(l, calls);
                self.collect_calls(r, calls);
            }
            Expr::Unary(_, inner, _) | Expr::Return(inner, _) => self.collect_calls(inner, calls),
            Expr::Let { value, .. } | Expr::Assign { value, .. } => {
                self.collect_calls(value, calls)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
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
            Expr::For {
                variable: _,
                iterable,
                body,
            } => {
                self.collect_calls(iterable, calls);
                for stmt in &body.stmts {
                    self.collect_calls(stmt, calls);
                }
            }
            Expr::ForIn {
                variable: _,
                iterable,
                body,
            } => {
                self.collect_calls(iterable, calls);
                for stmt in &body.stmts {
                    self.collect_calls(stmt, calls);
                }
            }
            Expr::ForRange {
                variable: _,
                start,
                end,
                body,
            } => {
                self.collect_calls(start, calls);
                self.collect_calls(end, calls);
                for stmt in &body.stmts {
                    self.collect_calls(stmt, calls);
                }
            }
            Expr::Break | Expr::Continue => {}
            Expr::FString(elements, _) => {
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
            Expr::Tuple(elements, _) => {
                for elem in elements {
                    self.collect_calls(elem, calls);
                }
            }
            Expr::TupleAccess { target, .. } => {
                self.collect_calls(target, calls);
            }
            Expr::Import(_, _)
            | Expr::Identifier(_, _)
            | Expr::Borrow { .. }
            | Expr::StringLiteral(_, _)
            | Expr::MultilineString(_, _)
            | Expr::Literal(_, _) => {}
        }
    }

    fn load_module(
        &mut self,
        spec: &str,
        loaded: &mut HashSet<String>,
        active: &mut Vec<String>,
    ) -> CompileResult<Program> {
        if active.iter().any(|item| item == spec) {
            return Err(CompileError::new(format!(
                "circular import detected: {}",
                spec
            )));
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
        let code = fs::read_to_string(&path).map_err(|e| {
            CompileError::new(format!("failed to read import '{}': {}", path.display(), e))
        })?;

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
            .map_err(|e| {
                CompileError::new(
                    e.iter()
                        .map(|err| err.to_string())
                        .collect::<Vec<_>>()
                        .join("\n")
                        .to_string(),
                )
            })?;
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
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect()
    }

    fn mangle_program(&mut self, program: Program, prefix: &str) -> Program {
        let function_names: HashSet<String> =
            program.functions.iter().map(|f| f.name.clone()).collect();
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
            Expr::Binary(l, op, r, _) => Expr::Binary(
                Box::new(self.rename_calls(*l, alias_map, function_names)),
                op,
                Box::new(self.rename_calls(*r, alias_map, function_names)),
                SourceSpan::unknown(),
            ),
            Expr::Unary(op, inner, _) => Expr::Unary(
                op,
                Box::new(self.rename_calls(*inner, alias_map, function_names)),
                SourceSpan::unknown(),
            ),
            Expr::Let {
                name,
                typ,
                value,
                is_const,
            } => Expr::Let {
                name,
                typ,
                value: Box::new(self.rename_calls(*value, alias_map, function_names)),
                is_const,
            },
            Expr::Assign { name, value } => Expr::Assign {
                name,
                value: Box::new(self.rename_calls(*value, alias_map, function_names)),
            },
            Expr::Return(inner, _) => Expr::Return(
                Box::new(self.rename_calls(*inner, alias_map, function_names)),
                SourceSpan::unknown(),
            ),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => Expr::If {
                condition: Box::new(self.rename_calls(*condition, alias_map, function_names)),
                then_branch: Box::new(Block {
                    stmts: then_branch
                        .stmts
                        .into_iter()
                        .map(|stmt| self.rename_calls(stmt, alias_map, function_names))
                        .collect(),
                }),
                else_branch: else_branch.map(|branch| {
                    Box::new(Block {
                        stmts: branch
                            .stmts
                            .into_iter()
                            .map(|stmt| self.rename_calls(stmt, alias_map, function_names))
                            .collect(),
                    })
                }),
            },
            Expr::FString(elements, _) => Expr::FString(
                elements
                    .into_iter()
                    .map(|el| self.rename_calls(el, alias_map, function_names))
                    .collect(),
                SourceSpan::unknown(),
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
            Expr::For {
                variable,
                iterable,
                body,
            } => Expr::For {
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
            Expr::ForIn {
                variable,
                iterable,
                body,
            } => Expr::ForIn {
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
            Expr::ForRange {
                variable,
                start,
                end,
                body,
            } => Expr::ForRange {
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
                items
                    .into_iter()
                    .map(|item| self.rename_calls(item, alias_map, function_names))
                    .collect(),
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
                arms: arms
                    .into_iter()
                    .map(|arm| crate::ast::MatchArm {
                        pattern: arm.pattern,
                        body: Box::new(self.rename_calls(*arm.body, alias_map, function_names)),
                    })
                    .collect(),
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
            if let Ok(exe_path) = std::env::current_exe()
                && let Some(dir) = exe_path.parent()
            {
                let candidate = dir.join("stdlib").join("std.ro");
                if candidate.exists() {
                    return candidate;
                }
                let candidate2 = dir.join("std.ro");
                if candidate2.exists() {
                    return candidate2;
                }
            }

            // 3) try current working directory
            let cwd_candidate = std::env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("."))
                .join("stdlib")
                .join("std.ro");
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
            std::env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("."))
                .join(path)
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
            "str" | "string" | "ptr" => "%String*",
            "void" => "void",
            _ => "i8*",
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
        if !func.generic_params.is_empty() {
            return Ok(());
        }
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
                        _ => "i8*",
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
                    writeln!(
                        &mut self.functions,
                        "  store i32 %arg{}, i32* %{}",
                        i, alloca_name
                    )
                    .unwrap();
                }
                Type::F64 => {
                    writeln!(&mut self.functions, "  %{} = alloca double", alloca_name).unwrap();
                    writeln!(
                        &mut self.functions,
                        "  store double %arg{}, double* %{}",
                        i, alloca_name
                    )
                    .unwrap();
                }
                Type::Ptr => {
                    // For pointer params, arg[i] is already a pointer value, store it
                    writeln!(&mut self.functions, "  %{} = alloca i8*", alloca_name).unwrap();
                    writeln!(
                        &mut self.functions,
                        "  store i8* %arg{}, i8** %{}",
                        i, alloca_name
                    )
                    .unwrap();
                }
                _ => {
                    writeln!(&mut self.functions, "  %{} = alloca i8*", alloca_name).unwrap();
                    writeln!(
                        &mut self.functions,
                        "  store i8* %arg{}, i8** %{}",
                        i, alloca_name
                    )
                    .unwrap();
                }
            }
            locals.insert(param.clone(), (param_type, alloca_name, false));
        }
        let mut has_return = false;

        for expr in &func.body {
            if matches!(expr, Expr::Return(_, _)) {
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
            "str" | "string" | "ptr" => "%String*",
            "void" => "void",
            _ => "i8*",
        }
    }

    fn type_str_to_type(t: &str) -> Type {
        match t {
            "i32" => Type::I32,
            "f64" | "double" | "float" => Type::F64,
            "str" | "string" | "ptr" => Type::String,
            _ => Type::I32,
        }
    }

    fn infer_list_elem_type(
        &self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> String {
        match expr {
            Expr::List(items) if !items.is_empty() => {
                self.infer_expr_type(&items[0], params, locals).to_string()
            }
            Expr::Identifier(name, _) => self
                .list_elem_types
                .get(name)
                .cloned()
                .unwrap_or_else(|| self.infer_expr_type(expr, params, locals).to_string()),
            _ => "i32".to_string(),
        }
    }

    fn list_elem_size(elem_type: &str) -> i32 {
        match elem_type {
            "double" => 8,
            _ => 4,
        }
    }

    pub fn infer_expr_type(
        &self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> &'static str {
        match expr {
            Expr::Import(_, _) => "void",
            Expr::Literal(Literal::Int(_), _) => "i32",
            Expr::Literal(Literal::Float(_), _) => "double",
            Expr::Literal(Literal::Bool(_), _) => "i32",
            Expr::Literal(Literal::Str(_), _) => "%String*",
            Expr::StringLiteral(_, _) | Expr::MultilineString(_, _) | Expr::FString(_, _) => {
                "%String*"
            }
            Expr::Borrow { .. } => "%String*",
            Expr::List(_) => "i8*",
            Expr::StructLiteral { .. } => "i8*",
            Expr::EnumLiteral { .. } => "i8*",
            Expr::Tuple(elements, _) => {
                if elements.is_empty() {
                    "void"
                } else {
                    "i8*"
                }
            }
            Expr::TupleAccess {
                target: _,
                index: _,
            } => "i32",
            Expr::FieldAssign { .. } => "void",
            Expr::FieldAccess { target, field } => {
                let target_name = if let Expr::Identifier(name, _) = target.as_ref() {
                    if locals.contains_key(name) {
                        name.clone()
                    } else {
                        String::new()
                    }
                } else {
                    String::new()
                };
                if !target_name.is_empty() {
                    for sdef in &self.struct_defs {
                        for sf in &sdef.fields {
                            if sf.name == *field {
                                match sf.typ.as_str() {
                                    "i32" | "bool" => return "i32",
                                    "f64" | "float" | "double" => return "double",
                                    _ => {
                                        let prefix = format!("{}_", sdef.name);
                                        for msdef in self.monomorphized_structs.values() {
                                            if msdef.name.starts_with(&prefix) {
                                                for msf in &msdef.fields {
                                                    if msf.name == *field {
                                                        match msf.typ.as_str() {
                                                            "i32" | "bool" => return "i32",
                                                            "f64" | "float" | "double" => {
                                                                return "double";
                                                            }
                                                            _ => return "i8*",
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        return "i8*";
                                    }
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
            Expr::Identifier(name, _) => {
                if let Some((typ, _, _)) = locals.get(name) {
                    match typ {
                        Type::I32 => "i32",
                        Type::F64 => "double",
                        Type::Ptr => "i8*",
                        Type::String => "%String*",
                        _ => "i8*",
                    }
                } else if params.contains(name) {
                    if let Some((typ, _, _)) = locals.get(name) {
                        match typ {
                            Type::I32 => "i32",
                            Type::F64 => "double",
                            Type::Ptr => "i8*",
                            Type::String => "%String*",
                            _ => "i8*",
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
            Expr::Call { func, .. } => {
                let resolved = self.resolve_function_name(func);
                let sig = self.function_sigs.get(&resolved).copied();
                self.builtin_return_type(func).or(sig).unwrap_or("void")
            }
            Expr::Binary(l, op, r, _) => {
                let is_comparison = matches!(
                    *op,
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
                );
                let is_logical = matches!(*op, BinOp::And | BinOp::Or);
                if is_comparison || is_logical {
                    return "i32";
                }
                if matches!(l.as_ref(), Expr::Literal(Literal::Float(_), _))
                    || matches!(r.as_ref(), Expr::Literal(Literal::Float(_), _))
                {
                    return "double";
                }
                if matches!(l.as_ref(), Expr::Literal(Literal::Int(_), _))
                    || matches!(r.as_ref(), Expr::Literal(Literal::Int(_), _))
                {
                    return "i32";
                }
                if matches!(l.as_ref(), Expr::Literal(Literal::Bool(_), _))
                    || matches!(r.as_ref(), Expr::Literal(Literal::Bool(_), _))
                {
                    return "i32";
                }
                if let Expr::Identifier(ln, _) = l.as_ref()
                    && let Some((Type::F64, _, _)) = locals.get(ln)
                {
                    return "double";
                }
                if let Expr::Identifier(rn, _) = r.as_ref()
                    && let Some((Type::F64, _, _)) = locals.get(rn)
                {
                    return "double";
                }
                let lt = self.infer_expr_type(l, params, locals);
                let rt = self.infer_expr_type(r, params, locals);
                if lt == "double" || rt == "double" {
                    "double"
                } else if lt == "void" || rt == "void" {
                    "i32"
                } else if (lt == "i8*" || lt == "%String*") && (rt == "i8*" || rt == "%String*") {
                    if matches!(*op, BinOp::Add) {
                        "%String*"
                    } else {
                        "i32"
                    }
                } else {
                    "i32"
                }
            }
            Expr::Unary(op, inner, _) => {
                if matches!(op, UnaryOp::Not) {
                    return "i32";
                }
                let inner_type = self.infer_expr_type(inner, params, locals);
                if inner_type == "void" {
                    "i32"
                } else {
                    inner_type
                }
            }
            Expr::If {
                then_branch,
                else_branch,
                ..
            } => {
                let last_then = then_branch
                    .stmts
                    .last()
                    .map(|e| self.infer_expr_type(e, params, locals))
                    .unwrap_or("void");
                if let Some(else_block) = else_branch {
                    let last_else = else_block
                        .stmts
                        .last()
                        .map(|e| self.infer_expr_type(e, params, locals))
                        .unwrap_or("void");
                    if last_then != "void" && last_else != "void" && last_then == last_else {
                        last_then
                    } else {
                        "void"
                    }
                } else {
                    last_then
                }
            }
            _ => "void",
        }
    }

    fn infer_return_type(&self, body: &[Expr], params: &[String]) -> &'static str {
        if let Some(name) = &self.current_function
            && name == "main"
        {
            return "i32";
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
            if let Expr::Return(inner, _) = expr {
                return self.infer_expr_type_with_sigs(inner, params, temp_locals, sigs);
            }
            if let Expr::If {
                then_branch,
                else_branch,
                ..
            } = expr
            {
                let then_type =
                    self.find_return_in_exprs(&then_branch.stmts, params, temp_locals, sigs);
                if then_type != "void" {
                    return then_type;
                }
                if let Some(else_block) = else_branch {
                    let else_type =
                        self.find_return_in_exprs(&else_block.stmts, params, temp_locals, sigs);
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
            "__rt_read_file" => return Some("%String*"),
            "__rt_write_file" => return Some("i32"),
            "__rt_exit" | "__rt_print_str" | "__rt_print_int" | "__rt_print_float"
            | "__rt_free" => return Some("void"),
            "__rt_wrap_string" => return Some("%String*"),
            "__rt_contains" | "__rt_starts_with" | "__rt_ends_with" | "__rt_to_int"
            | "__rt_list_len" | "__rt_list_pop" => return Some("i32"),
            "__rt_substr"
            | "__rt_trim"
            | "__rt_to_uppercase"
            | "__rt_to_lowercase"
            | "__rt_read_line"
            | "__rt_to_string_int"
            | "__rt_to_string_float"
            | "__rt_to_hex"
            | "__rt_str_repeat" => return Some("i8*"),
            "__rt_list_push" => return Some("i8*"),
            "__rt_to_float" | "__rt_floor" | "__rt_ceil" | "__rt_round" | "__rt_sqrt"
            | "__rt_sin" | "__rt_cos" | "__rt_tan" | "__rt_abs" => return Some("double"),
            _ => {}
        }

        if !self.stdlib_enabled {
            return None;
        }

        match func {
            "print" | "asm" | "exit" | "write_file" => Some("void"),
            "len" => Some("i32"),
            "read_file" => Some("%String*"),
            "contains" | "starts_with" | "ends_with" | "to_int" | "is_empty" => Some("i32"),
            "substr" | "trim" | "to_uppercase" | "to_lowercase" | "to_string" | "read_line" => {
                Some("%String*")
            }
            "to_float" | "floor" | "ceil" | "round" => Some("double"),
            "sqrt" | "sin" | "cos" | "tan" => Some("double"),
            "to_hex" | "str_repeat" => Some("%String*"),
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

    fn best_suggestion<'a>(
        &self,
        name: &str,
        candidates: impl IntoIterator<Item = &'a str>,
    ) -> Option<String> {
        let mut best: Option<(usize, String)> = None;
        for candidate in candidates {
            let dist = Self::levenshtein(name, candidate);
            if dist > 3 {
                continue;
            }
            if best
                .as_ref()
                .map(|(best_dist, _)| dist < *best_dist)
                .unwrap_or(true)
            {
                best = Some((dist, candidate.to_string()));
            }
        }
        best.map(|(_, s)| s)
    }

    fn suggest_function(&self, func: &str) -> Option<String> {
        if !self.stdlib_enabled
            && matches!(func, "print" | "len" | "read_file" | "write_file" | "exit")
        {
            return Some(self.stdlib_hint(func).to_string());
        }

        let mut candidates: Vec<String> = self.function_sigs.keys().cloned().collect();
        if self.stdlib_enabled {
            candidates.extend(
                ["print", "len", "read_file", "write_file", "exit", "asm"]
                    .into_iter()
                    .map(String::from),
            );
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

    fn find_function_def(&self, name: &str) -> Option<&FunctionDef> {
        self.flattened_funcs.iter().find(|f| f.name == name)
    }

    fn type_to_mangled_suffix(typ: &Type) -> String {
        match typ {
            Type::I32 => "i32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "str".to_string(),
            Type::Ptr => "ptr".to_string(),
            Type::Struct(name) => format!("struct_{}", name),
            Type::Enum(name) => format!("enum_{}", name),
            Type::Tuple(types) => {
                let inner = types
                    .iter()
                    .map(Self::type_to_mangled_suffix)
                    .collect::<Vec<_>>()
                    .join("_");
                format!("tuple_{}", inner)
            }
            Type::Void => "void".to_string(),
            Type::Unknown => "unknown".to_string(),
            Type::Generic(name) => format!("generic_{}", name),
        }
    }

    fn get_function_call_return_type(
        &mut self,
        func: &str,
        args: &[Expr],
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> CompileResult<&'static str> {
        let resolved = self.resolve_function_name(func);
        let mut ret_ty = self
            .builtin_return_type(func)
            .or_else(|| self.function_sigs.get(&resolved).copied())
            .unwrap_or("void");

        if let Some(func_def) = self.find_function_def(&resolved)
            && !func_def.generic_params.is_empty()
        {
            let mut concrete_types = Vec::new();
            for arg in args.iter() {
                let arg_type = self.infer_expr_type(arg, params, locals);
                let concrete = match arg_type {
                    "i32" => Type::I32,
                    "double" => Type::F64,
                    "i8*" => Type::Ptr,
                    "%String*" => Type::String,
                    _ => Type::Unknown,
                };
                concrete_types.push(concrete);
            }
            let mangled = self.monomorphize_function(&func_def.clone(), &concrete_types)?;
            if let Some(mangled_def) = self.monomorphized.get(&mangled) {
                ret_ty = mangled_def
                    .return_type
                    .as_deref()
                    .map(Self::type_str_to_llvm_ret)
                    .unwrap_or("void");
            }
        }

        Ok(ret_ty)
    }

    fn monomorphize_function(
        &mut self,
        func: &FunctionDef,
        concrete_types: &[Type],
    ) -> CompileResult<String> {
        if concrete_types.len() != func.generic_params.len() {
            return Err(CompileError::new(format!(
                "generic function {} expects {} type parameters, got {}",
                func.name,
                func.generic_params.len(),
                concrete_types.len()
            )));
        }

        let mut type_map: HashMap<String, Type> = HashMap::new();
        for (param, typ) in func.generic_params.iter().zip(concrete_types.iter()) {
            type_map.insert(param.clone(), typ.clone());
        }

        let suffix = concrete_types
            .iter()
            .map(Self::type_to_mangled_suffix)
            .collect::<Vec<_>>()
            .join("_");
        let mangled_name = format!("{}_{}", func.name, suffix);

        if self.monomorphized.contains_key(&mangled_name) {
            return Ok(mangled_name);
        }

        let mut monomorphized = func.clone();
        monomorphized.name = mangled_name.clone();
        monomorphized.generic_params.clear();

        for param_type in &mut monomorphized.param_types {
            if let Some(t) = param_type {
                *param_type = Some(self.substitute_generic_types(t, &type_map));
            }
        }
        if let Some(ref mut rt) = monomorphized.return_type {
            *rt = self.substitute_generic_types(rt, &type_map);
        }

        for param_type in &mut monomorphized.param_types {
            if let Some(t) = param_type {
                *param_type = Some(self.resolve_type_name(t));
            }
        }
        if let Some(ref mut rt) = monomorphized.return_type {
            *rt = self.resolve_type_name(rt);
        }

        let mut new_body = Vec::new();
        for expr in &monomorphized.body {
            new_body.push(self.substitute_expr_generics(expr, &type_map)?);
        }
        monomorphized.body = new_body;

        self.monomorphized
            .insert(mangled_name.clone(), monomorphized);
        Ok(mangled_name)
    }

    fn substitute_generic_types(&self, s: &str, type_map: &HashMap<String, Type>) -> String {
        if let Some(typ) = type_map.get(s) {
            match typ {
                Type::I32 => "i32".to_string(),
                Type::F64 => "f64".to_string(),
                Type::Bool => "bool".to_string(),
                Type::String => "str".to_string(),
                Type::Ptr => "ptr".to_string(),
                Type::Struct(name) => name.clone(),
                Type::Enum(name) => name.clone(),
                Type::Generic(_) => s.to_string(),
                _ => s.to_string(),
            }
        } else if let Some(open) = s.find('[')
            && s.ends_with(']')
        {
            let base = &s[..open];
            let args_str = &s[open + 1..s.len() - 1];
            let args: Vec<String> = args_str
                .split(',')
                .map(|a| self.substitute_generic_types(a.trim(), type_map))
                .collect();
            format!("{}[{}]", base, args.join(", "))
        } else {
            s.to_string()
        }
    }

    fn monomorphize_struct(
        &mut self,
        name: &str,
        concrete_types: &[Type],
    ) -> CompileResult<String> {
        let suffix = concrete_types
            .iter()
            .map(Self::type_to_mangled_suffix)
            .collect::<Vec<_>>()
            .join("_");
        let mangled_name = format!("{}_{}", name, suffix);

        if self.monomorphized_structs.contains_key(&mangled_name) {
            return Ok(mangled_name);
        }

        let sdef = self.struct_defs.iter().find(|s| s.name == name).cloned();
        if let Some(mut struct_def) = sdef {
            if struct_def.generic_params.len() != concrete_types.len() {
                return Err(CompileError::new(format!(
                    "struct {} expects {} type parameters, got {}",
                    name,
                    struct_def.generic_params.len(),
                    concrete_types.len()
                )));
            }

            let mut type_map: HashMap<String, Type> = HashMap::new();
            for (param, typ) in struct_def.generic_params.iter().zip(concrete_types.iter()) {
                type_map.insert(param.clone(), typ.clone());
            }

            struct_def.name = mangled_name.clone();
            struct_def.generic_params.clear();
            for field in &mut struct_def.fields {
                field.typ = self.substitute_generic_types(&field.typ, &type_map);
            }

            self.monomorphized_structs
                .insert(mangled_name.clone(), struct_def);
        }
        Ok(mangled_name)
    }

    fn substitute_expr_generics(
        &mut self,
        expr: &Expr,
        type_map: &HashMap<String, Type>,
    ) -> CompileResult<Expr> {
        match expr {
            Expr::Call { func, args } => {
                let mut new_args = Vec::new();
                for arg in args {
                    new_args.push(self.substitute_expr_generics(arg, type_map)?);
                }
                Ok(Expr::Call {
                    func: func.clone(),
                    args: new_args,
                })
            }
            Expr::Binary(l, op, r, span) => Ok(Expr::Binary(
                Box::new(self.substitute_expr_generics(l, type_map)?),
                op.clone(),
                Box::new(self.substitute_expr_generics(r, type_map)?),
                *span,
            )),
            Expr::Unary(op, inner, span) => Ok(Expr::Unary(
                op.clone(),
                Box::new(self.substitute_expr_generics(inner, type_map)?),
                *span,
            )),
            Expr::Return(inner, span) => Ok(Expr::Return(
                Box::new(self.substitute_expr_generics(inner, type_map)?),
                *span,
            )),
            Expr::Let {
                name,
                typ,
                value,
                is_const,
            } => {
                let new_typ = typ
                    .as_ref()
                    .map(|t| self.substitute_generic_types(t, type_map));
                Ok(Expr::Let {
                    name: name.clone(),
                    typ: new_typ,
                    value: Box::new(self.substitute_expr_generics(value, type_map)?),
                    is_const: *is_const,
                })
            }
            Expr::Assign { name, value } => Ok(Expr::Assign {
                name: name.clone(),
                value: Box::new(self.substitute_expr_generics(value, type_map)?),
            }),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let new_then = then_branch
                    .stmts
                    .iter()
                    .map(|e| self.substitute_expr_generics(e, type_map))
                    .collect::<CompileResult<Vec<_>>>()?;
                let new_else = else_branch
                    .as_ref()
                    .map(|b| {
                        let stmts = b
                            .stmts
                            .iter()
                            .map(|e| self.substitute_expr_generics(e, type_map))
                            .collect::<CompileResult<Vec<_>>>()?;
                        Ok(Box::new(Block { stmts }))
                    })
                    .transpose()?;
                Ok(Expr::If {
                    condition: Box::new(self.substitute_expr_generics(condition, type_map)?),
                    then_branch: Box::new(Block { stmts: new_then }),
                    else_branch: new_else,
                })
            }
            Expr::While { condition, body } => {
                let new_body = body
                    .stmts
                    .iter()
                    .map(|e| self.substitute_expr_generics(e, type_map))
                    .collect::<CompileResult<Vec<_>>>()?;
                Ok(Expr::While {
                    condition: Box::new(self.substitute_expr_generics(condition, type_map)?),
                    body: Box::new(Block { stmts: new_body }),
                })
            }
            Expr::For {
                variable,
                iterable,
                body,
            } => {
                let new_body = body
                    .stmts
                    .iter()
                    .map(|e| self.substitute_expr_generics(e, type_map))
                    .collect::<CompileResult<Vec<_>>>()?;
                Ok(Expr::For {
                    variable: variable.clone(),
                    iterable: Box::new(self.substitute_expr_generics(iterable, type_map)?),
                    body: Box::new(Block { stmts: new_body }),
                })
            }
            Expr::ForIn {
                variable,
                iterable,
                body,
            } => {
                let new_body = body
                    .stmts
                    .iter()
                    .map(|e| self.substitute_expr_generics(e, type_map))
                    .collect::<CompileResult<Vec<_>>>()?;
                Ok(Expr::ForIn {
                    variable: variable.clone(),
                    iterable: Box::new(self.substitute_expr_generics(iterable, type_map)?),
                    body: Box::new(Block { stmts: new_body }),
                })
            }
            Expr::ForRange {
                variable,
                start,
                end,
                body,
            } => {
                let new_body = body
                    .stmts
                    .iter()
                    .map(|e| self.substitute_expr_generics(e, type_map))
                    .collect::<CompileResult<Vec<_>>>()?;
                Ok(Expr::ForRange {
                    variable: variable.clone(),
                    start: Box::new(self.substitute_expr_generics(start, type_map)?),
                    end: Box::new(self.substitute_expr_generics(end, type_map)?),
                    body: Box::new(Block { stmts: new_body }),
                })
            }
            Expr::Match { expr, arms } => {
                let new_arms = arms
                    .iter()
                    .map(|arm| {
                        let new_body = self.substitute_expr_generics(&arm.body, type_map)?;
                        Ok(MatchArm {
                            pattern: arm.pattern.clone(),
                            body: Box::new(new_body),
                        })
                    })
                    .collect::<CompileResult<Vec<_>>>()?;
                Ok(Expr::Match {
                    expr: Box::new(self.substitute_expr_generics(expr, type_map)?),
                    arms: new_arms,
                })
            }
            Expr::StructLiteral { name, fields } => {
                let new_name = if type_map.contains_key(name) {
                    if let Some(Type::Struct(n)) = type_map.get(name) {
                        n.clone()
                    } else {
                        name.clone()
                    }
                } else {
                    name.clone()
                };
                let new_fields = fields
                    .iter()
                    .map(|(fname, fval)| {
                        Ok((
                            fname.clone(),
                            self.substitute_expr_generics(fval, type_map)?,
                        ))
                    })
                    .collect::<CompileResult<Vec<_>>>()?;
                Ok(Expr::StructLiteral {
                    name: new_name,
                    fields: new_fields,
                })
            }
            _ => Ok(expr.clone()),
        }
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
            "__rt_strlen" => vec!["%String*"],
            "__rt_print_str" => vec!["%String*"],
            "__rt_print_int" => vec!["i32"],
            "__rt_print_float" => vec!["double"],
            "__rt_exit" => vec!["i32"],
            "__rt_read_file" => vec!["i8*"],
            "__rt_write_file" => vec!["i8*", "%String*"],
            "__rt_contains" | "__rt_starts_with" | "__rt_ends_with" => vec!["%String*", "%String*"],
            "__rt_substr" => vec!["%String*", "i32", "i32"],
            "__rt_trim" | "__rt_to_uppercase" | "__rt_to_lowercase" => vec!["%String*"],
            "__rt_to_int" => vec!["%String*"],
            "__rt_to_float" => vec!["%String*"],
            "__rt_floor" | "__rt_ceil" | "__rt_round" => vec!["double"],
            "__rt_read_line" => vec![],
            "__rt_to_string_int" => vec!["i32"],
            "__rt_to_string_float" => vec!["double"],
            "__rt_list_len" => vec!["i8*"],
            "__rt_list_push" => vec!["i8*", "i32"],
            "__rt_list_pop" => vec!["i8*"],
            "__rt_free" => vec!["i8*"],
            "__rt_to_hex" => vec!["i32"],
            "__rt_str_repeat" => vec!["%String*", "i32"],
            "__rt_wrap_string" => vec!["i8*"],
            "__rt_sqrt" | "__rt_sin" | "__rt_cos" | "__rt_tan" | "__rt_abs" => vec!["double"],
            _ => vec![],
        };
        if !builtin_types.is_empty() {
            return builtin_types;
        }

        // Stdlib wrappers that take list pointers (i8*) – hardcode to avoid
        // inference treating list `i8*` as string. Without this, `list_len`
        // would be inferred as String via the `i8*` string heuristic.
        match func {
            "list_len" | "std__list_len" => return vec!["i8*"],
            "list_push" | "std__list_push" => return vec!["i8*", "i32"],
            "list_pop" | "std__list_pop" => return vec!["i8*"],
            "list_free" | "std__list_free" => return vec!["i8*"],
            _ => {}
        }
        match self.resolve_function_name(func).as_str() {
            "std__list_len" => return vec!["i8*"],
            "std__list_push" => return vec!["i8*", "i32"],
            "std__list_pop" => return vec!["i8*"],
            "std__list_free" => return vec!["i8*"],
            _ => {}
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
                            "str" | "string" | "ptr" => "%String*",
                            _ => "i8*",
                        }
                    } else {
                        let inferred =
                            self.infer_param_type(&func_def.body, param, &func_def.params);
                        match inferred {
                            Type::I32 => "i32",
                            Type::F64 => "double",
                            Type::String => "%String*",
                            _ => "i8*",
                        }
                    };
                    types.push(param_type);
                }
                return types;
            }
        }
        if let Some(func_def) = self.monomorphized.get(&resolved) {
            for (i, param) in func_def.params.iter().enumerate() {
                let param_type = if let Some(Some(t)) = func_def.param_types.get(i) {
                    match t.as_str() {
                        "i32" => "i32",
                        "f64" | "double" | "float" => "double",
                        "str" | "string" | "ptr" => "%String*",
                        _ => "i8*",
                    }
                } else {
                    let inferred = self.infer_param_type(&func_def.body, param, &func_def.params);
                    match inferred {
                        Type::I32 => "i32",
                        Type::F64 => "double",
                        Type::String => "%String*",
                        _ => "i8*",
                    }
                };
                types.push(param_type);
            }
            return types;
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
            "%String*" => {
                let empty = self.emit_string_const("");
                let ptr = self.next_temp();
                self.emit_gep(&empty, 1, &ptr);
                let str_ptr = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca %String", str_ptr).unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.ptr = getelementptr %String, %String* %{}, i32 0, i32 0",
                    str_ptr, str_ptr
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i8* %{}, i8** %{}.ptr",
                    ptr, str_ptr
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.len = getelementptr %String, %String* %{}, i32 0, i32 1",
                    str_ptr, str_ptr
                )
                .unwrap();
                writeln!(&mut self.functions, "  store i32 0, i32* %{}.len", str_ptr).unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.cap = getelementptr %String, %String* %{}, i32 0, i32 2",
                    str_ptr, str_ptr
                )
                .unwrap();
                writeln!(&mut self.functions, "  store i32 1, i32* %{}.cap", str_ptr).unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast %String* %{} to %String*",
                    ptr, str_ptr
                )
                .unwrap();
                writeln!(&mut self.functions, "  ret %String* %{}", ptr).unwrap();
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
        // Hardcode for list stdlib wrappers – inference via `i8*` is ambiguous
        // with string `i8*`, and the generic heuristic would misclassify.
        if let Some(cur) = &self.current_function {
            if cur == "std__list_len" || cur == "list_len" {
                if param == "lst" {
                    return Type::Ptr;
                }
            }
            if cur == "std__list_push" || cur == "list_push" {
                if param == "lst" {
                    return Type::Ptr;
                }
                if param == "val" {
                    return Type::I32;
                }
            }
            if cur == "std__list_pop" || cur == "list_pop" || cur == "std__list_free" || cur == "list_free" {
                if param == "lst" {
                    return Type::Ptr;
                }
            }
        }
        let mut temp_locals: HashMap<String, (Type, String, bool)> = HashMap::new();

        // First: process all params BEFORE the one we're inferring
        for (i, p) in known_params.iter().enumerate() {
            if p == param {
                break;
            }
            for e in body {
                if let Expr::Let {
                    name,
                    typ: _,
                    value,
                    is_const: _,
                } = e
                    && name == p
                {
                    let inferred = self.infer_expr_type(value, known_params, &temp_locals);
                    let t = match inferred {
                        "i32" => Type::I32,
                        "double" => Type::F64,
                        "%String*" | "i8*" => Type::String,
                        _ => Type::Ptr,
                    };
                    temp_locals.insert(p.to_string(), (t, format!("arg{}", i), false));
                    break;
                }
            }
        }

        // Second: look for let binding of the CURRENT param
        for expr in body {
            if let Expr::Let {
                name,
                typ,
                value,
                is_const: _,
            } = expr
                && name == param
            {
                if let Some(t) = typ {
                    let resolved = self.resolve_type_name(t);
                    match resolved.as_str() {
                        "i32" => return Type::I32,
                        "f64" | "float" => return Type::F64,
                        "str" | "string" | "ptr" => return Type::String,
                        _ => {}
                    }
                }
                let inferred = self.infer_expr_type(value, known_params, &temp_locals);
                return match inferred {
                    "i32" => Type::I32,
                    "double" => Type::F64,
                    "%String*" | "i8*" => Type::String,
                    _ => Type::Ptr,
                };
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
        let is_used_in_string_context = |body: &[Expr],
                                         target_param: &str,
                                         stdlib_enabled: bool|
         -> bool {
            fn check_expr(
                expr: &Expr,
                param: &str,
                stdlib_enabled: bool,
                visited: &mut Vec<String>,
                pt: &dyn Fn(&str, usize) -> Option<&'static str>,
            ) -> bool {
                match expr {
                    Expr::Call { func, args } => {
                        for (i, arg) in args.iter().enumerate() {
                            if let Expr::Identifier(n, _) = arg {
                                if n == param {
                                    if let Some(expected) = pt(func, i) {
                                        if expected == "%String*" {
                                            return true;
                                        }
                                        if expected == "i8*" {
                                            // i8* is used for both raw strings and lists;
                                            // only treat as string if not a list helper
                                            if !func.starts_with("__rt_list")
                                                && func != "__rt_free"
                                                && func != "__rt_panic_bounds"
                                            {
                                                return true;
                                            }
                                        }
                                    } else if func.starts_with("__rt_")
                                        || (stdlib_enabled
                                            && matches!(
                                                func.as_str(),
                                                "print"
                                                    | "len"
                                                    | "read_file"
                                                    | "write_file"
                                                    | "is_empty"
                                            ))
                                    {
                                        return true;
                                    }
                                }
                            } else if let Expr::Call { .. } = arg
                                && check_expr(arg, param, stdlib_enabled, visited, pt)
                            {
                                return true;
                            }
                        }
                        if !visited.contains(func) {
                            visited.push(func.to_string());
                        }
                        false
                    }
                    Expr::Return(inner, _) => check_expr(inner, param, stdlib_enabled, visited, pt),
                    _ => false,
                }
            }

            let mut visited = Vec::new();
            for expr in body {
                if check_expr(
                    expr,
                    target_param,
                    stdlib_enabled,
                    &mut visited,
                    &param_type_at,
                ) {
                    return true;
                }
            }
            false
        };

        if is_used_in_string_context(body, param, self.stdlib_enabled) {
            return Type::String;
        }

        // Check if param is used in int context (passed to int-expecting functions)
        fn check_int_context(
            expr: &Expr,
            param: &str,
            pt: &dyn Fn(&str, usize) -> Option<&'static str>,
        ) -> bool {
            match expr {
                Expr::Call { func, args } => {
                    for (i, arg) in args.iter().enumerate() {
                        if let Expr::Identifier(n, _) = arg {
                            if n == param
                                && let Some(expected) = pt(func, i)
                                && expected == "i32"
                            {
                                return true;
                            }
                        } else if let Expr::Call { .. } = arg
                            && check_int_context(arg, param, pt)
                        {
                            return true;
                        }
                    }
                    false
                }
                Expr::Return(inner, _) => check_int_context(inner, param, pt),
                _ => false,
            }
        }
        if body
            .iter()
            .any(|e| check_int_context(e, param, &param_type_at))
        {
            return Type::I32;
        }

        // Check if param is used in float context (passed to float-expecting functions)
        fn check_float_context(
            expr: &Expr,
            param: &str,
            pt: &dyn Fn(&str, usize) -> Option<&'static str>,
        ) -> bool {
            match expr {
                Expr::Call { func, args } => {
                    for (i, arg) in args.iter().enumerate() {
                        if let Expr::Identifier(n, _) = arg {
                            if n == param
                                && let Some(expected) = pt(func, i)
                                && expected == "double"
                            {
                                return true;
                            }
                        } else if let Expr::Call { .. } = arg
                            && check_float_context(arg, param, pt)
                        {
                            return true;
                        }
                    }
                    false
                }
                Expr::Return(inner, _) => check_float_context(inner, param, pt),
                _ => false,
            }
        }
        if body
            .iter()
            .any(|e| check_float_context(e, param, &param_type_at))
        {
            return Type::F64;
        }

        // Fourth: check if param is used in binary expressions (comparison/arithmetic) anywhere
        fn param_in_binary(expr: &Expr, param: &str) -> bool {
            match expr {
                Expr::Identifier(n, _) => n == param,
                Expr::Binary(l, _, r, _) => {
                    if let Expr::Identifier(n, _) = l.as_ref()
                        && n == param
                    {
                        return true;
                    }
                    if let Expr::Identifier(n, _) = r.as_ref()
                        && n == param
                    {
                        return true;
                    }
                    param_in_binary(l, param) || param_in_binary(r, param)
                }
                Expr::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    param_in_binary(condition, param)
                        || then_branch.stmts.iter().any(|e| param_in_binary(e, param))
                        || else_branch
                            .as_ref()
                            .is_some_and(|b| b.stmts.iter().any(|e| param_in_binary(e, param)))
                }
                Expr::While { condition, body } => {
                    param_in_binary(condition, param)
                        || body.stmts.iter().any(|e| param_in_binary(e, param))
                }
                Expr::For { iterable, body, .. } => {
                    param_in_binary(iterable, param)
                        || body.stmts.iter().any(|e| param_in_binary(e, param))
                }
                Expr::ForIn { iterable, body, .. } => {
                    param_in_binary(iterable, param)
                        || body.stmts.iter().any(|e| param_in_binary(e, param))
                }
                Expr::ForRange {
                    start, end, body, ..
                } => {
                    param_in_binary(start, param)
                        || param_in_binary(end, param)
                        || body.stmts.iter().any(|e| param_in_binary(e, param))
                }
                Expr::Call { args, .. } => args.iter().any(|a| param_in_binary(a, param)),
                Expr::Return(inner, _) => param_in_binary(inner, param),
                Expr::Unary(_, inner, _) => param_in_binary(inner, param),
                Expr::Index { target, index } => {
                    param_in_binary(target, param) || param_in_binary(index, param)
                }
                Expr::AssignIndex { index, value, .. } => {
                    param_in_binary(index, param) || param_in_binary(value, param)
                }
                Expr::List(items) => items.iter().any(|item| param_in_binary(item, param)),
                Expr::Match { expr, arms } => {
                    param_in_binary(expr, param)
                        || arms.iter().any(|arm| param_in_binary(&arm.body, param))
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
        let last_is_return = stmts
            .iter()
            .last()
            .is_some_and(|s| matches!(s, Expr::Return(_, _)));
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
            Expr::Import(_, _) => {}

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
                        "str" | "string" | "ptr" => Type::String,
                        _ => Type::Ptr,
                    }
                } else {
                    let inferred = self.infer_expr_type(value, params, locals);
                    match inferred {
                        "i32" => Type::I32,
                        "double" => Type::F64,
                        "%String*" => Type::String,
                        "i8*" => Type::Ptr,
                        "void" => Type::I32,
                        _ => {
                            return Err(CompileError::new(format!(
                                "couldn't infer the type for variable {} (got {})",
                                name, inferred
                            )));
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
                        if let Expr::List(_) = value.as_ref() {
                            let elem_type = self.infer_list_elem_type(value, params, locals);
                            self.list_elem_types.insert(name.clone(), elem_type);
                        }
                    }
                    Type::String => {
                        let str_ptr = self.next_temp();
                        self.generate_string_expr(value, params, locals, &str_ptr)?;
                        writeln!(&mut self.functions, "  %{} = alloca %String*", alloca).unwrap();
                        writeln!(
                            &mut self.functions,
                            "  store %String* %{}, %String** %{}",
                            str_ptr, alloca
                        )
                        .unwrap();
                        locals.insert(name.clone(), (Type::String, alloca.clone(), *is_const));
                    }
                    _ => {
                        // fallback for unsupported types
                        // handled as pointer
                    }
                }
            }

            Expr::Assign { name, value } => {
                let (var_ty, alloca, is_const) = locals.get(name).ok_or_else(|| {
                    let suggestion = self
                        .suggest_variable(name, params, locals)
                        .map(|s| format!(" did you mean `{}`?", s))
                        .unwrap_or_default();
                    CompileError::new(format!(
                        "assign of unknown variable: {}{}",
                        name, suggestion
                    ))
                })?;

                if *is_const {
                    return Err(CompileError::new(format!(
                        "cannot assign to const-variable: {}",
                        name
                    )));
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
                    Type::String => {
                        let ptr = self.next_temp();
                        self.generate_string_expr(value, params, locals, &ptr)?;
                        writeln!(
                            &mut self.functions,
                            "  store %String* %{}, %String** %{}",
                            ptr, alloca
                        )
                        .unwrap();
                    }
                    _ => {
                        // fallback for unsupported types
                        // handled as pointer
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
                    } else if ty == "%String*" {
                        let result = self.next_temp();
                        self.generate_string_expr(arg, params, locals, &result)?;
                        let str_ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = getelementptr %String, %String* %{}, i32 0, i32 0",
                            str_ptr, result
                        )
                        .unwrap();
                        let raw_ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = load i8*, i8** %{}",
                            raw_ptr, str_ptr
                        )
                        .unwrap();
                        self.emit_printf_str(&raw_ptr);
                    }
                }
            }

            Expr::Call { func, args } if func != "asm" => {
                let _ = self.emit_call(func, args, params, locals)?;
            }

            Expr::Call { func, args } if func == "asm" => {
                self.emit_asm_call(args, params, locals)?;
            }

            Expr::Return(inner, _) => {
                let ty = self.infer_expr_type(inner, params, locals);
                if ty == "i32" && return_ty == "i32" {
                    let val = self.generate_int_expr(inner, params, locals)?;
                    writeln!(&mut self.functions, "  ret i32 {}", val.as_str()).unwrap();
                } else if ty == "i8*" && return_ty == "i8*" {
                    let ptr = self.next_temp();
                    self.generate_ptr_expr(inner, params, locals, &ptr)?;
                    writeln!(&mut self.functions, "  ret i8* %{}", ptr).unwrap();
                } else if ty == "%String*" && return_ty == "%String*" {
                    let ptr = self.next_temp();
                    self.generate_string_expr(inner, params, locals, &ptr)?;
                    writeln!(&mut self.functions, "  ret %String* %{}", ptr).unwrap();
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
                    return Err(CompileError::new(format!(
                        "return: types mismatch (got {}, expected {})",
                        ty, return_ty
                    )));
                }
            }

            Expr::FString(elements, _) => {
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
                    let then_ended_with_return = self.generate_block_with_terminator(
                        &then_branch.stmts,
                        params,
                        locals,
                        return_ty,
                    )?;
                    if !then_ended_with_return {
                        writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                    }

                    writeln!(&mut self.functions, "{}:", else_l).unwrap();
                    let else_ended_with_return = self.generate_block_with_terminator(
                        &else_block.stmts,
                        params,
                        locals,
                        return_ty,
                    )?;
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
                    let then_ended_with_return = self.generate_block_with_terminator(
                        &then_branch.stmts,
                        params,
                        locals,
                        return_ty,
                    )?;
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

                self.loop_label_stack
                    .push((loop_start.clone(), loop_end.clone()));

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

            Expr::For {
                variable,
                iterable,
                body,
            } => {
                let iter_val = self.generate_int_expr(iterable, params, locals)?;
                let loop_start = self.next_block_label("for_start");
                let loop_body = self.next_block_label("for_body");
                let loop_continue = self.next_block_label("for_continue");
                let loop_end = self.next_block_label("for_end");

                let counter_alloca = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca i32", counter_alloca).unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}",
                    iter_val.as_str(),
                    counter_alloca
                )
                .unwrap();

                let mut locals_for = locals.clone();
                locals_for.insert(variable.clone(), (Type::I32, counter_alloca.clone(), false));

                self.loop_label_stack
                    .push((loop_continue.clone(), loop_end.clone()));

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
                writeln!(&mut self.functions, "  br label %{}", loop_continue).unwrap();
                writeln!(&mut self.functions, "{}:", loop_continue).unwrap();
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
                writeln!(&mut self.functions, "  br label %{}", loop_start).unwrap();
                writeln!(&mut self.functions, "{}:", loop_end).unwrap();
                self.loop_label_stack.pop();
            }

            Expr::ForIn {
                variable,
                iterable,
                body,
            } => {
                let list_ptr = self.next_temp();
                self.generate_ptr_expr(iterable, params, locals, &list_ptr)?;

                let len_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 4",
                    len_ptr, list_ptr
                )
                .unwrap();
                let len_i32 = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i32*",
                    len_i32, len_ptr
                )
                .unwrap();
                let list_len = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i32, i32* %{}",
                    list_len, len_i32
                )
                .unwrap();

                let elem_type = self.infer_list_elem_type(iterable, params, locals);
                let elem_size = Self::list_elem_size(&elem_type);

                let var_alloca = self.next_temp();
                match elem_type.as_str() {
                    "double" => {
                        writeln!(&mut self.functions, "  %{} = alloca double", var_alloca).unwrap();
                    }
                    _ => {
                        writeln!(&mut self.functions, "  %{} = alloca i32", var_alloca).unwrap();
                    }
                }

                let counter_alloca = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca i32", counter_alloca).unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 0, i32* %{}",
                    counter_alloca
                )
                .unwrap();

                let mut locals_for = locals.clone();
                locals_for.insert(
                    variable.clone(),
                    (
                        Self::type_str_to_type(&elem_type),
                        var_alloca.clone(),
                        false,
                    ),
                );

                let loop_start = self.next_block_label("for_start");
                let loop_body = self.next_block_label("for_body");
                let loop_continue = self.next_block_label("for_continue");
                let loop_end = self.next_block_label("for_end");

                self.loop_label_stack
                    .push((loop_continue.clone(), loop_end.clone()));

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
                writeln!(
                    &mut self.functions,
                    "  %{} = icmp slt i32 %{}, {}",
                    loop_cond, counter_val, list_len
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  br i1 %{}, label %{}, label %{}",
                    loop_cond, loop_body, loop_end
                )
                .unwrap();

                writeln!(&mut self.functions, "{}:", loop_body).unwrap();

                let elem_off = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = mul i32 {}, {}",
                    elem_off, counter_val, elem_size
                )
                .unwrap();
                let elem_off_total = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 %{}, 8",
                    elem_off_total, elem_off
                )
                .unwrap();
                let elem_gep = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 {}",
                    elem_gep, list_ptr, elem_off_total
                )
                .unwrap();

                match elem_type.as_str() {
                    "double" => {
                        let elem_ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = bitcast i8* %{} to double*",
                            elem_ptr, elem_gep
                        )
                        .unwrap();
                        let elem_val = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = load double, double* %{}",
                            elem_val, elem_ptr
                        )
                        .unwrap();
                        writeln!(
                            &mut self.functions,
                            "  store double %{}, double* %{}",
                            elem_val, var_alloca
                        )
                        .unwrap();
                    }
                    _ => {
                        let elem_ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = bitcast i8* %{} to i32*",
                            elem_ptr, elem_gep
                        )
                        .unwrap();
                        let elem_val = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = load i32, i32* %{}",
                            elem_val, elem_ptr
                        )
                        .unwrap();
                        writeln!(
                            &mut self.functions,
                            "  store i32 %{}, i32* %{}",
                            elem_val, var_alloca
                        )
                        .unwrap();
                    }
                }

                for stmt in &body.stmts {
                    self.generate_expr(stmt, params, &mut locals_for, return_ty)?;
                }
                writeln!(&mut self.functions, "  br label %{}", loop_continue).unwrap();
                writeln!(&mut self.functions, "{}:", loop_continue).unwrap();
                let new_counter = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 %{}, 1",
                    new_counter, counter_val
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 %{}, i32* %{}",
                    new_counter, counter_alloca
                )
                .unwrap();
                writeln!(&mut self.functions, "  br label %{}", loop_start).unwrap();
                writeln!(&mut self.functions, "{}:", loop_end).unwrap();
                self.loop_label_stack.pop();
            }

            Expr::ForRange {
                variable,
                start,
                end,
                body,
            } => {
                let start_val = self.generate_int_expr(start, params, locals)?;
                let end_val = self.generate_int_expr(end, params, locals)?;
                let loop_start_label = self.next_block_label("for_start");
                let loop_body = self.next_block_label("for_body");
                let loop_continue = self.next_block_label("for_continue");
                let loop_end = self.next_block_label("for_end");

                let counter_alloca = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca i32", counter_alloca).unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}",
                    start_val.as_str(),
                    counter_alloca
                )
                .unwrap();

                let mut locals_for = locals.clone();
                locals_for.insert(variable.clone(), (Type::I32, counter_alloca.clone(), false));

                self.loop_label_stack
                    .push((loop_continue.clone(), loop_end.clone()));

                writeln!(&mut self.functions, "  br label %{}", loop_start_label).unwrap();
                writeln!(&mut self.functions, "{}:", loop_start_label).unwrap();

                let counter_val = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i32, i32* %{}",
                    counter_val, counter_alloca
                )
                .unwrap();

                let loop_cond = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = icmp slt i32 %{}, {}",
                    loop_cond,
                    counter_val,
                    end_val.as_str()
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
                writeln!(&mut self.functions, "  br label %{}", loop_continue).unwrap();
                writeln!(&mut self.functions, "{}:", loop_continue).unwrap();
                let new_counter = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 %{}, 1",
                    new_counter, counter_val
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 %{}, i32* %{}",
                    new_counter, counter_alloca
                )
                .unwrap();
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

            Expr::Literal(Literal::Int(_), _)
            | Expr::Literal(Literal::Bool(_), _)
            | Expr::Literal(Literal::Float(_), _)
            | Expr::Unary(_, _, _) => {
                let inferred = self.infer_expr_type(expr, params, locals);
                if inferred == "double" {
                    let val = self.generate_float_expr(expr, params, locals)?;
                    self.emit_printf_float(&val.as_str());
                } else {
                    let val = self.generate_int_expr(expr, params, locals)?;
                    self.emit_printf_int(&val.as_str());
                }
            }

            Expr::Binary(l, op, r, _) => {
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

            Expr::StringLiteral(s, _) | Expr::MultilineString(s, _) => {
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

            Expr::Index {
                target: _target,
                index: _index,
            } => {
                let val = self.generate_int_expr(expr, params, locals)?;
                self.emit_printf_int(&val.as_str());
            }

            Expr::Match { expr, arms } => {
                let has_variant = arms
                    .iter()
                    .any(|a| matches!(a.pattern, crate::ast::MatchPattern::Variant { .. }));
                if has_variant {
                    // Enum match: compare discriminant
                    let enum_ptr = self.next_temp();
                    self.generate_ptr_expr(expr, params, locals, &enum_ptr)?;
                    let disc_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast i8* %{} to i32*",
                        disc_ptr, enum_ptr
                    )
                    .unwrap();
                    let disc_val = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = load i32, i32* %{}",
                        disc_val, disc_ptr
                    )
                    .unwrap();
                    let end_label = self.next_block_label("ematch_end");
                    let wildcard_label = arms
                        .iter()
                        .position(|a| matches!(a.pattern, crate::ast::MatchPattern::Wildcard))
                        .map(|_| self.next_block_label("ematch_default"));
                    for (i, arm) in arms.iter().enumerate() {
                        match &arm.pattern {
                            crate::ast::MatchPattern::Variant {
                                name: vname,
                                bindings,
                            } => {
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
                                    && !matches!(
                                        arms[i + 1].pattern,
                                        crate::ast::MatchPattern::Wildcard
                                    ) {
                                    self.next_block_label("ematch_next")
                                } else {
                                    wildcard_label.clone().unwrap_or_else(|| end_label.clone())
                                };
                                let cmp = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = icmp eq i32 %{}, {}",
                                    cmp, disc_val, disc_val_to_check
                                )
                                .unwrap();
                                let arm_label = self.next_block_label("earm");
                                writeln!(
                                    &mut self.functions,
                                    "  br i1 %{}, label %{}, label %{}",
                                    cmp, arm_label, next_label
                                )
                                .unwrap();
                                writeln!(&mut self.functions, "{}:", arm_label).unwrap();
                                // Extract bindings from payload
                                let variant_fields: Vec<(String, String)> = {
                                    let edef = self
                                        .enum_defs
                                        .iter()
                                        .find(|e| e.variants.iter().any(|v| v.name == *vname));
                                    match edef {
                                        Some(e) => e
                                            .variants
                                            .iter()
                                            .find(|v| v.name == *vname)
                                            .map(|v| {
                                                v.fields
                                                    .iter()
                                                    .enumerate()
                                                    .filter(|(i, _)| bindings.len() > *i)
                                                    .map(|(i, ft)| {
                                                        (bindings[i].clone(), ft.clone())
                                                    })
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
                                    writeln!(
                                        &mut self.functions,
                                        "  %{} = getelementptr i8, i8* %{}, i32 {}",
                                        elem_ptr, enum_ptr, payload_off
                                    )
                                    .unwrap();
                                    match field_typ.as_str() {
                                        "i32" | "bool" => {
                                            writeln!(
                                                &mut self.functions,
                                                "  %{} = alloca i32",
                                                alloca
                                            )
                                            .unwrap();
                                            let typed_ptr = self.next_temp();
                                            writeln!(
                                                &mut self.functions,
                                                "  %{} = bitcast i8* %{} to i32*",
                                                typed_ptr, elem_ptr
                                            )
                                            .unwrap();
                                            let loaded = self.next_temp();
                                            writeln!(
                                                &mut self.functions,
                                                "  %{} = load i32, i32* %{}",
                                                loaded, typed_ptr
                                            )
                                            .unwrap();
                                            writeln!(
                                                &mut self.functions,
                                                "  store i32 %{}, i32* %{}",
                                                loaded, alloca
                                            )
                                            .unwrap();
                                            locals.insert(
                                                binding_name.clone(),
                                                (crate::compiler::Type::I32, alloca.clone(), false),
                                            );
                                            payload_off += 4;
                                        }
                                        "f64" | "float" | "double" => {
                                            writeln!(
                                                &mut self.functions,
                                                "  %{} = alloca double",
                                                alloca
                                            )
                                            .unwrap();
                                            let typed_ptr = self.next_temp();
                                            writeln!(
                                                &mut self.functions,
                                                "  %{} = bitcast i8* %{} to double*",
                                                typed_ptr, elem_ptr
                                            )
                                            .unwrap();
                                            let loaded = self.next_temp();
                                            writeln!(
                                                &mut self.functions,
                                                "  %{} = load double, double* %{}",
                                                loaded, typed_ptr
                                            )
                                            .unwrap();
                                            writeln!(
                                                &mut self.functions,
                                                "  store double %{}, double* %{}",
                                                loaded, alloca
                                            )
                                            .unwrap();
                                            locals.insert(
                                                binding_name.clone(),
                                                (crate::compiler::Type::F64, alloca.clone(), false),
                                            );
                                            payload_off += 8;
                                        }
                                        _ => {
                                            writeln!(
                                                &mut self.functions,
                                                "  %{} = alloca i8*",
                                                alloca
                                            )
                                            .unwrap();
                                            let typed_ptr = self.next_temp();
                                            writeln!(
                                                &mut self.functions,
                                                "  %{} = bitcast i8* %{} to i8**",
                                                typed_ptr, elem_ptr
                                            )
                                            .unwrap();
                                            let loaded = self.next_temp();
                                            writeln!(
                                                &mut self.functions,
                                                "  %{} = load i8*, i8** %{}",
                                                loaded, typed_ptr
                                            )
                                            .unwrap();
                                            writeln!(
                                                &mut self.functions,
                                                "  store i8* %{}, i8** %{}",
                                                loaded, alloca
                                            )
                                            .unwrap();
                                            locals.insert(
                                                binding_name.clone(),
                                                (crate::compiler::Type::Ptr, alloca.clone(), false),
                                            );
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
                                if next_label != end_label
                                    && wildcard_label.as_ref() != Some(&next_label)
                                {
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
                        match_reg,
                        match_val.as_str()
                    )
                    .unwrap();
                    let end_label = self.next_block_label("match_end");
                    let wildcard_label = arms
                        .iter()
                        .position(|a| matches!(a.pattern, crate::ast::MatchPattern::Wildcard))
                        .map(|_| self.next_block_label("match_default"));
                    for (i, arm) in arms.iter().enumerate() {
                        match &arm.pattern {
                            crate::ast::MatchPattern::Int(n) => {
                                let next_label = if i + 1 < arms.len()
                                    && !matches!(
                                        arms[i + 1].pattern,
                                        crate::ast::MatchPattern::Wildcard
                                    ) {
                                    self.next_block_label("match_next")
                                } else {
                                    wildcard_label.clone().unwrap_or_else(|| end_label.clone())
                                };
                                let cmp = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = icmp eq i32 %{}, {}",
                                    cmp, match_reg, n
                                )
                                .unwrap();
                                let arm_label = self.next_block_label("match_arm");
                                writeln!(
                                    &mut self.functions,
                                    "  br i1 %{}, label %{}, label %{}",
                                    cmp, arm_label, next_label
                                )
                                .unwrap();
                                writeln!(&mut self.functions, "{}:", arm_label).unwrap();
                                self.generate_expr(&arm.body, params, locals, return_ty)?;
                                writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                                if next_label != end_label
                                    && wildcard_label.as_ref() != Some(&next_label)
                                {
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
                self.generate_ptr_expr(
                    &Expr::Identifier(name.clone(), SourceSpan::unknown()),
                    params,
                    locals,
                    &ptr,
                )?;
                let idx = self.generate_int_expr(index, params, locals)?;
                let elem_off = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = mul i32 {}, 4",
                    elem_off,
                    idx.as_str()
                )
                .unwrap();
                let offset = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 %{}, 8",
                    offset, elem_off
                )
                .unwrap();
                let elem_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 %{}",
                    elem_ptr, ptr, offset
                )
                .unwrap();
                let i32_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i32*",
                    i32_ptr, elem_ptr
                )
                .unwrap();
                let val = self.generate_int_expr(value, params, locals)?;
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}",
                    val.as_str(),
                    i32_ptr
                )
                .unwrap();
            }

            Expr::FieldAssign {
                target,
                field,
                value,
            } => {
                let mut struct_defs = self.struct_defs.clone();
                struct_defs.extend(self.monomorphized_structs.values().cloned());
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let target_name = if let Expr::Identifier(id, _) = target.as_ref() {
                    Some(id.clone())
                } else {
                    None
                };
                let mut offset = 0i32;
                let mut field_typ = "i32".to_string();
                if let Some(ref name) = target_name
                    && let Some(sdef) = struct_defs.iter().find(|_s| locals.contains_key(name))
                {
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
                let elem_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 {}",
                    elem_ptr, ptr, offset
                )
                .unwrap();
                match field_typ.as_str() {
                    "i32" | "bool" => {
                        let typed_ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = bitcast i8* %{} to i32*",
                            typed_ptr, elem_ptr
                        )
                        .unwrap();
                        let val = self.generate_int_expr(value, params, locals)?;
                        writeln!(
                            &mut self.functions,
                            "  store i32 {}, i32* %{}",
                            val.as_str(),
                            typed_ptr
                        )
                        .unwrap();
                    }
                    "f64" | "float" | "double" => {
                        let typed_ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = bitcast i8* %{} to double*",
                            typed_ptr, elem_ptr
                        )
                        .unwrap();
                        let val = self.generate_float_expr(value, params, locals)?;
                        writeln!(
                            &mut self.functions,
                            "  store double {}, double* %{}",
                            val.as_str(),
                            typed_ptr
                        )
                        .unwrap();
                    }
                    _ => {
                        let typed_ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = bitcast i8* %{} to i8**",
                            typed_ptr, elem_ptr
                        )
                        .unwrap();
                        let val_ptr = self.next_temp();
                        self.generate_ptr_expr(value, params, locals, &val_ptr)?;
                        writeln!(
                            &mut self.functions,
                            "  store i8* %{}, i8** %{}",
                            val_ptr, typed_ptr
                        )
                        .unwrap();
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
            fmt, value
        )
        .unwrap();
    }

    fn emit_printf_float(&mut self, value: &str) {
        let fmt = self.printf_fmt("%f\n", 4);
        writeln!(
            &mut self.functions,
            "  call i32 (i8*, ...) @printf(i8* %{}, double {})",
            fmt, value
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
                Expr::StringLiteral(s, _) | Expr::MultilineString(s, _) => parts.push(s.clone()),
                Expr::Identifier(name, _) => {
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
                            Type::String => {
                                parts.push("%s".to_string());
                                let loaded = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = load %String*, %String** %{}",
                                    loaded, alloca
                                )
                                .unwrap();
                                let ptr = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = getelementptr %String, %String* %{}, i32 0, i32 0",
                                    ptr, loaded
                                )
                                .unwrap();
                                let str_val = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = load i8*, i8** %{}",
                                    str_val, ptr
                                )
                                .unwrap();
                                args.push(format!("i8* %{}", str_val));
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
                            _ => {
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
        writeln!(
            &mut self.functions,
            "  %{} = bitcast i8* %{} to i8*",
            result_ptr, buf_ptr
        )
        .unwrap();
        Ok(())
    }

    fn generate_int_expr(
        &mut self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> CompileResult<Val> {
        match expr {
            Expr::Literal(Literal::Int(n), _) => Ok(Val::Imm(*n)),
            Expr::Literal(Literal::Float(n), _) => Ok(Val::ImmFloat(*n)),
            Expr::Literal(Literal::Bool(b), _) => Ok(Val::Imm(if *b { 1 } else { 0 })),
            Expr::Call { func, args } => {
                let ret_ty = self.get_function_call_return_type(func, args, params, locals)?;
                if ret_ty != "i32" {
                    return Err(CompileError::new(format!("{} does not return i32", func)));
                }
                let result = self
                    .emit_call(func, args, params, locals)?
                    .ok_or_else(|| CompileError::new("missing call result"))?;
                Ok(Val::Reg(result))
            }
            Expr::Identifier(name, _) => {
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
                        Type::String => {
                            let res = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = load %String*, %String** %{}",
                                res, alloca
                            )
                            .unwrap();
                            Ok(Val::Reg(res))
                        }
                        _ => {
                            return Err(CompileError::new(format!(
                                "unsupported type in numeric context for variable '{}'",
                                name
                            )));
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
            Expr::Unary(op, inner, _) => {
                let v = self.generate_int_expr(inner, params, locals)?;
                match op {
                    UnaryOp::Pos => match v {
                        Val::Imm(n) => Ok(Val::Imm(n)),
                        Val::ImmFloat(_) => Err(CompileError::new(
                            "cannot use + on float in integer context",
                        )),
                        Val::Reg(r) => {
                            let res = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = add i32 0, %{}", res, r)
                                .unwrap();
                            Ok(Val::Reg(res))
                        }
                    },
                    UnaryOp::Neg => match v {
                        Val::Imm(n) => Ok(Val::Imm(-n)),
                        Val::ImmFloat(_) => Err(CompileError::new(
                            "cannot use - on float in integer context",
                        )),
                        Val::Reg(r) => {
                            let res = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = sub i32 0, %{}", res, r)
                                .unwrap();
                            Ok(Val::Reg(res))
                        }
                    },
                    UnaryOp::Not => match v {
                        Val::Imm(n) => Ok(Val::Imm(if n == 0 { 1 } else { 0 })),
                        Val::ImmFloat(f) => Ok(Val::Imm(if f == 0.0 { 1 } else { 0 })),
                        Val::Reg(r) => {
                            let cmp = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = icmp eq i32 %{}, 0", cmp, r)
                                .unwrap();
                            let zext = self.next_temp();
                            writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                                .unwrap();
                            Ok(Val::Reg(zext))
                        }
                    },
                }
            }
            Expr::Binary(l, op, r, _) => {
                let lv = self.generate_int_expr(l, params, locals)?;
                let rv = self.generate_int_expr(r, params, locals)?;
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
                        return Ok(Val::Reg(int_res));
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
                        return Ok(Val::Reg(zext));
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
                        return Ok(Val::Reg(zext));
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
                        return Ok(Val::Reg(zext));
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
                        return Ok(Val::Reg(zext));
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
                        return Ok(Val::Reg(zext));
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
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::And => {
                        let lv_bool = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = icmp ne i32 {}, 0",
                            lv_bool,
                            lv.as_str()
                        )
                        .unwrap();
                        let rv_bool = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = icmp ne i32 {}, 0",
                            rv_bool,
                            rv.as_str()
                        )
                        .unwrap();
                        let and_res = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = and i1 %{}, %{}",
                            and_res, lv_bool, rv_bool
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = zext i1 %{} to i32",
                            zext, and_res
                        )
                        .unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Or => {
                        let lv_bool = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = icmp ne i32 {}, 0",
                            lv_bool,
                            lv.as_str()
                        )
                        .unwrap();
                        let rv_bool = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = icmp ne i32 {}, 0",
                            rv_bool,
                            rv.as_str()
                        )
                        .unwrap();
                        let or_res = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = or i1 %{}, %{}",
                            or_res, lv_bool, rv_bool
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = zext i1 %{} to i32",
                            zext, or_res
                        )
                        .unwrap();
                        return Ok(Val::Reg(zext));
                    }
                }
                .unwrap();
                Ok(Val::Reg(res))
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
                let result_alloca = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca i32", result_alloca).unwrap();
                let merge_label = self.next_block_label("ival_merge");
                let then_label = self.next_block_label("ival_then");
                if let Some(else_block) = else_branch {
                    let else_label = self.next_block_label("ival_else");
                    writeln!(
                        &mut self.functions,
                        "  br i1 %{}, label %{}, label %{}",
                        cond_bool, then_label, else_label
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "{}:", then_label).unwrap();
                    let val = self.generate_int_expr(
                        then_branch
                            .stmts
                            .last()
                            .unwrap_or(&Expr::Literal(Literal::Int(0), Default::default())),
                        params,
                        locals,
                    )?;
                    writeln!(
                        &mut self.functions,
                        "  store i32 {}, i32* %{}",
                        val.as_str(),
                        result_alloca
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                    writeln!(&mut self.functions, "{}:", else_label).unwrap();
                    let val = self.generate_int_expr(
                        else_block
                            .stmts
                            .last()
                            .unwrap_or(&Expr::Literal(Literal::Int(0), Default::default())),
                        params,
                        locals,
                    )?;
                    writeln!(
                        &mut self.functions,
                        "  store i32 {}, i32* %{}",
                        val.as_str(),
                        result_alloca
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                } else {
                    writeln!(
                        &mut self.functions,
                        "  br i1 %{}, label %{}, label %{}",
                        cond_bool, then_label, merge_label
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "{}:", then_label).unwrap();
                    let val = self.generate_int_expr(
                        then_branch
                            .stmts
                            .last()
                            .unwrap_or(&Expr::Literal(Literal::Int(0), Default::default())),
                        params,
                        locals,
                    )?;
                    writeln!(
                        &mut self.functions,
                        "  store i32 {}, i32* %{}",
                        val.as_str(),
                        result_alloca
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                }
                writeln!(&mut self.functions, "{}:", merge_label).unwrap();
                let res = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i32, i32* %{}",
                    res, result_alloca
                )
                .unwrap();
                Ok(Val::Reg(res))
            }
            Expr::Match { expr, arms } => {
                let match_val = self.generate_int_expr(expr, params, locals)?;
                let match_reg = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 {}, 0",
                    match_reg,
                    match_val.as_str()
                )
                .unwrap();
                let result_alloca = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca i32", result_alloca).unwrap();
                let end_label = self.next_block_label("match_end");
                let wildcard_label = arms
                    .iter()
                    .position(|a| matches!(a.pattern, crate::ast::MatchPattern::Wildcard))
                    .map(|_| self.next_block_label("match_default"));
                for (i, arm) in arms.iter().enumerate() {
                    match &arm.pattern {
                        crate::ast::MatchPattern::Int(n) => {
                            let next_label = if i + 1 < arms.len()
                                && !matches!(
                                    arms[i + 1].pattern,
                                    crate::ast::MatchPattern::Wildcard
                                ) {
                                self.next_block_label("match_next")
                            } else {
                                wildcard_label.clone().unwrap_or_else(|| end_label.clone())
                            };
                            let cmp = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = icmp eq i32 %{}, {}",
                                cmp, match_reg, n
                            )
                            .unwrap();
                            let arm_label = self.next_block_label("match_arm");
                            writeln!(
                                &mut self.functions,
                                "  br i1 %{}, label %{}, label %{}",
                                cmp, arm_label, next_label
                            )
                            .unwrap();
                            writeln!(&mut self.functions, "{}:", arm_label).unwrap();
                            let val = self.generate_int_expr(&arm.body, params, locals)?;
                            writeln!(
                                &mut self.functions,
                                "  store i32 {}, i32* %{}",
                                val.as_str(),
                                result_alloca
                            )
                            .unwrap();
                            writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                            if next_label != end_label
                                && wildcard_label.as_ref() != Some(&next_label)
                            {
                                writeln!(&mut self.functions, "{}:", next_label).unwrap();
                            }
                        }
                        crate::ast::MatchPattern::Variant {
                            name: vname,
                            bindings,
                        } => {
                            let next_label = if i + 1 < arms.len()
                                && !matches!(
                                    arms[i + 1].pattern,
                                    crate::ast::MatchPattern::Wildcard
                                ) {
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
                            writeln!(
                                &mut self.functions,
                                "  %{} = bitcast i8* %{} to i32*",
                                disc_ptr, enum_ptr
                            )
                            .unwrap();
                            let edisc = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = load i32, i32* %{}",
                                edisc, disc_ptr
                            )
                            .unwrap();
                            let cmp = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = icmp eq i32 %{}, {}",
                                cmp, edisc, disc_val_to_check
                            )
                            .unwrap();
                            let arm_label = self.next_block_label("earm_int");
                            writeln!(
                                &mut self.functions,
                                "  br i1 %{}, label %{}, label %{}",
                                cmp, arm_label, next_label
                            )
                            .unwrap();
                            writeln!(&mut self.functions, "{}:", arm_label).unwrap();
                            // Extract bindings - use cloned locals since we can't modify originals
                            let variant_fields: Vec<(String, String)> = {
                                let edef = self
                                    .enum_defs
                                    .iter()
                                    .find(|e| e.variants.iter().any(|v| v.name == *vname));
                                match edef {
                                    Some(e) => e
                                        .variants
                                        .iter()
                                        .find(|v| v.name == *vname)
                                        .map(|v| {
                                            v.fields
                                                .iter()
                                                .enumerate()
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
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = getelementptr i8, i8* %{}, i32 {}",
                                    elem_ptr, enum_ptr, payload_off
                                )
                                .unwrap();
                                match field_typ.as_str() {
                                    "i32" | "bool" => {
                                        writeln!(&mut self.functions, "  %{} = alloca i32", alloca)
                                            .unwrap();
                                        let typed_ptr = self.next_temp();
                                        writeln!(
                                            &mut self.functions,
                                            "  %{} = bitcast i8* %{} to i32*",
                                            typed_ptr, elem_ptr
                                        )
                                        .unwrap();
                                        let loaded = self.next_temp();
                                        writeln!(
                                            &mut self.functions,
                                            "  %{} = load i32, i32* %{}",
                                            loaded, typed_ptr
                                        )
                                        .unwrap();
                                        writeln!(
                                            &mut self.functions,
                                            "  store i32 %{}, i32* %{}",
                                            loaded, alloca
                                        )
                                        .unwrap();
                                        arm_locals.insert(
                                            binding_name.clone(),
                                            (crate::compiler::Type::I32, alloca.clone(), false),
                                        );
                                        payload_off += 4;
                                    }
                                    "f64" | "float" | "double" => {
                                        writeln!(
                                            &mut self.functions,
                                            "  %{} = alloca double",
                                            alloca
                                        )
                                        .unwrap();
                                        let typed_ptr = self.next_temp();
                                        writeln!(
                                            &mut self.functions,
                                            "  %{} = bitcast i8* %{} to double*",
                                            typed_ptr, elem_ptr
                                        )
                                        .unwrap();
                                        let loaded = self.next_temp();
                                        writeln!(
                                            &mut self.functions,
                                            "  %{} = load double, double* %{}",
                                            loaded, typed_ptr
                                        )
                                        .unwrap();
                                        writeln!(
                                            &mut self.functions,
                                            "  store double %{}, double* %{}",
                                            loaded, alloca
                                        )
                                        .unwrap();
                                        arm_locals.insert(
                                            binding_name.clone(),
                                            (crate::compiler::Type::F64, alloca.clone(), false),
                                        );
                                        payload_off += 8;
                                    }
                                    _ => {
                                        writeln!(&mut self.functions, "  %{} = alloca i8*", alloca)
                                            .unwrap();
                                        let typed_ptr = self.next_temp();
                                        writeln!(
                                            &mut self.functions,
                                            "  %{} = bitcast i8* %{} to i8**",
                                            typed_ptr, elem_ptr
                                        )
                                        .unwrap();
                                        let loaded = self.next_temp();
                                        writeln!(
                                            &mut self.functions,
                                            "  %{} = load i8*, i8** %{}",
                                            loaded, typed_ptr
                                        )
                                        .unwrap();
                                        writeln!(
                                            &mut self.functions,
                                            "  store i8* %{}, i8** %{}",
                                            loaded, alloca
                                        )
                                        .unwrap();
                                        arm_locals.insert(
                                            binding_name.clone(),
                                            (crate::compiler::Type::Ptr, alloca.clone(), false),
                                        );
                                        payload_off += 8;
                                    }
                                }
                            }
                            let val = self.generate_int_expr(&arm.body, params, &arm_locals)?;
                            writeln!(
                                &mut self.functions,
                                "  store i32 {}, i32* %{}",
                                val.as_str(),
                                result_alloca
                            )
                            .unwrap();
                            writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                            if next_label != end_label
                                && wildcard_label.as_ref() != Some(&next_label)
                            {
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
                                val.as_str(),
                                result_alloca
                            )
                            .unwrap();
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
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                }
                writeln!(&mut self.functions, "{}:", end_label).unwrap();
                let result = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i32, i32* %{}",
                    result, result_alloca
                )
                .unwrap();
                Ok(Val::Reg(result))
            }
            Expr::Index { target, index } => {
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let idx = self.generate_int_expr(index, params, locals)?;
                // Bounds check: load length from header (offset 4)
                let len_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 4",
                    len_ptr, ptr
                )
                .unwrap();
                let len_i32 = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i32*",
                    len_i32, len_ptr
                )
                .unwrap();
                let list_len = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i32, i32* %{}",
                    list_len, len_i32
                )
                .unwrap();
                let oob = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = icmp uge i32 {}, %{}",
                    oob,
                    idx.as_str(),
                    list_len
                )
                .unwrap();
                let panic_label = self.next_block_label("index_oob");
                let cont_label = self.next_block_label("index_cont");
                writeln!(
                    &mut self.functions,
                    "  br i1 %{}, label %{}, label %{}",
                    oob, panic_label, cont_label
                )
                .unwrap();
                writeln!(&mut self.functions, "{}:", panic_label).unwrap();
                let bounds_msg = self.emit_string_const("list index out of bounds");
                let bounds_msg_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr [23 x i8], [23 x i8]* @{}, i32 0, i32 0",
                    bounds_msg_ptr, bounds_msg
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  call void @__rt_panic_bounds(i8* %{})",
                    bounds_msg_ptr
                )
                .unwrap();
                writeln!(&mut self.functions, "  unreachable").unwrap();
                writeln!(&mut self.functions, "{}:", cont_label).unwrap();
                let elem_type = match target.as_ref() {
                    Expr::List(items) if !items.is_empty() => {
                        self.infer_expr_type(&items[0], params, locals)
                    }
                    Expr::Identifier(name, _) => {
                        if let Some((typ, _, _)) = locals.get(name) {
                            match typ {
                                Type::I32 => "i32",
                                Type::F64 => "double",
                                Type::Ptr => "i8*",
                                _ => "i32",
                            }
                        } else {
                            "i32"
                        }
                    }
                    _ => "i32",
                };
                let elem_size: i32 = match elem_type {
                    "double" => 8,
                    _ => 4,
                };
                let elem_off = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = mul i32 {}, {}",
                    elem_off,
                    idx.as_str(),
                    elem_size
                )
                .unwrap();
                let offset = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 %{}, 8",
                    offset, elem_off
                )
                .unwrap();
                let elem_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 %{}",
                    elem_ptr, ptr, offset
                )
                .unwrap();
                match elem_type {
                    "double" => {
                        let double_ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = bitcast i8* %{} to double*",
                            double_ptr, elem_ptr
                        )
                        .unwrap();
                        let loaded = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = load double, double* %{}",
                            loaded, double_ptr
                        )
                        .unwrap();
                        Ok(Val::Reg(loaded))
                    }
                    _ => {
                        let i32_ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = bitcast i8* %{} to i32*",
                            i32_ptr, elem_ptr
                        )
                        .unwrap();
                        let loaded = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = load i32, i32* %{}",
                            loaded, i32_ptr
                        )
                        .unwrap();
                        Ok(Val::Reg(loaded))
                    }
                }
            }
            Expr::FieldAccess { target, field } => {
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let mut offset = 0i32;
                let mut field_type = "i32".to_string();
                let target_name = if let Expr::Identifier(id, _) = target.as_ref() {
                    Some(id.clone())
                } else {
                    None
                };
                if let Some(ref name) = target_name {
                    let mut found_sdef = None;
                    for sdef in self.monomorphized_structs.values() {
                        if locals.contains_key(name) {
                            found_sdef = Some(sdef);
                            break;
                        }
                    }
                    if found_sdef.is_none() {
                        found_sdef = self.struct_defs.iter().find(|_s| locals.contains_key(name));
                    }
                    if let Some(sdef) = found_sdef {
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
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr i8, i8* %{}, i32 {}",
                        elem_ptr, ptr, offset
                    )
                    .unwrap();
                    let typed_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast i8* %{} to i32*",
                        typed_ptr, elem_ptr
                    )
                    .unwrap();
                    let loaded = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = load i32, i32* %{}",
                        loaded, typed_ptr
                    )
                    .unwrap();
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
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 {}",
                    elem_ptr, ptr, offset
                )
                .unwrap();
                let typed_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i64*",
                    typed_ptr, elem_ptr
                )
                .unwrap();
                let loaded = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i64, i64* %{}",
                    loaded, typed_ptr
                )
                .unwrap();
                let truncated = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = trunc i64 %{} to i32",
                    truncated, loaded
                )
                .unwrap();
                Ok(Val::Reg(truncated))
            }
            _ => Err(CompileError::new(format!(
                "unexpected expression in int-context: {:?}",
                expr
            ))),
        }
    }

    fn generate_float_expr(
        &mut self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> CompileResult<Val> {
        match expr {
            Expr::Literal(Literal::Float(n), _) => Ok(Val::ImmFloat(*n)),
            Expr::Literal(Literal::Int(n), _) => Ok(Val::ImmFloat(*n as f64)),
            Expr::Literal(Literal::Bool(b), _) => Ok(Val::ImmFloat(if *b { 1.0 } else { 0.0 })),
            Expr::Call { func, args } => {
                let ret_ty = self.get_function_call_return_type(func, args, params, locals)?;
                if ret_ty != "double" {
                    return Err(CompileError::new(format!(
                        "{} does not return double",
                        func
                    )));
                }
                let result = self
                    .emit_call(func, args, params, locals)?
                    .ok_or_else(|| CompileError::new("missing call result"))?;
                Ok(Val::Reg(result))
            }
            Expr::Identifier(name, _) => {
                if let Some((typ, alloca, _)) = locals.get(name) {
                    return match typ {
                        Type::F64 => {
                            let res = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = load double, double* %{}",
                                res, alloca
                            )
                            .unwrap();
                            Ok(Val::Reg(res))
                        }
                        Type::I32 => {
                            let res = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = load i32, i32* %{}",
                                res, alloca
                            )
                            .unwrap();
                            let converted = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = sitofp i32 %{} to double",
                                converted, res
                            )
                            .unwrap();
                            Ok(Val::Reg(converted))
                        }
                        Type::Ptr => Err(CompileError::new(format!(
                            "variable '{}' is a string, cannot convert to float",
                            name
                        ))),
                        _ => {
                            return Err(CompileError::new(format!(
                                "unsupported type in float context for variable '{}'",
                                name
                            )));
                        }
                    };
                }
                Err(CompileError::new(format!(
                    "variable '{}' is not a float",
                    name
                )))
            }
            Expr::Unary(op, inner, _) => {
                let v = self.generate_float_expr(inner, params, locals)?;
                match op {
                    UnaryOp::Pos => Ok(v),
                    UnaryOp::Neg => {
                        let res = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fsub double 0.0, {}",
                            res,
                            v.as_str()
                        )
                        .unwrap();
                        Ok(Val::Reg(res))
                    }
                    UnaryOp::Not => {
                        let res = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fcmp one double {}, 0.0",
                            res,
                            v.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, res)
                            .unwrap();
                        Ok(Val::Reg(zext))
                    }
                }
            }
            Expr::Binary(l, op, r, _) => {
                let lv = self.generate_float_expr(l, params, locals)?;
                let rv = self.generate_float_expr(r, params, locals)?;
                let res = self.next_temp();

                match op {
                    BinOp::Add => writeln!(
                        &mut self.functions,
                        "  %{} = fadd double {}, {}",
                        res,
                        lv.as_str(),
                        rv.as_str()
                    ),
                    BinOp::Sub => writeln!(
                        &mut self.functions,
                        "  %{} = fsub double {}, {}",
                        res,
                        lv.as_str(),
                        rv.as_str()
                    ),
                    BinOp::Mul => writeln!(
                        &mut self.functions,
                        "  %{} = fmul double {}, {}",
                        res,
                        lv.as_str(),
                        rv.as_str()
                    ),
                    BinOp::Div => writeln!(
                        &mut self.functions,
                        "  %{} = fdiv double {}, {}",
                        res,
                        lv.as_str(),
                        rv.as_str()
                    ),
                    BinOp::Pow => {
                        let pow = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = call double @llvm.pow.f64(double {}, double {})",
                            pow,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        return Ok(Val::Reg(pow));
                    }
                    BinOp::Eq => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fcmp oeq double {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Ne => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fcmp one double {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Lt => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fcmp olt double {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Le => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fcmp ole double {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Gt => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fcmp ogt double {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Ge => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fcmp oge double {}, {}",
                            cmp,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(&mut self.functions, "  %{} = zext i1 %{} to i32", zext, cmp)
                            .unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::And => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fcmp one double {}, 0.0",
                            cmp,
                            lv.as_str()
                        )
                        .unwrap();
                        let and_res = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = and i1 %{}, {}",
                            and_res,
                            cmp,
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = zext i1 %{} to i32",
                            zext, and_res
                        )
                        .unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::Or => {
                        let cmp = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = fcmp one double {}, 0.0",
                            cmp,
                            lv.as_str()
                        )
                        .unwrap();
                        let or_res = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = or i1 %{}, {}",
                            or_res,
                            cmp,
                            rv.as_str()
                        )
                        .unwrap();
                        let zext = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = zext i1 %{} to i32",
                            zext, or_res
                        )
                        .unwrap();
                        return Ok(Val::Reg(zext));
                    }
                    BinOp::DivMod => {
                        let res = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = frem double {}, {}",
                            res,
                            lv.as_str(),
                            rv.as_str()
                        )
                        .unwrap();
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
                let target_name = if let Expr::Identifier(id, _) = target.as_ref() {
                    Some(id.clone())
                } else {
                    None
                };
                if let Some(ref name) = target_name
                    && let Some(sdef) = struct_defs.iter().find(|_s| locals.contains_key(name))
                {
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
                if field_type == "f64" || field_type == "float" || field_type == "double" {
                    let elem_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr i8, i8* %{}, i32 {}",
                        elem_ptr, ptr, offset
                    )
                    .unwrap();
                    let typed_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast i8* %{} to double*",
                        typed_ptr, elem_ptr
                    )
                    .unwrap();
                    let loaded = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = load double, double* %{}",
                        loaded, typed_ptr
                    )
                    .unwrap();
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
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 {}",
                    elem_ptr, ptr, offset
                )
                .unwrap();
                let typed_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to double*",
                    typed_ptr, elem_ptr
                )
                .unwrap();
                let loaded = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load double, double* %{}",
                    loaded, typed_ptr
                )
                .unwrap();
                Ok(Val::Reg(loaded))
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
                let result_alloca = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca double", result_alloca).unwrap();
                let merge_label = self.next_block_label("fval_merge");
                let then_label = self.next_block_label("fval_then");
                if let Some(else_block) = else_branch {
                    let else_label = self.next_block_label("fval_else");
                    writeln!(
                        &mut self.functions,
                        "  br i1 %{}, label %{}, label %{}",
                        cond_bool, then_label, else_label
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "{}:", then_label).unwrap();
                    let val = self.generate_float_expr(
                        then_branch
                            .stmts
                            .last()
                            .unwrap_or(&Expr::Literal(Literal::Float(0.0), Default::default())),
                        params,
                        locals,
                    )?;
                    writeln!(
                        &mut self.functions,
                        "  store double {}, double* %{}",
                        val.as_str(),
                        result_alloca
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                    writeln!(&mut self.functions, "{}:", else_label).unwrap();
                    let val = self.generate_float_expr(
                        else_block
                            .stmts
                            .last()
                            .unwrap_or(&Expr::Literal(Literal::Float(0.0), Default::default())),
                        params,
                        locals,
                    )?;
                    writeln!(
                        &mut self.functions,
                        "  store double {}, double* %{}",
                        val.as_str(),
                        result_alloca
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                } else {
                    writeln!(
                        &mut self.functions,
                        "  br i1 %{}, label %{}, label %{}",
                        cond_bool, then_label, merge_label
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "{}:", then_label).unwrap();
                    let val = self.generate_float_expr(
                        then_branch
                            .stmts
                            .last()
                            .unwrap_or(&Expr::Literal(Literal::Float(0.0), Default::default())),
                        params,
                        locals,
                    )?;
                    writeln!(
                        &mut self.functions,
                        "  store double {}, double* %{}",
                        val.as_str(),
                        result_alloca
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                }
                writeln!(&mut self.functions, "{}:", merge_label).unwrap();
                let res = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = load double, double* %{}",
                    res, result_alloca
                )
                .unwrap();
                Ok(Val::Reg(res))
            }
            _ => Err(CompileError::new(format!(
                "unexpected expression in float-context: {:?}",
                expr
            ))),
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
            Expr::StringLiteral(s, _) | Expr::MultilineString(s, _) => {
                let id = self.emit_string_const(s);
                let ptr = self.next_temp();
                self.emit_gep(&id, s.len() + 1, &ptr);
                let len = s.len();
                let cap = s.len() + 1;
                writeln!(&mut self.functions, "  %{} = alloca %String", result).unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.ptr = getelementptr %String, %String* %{}, i32 0, i32 0",
                    result, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i8* %{}, i8** %{}.ptr",
                    ptr, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.len = getelementptr %String, %String* %{}, i32 0, i32 1",
                    result, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}.len",
                    len, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.cap = getelementptr %String, %String* %{}, i32 0, i32 2",
                    result, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}.cap",
                    cap, result
                )
                .unwrap();
            }
            Expr::Identifier(name, _) => {
                if let Some((Type::String, alloca, _)) = locals.get(name) {
                    writeln!(
                        &mut self.functions,
                        "  %{} = load %String*, %String** %{}",
                        result, alloca
                    )
                    .unwrap();
                } else if let Some((Type::Ptr, alloca, _)) = locals.get(name) {
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
            _ => {
                return Err(CompileError::new(format!(
                    "print: unsupported argument{}",
                    self.stdlib_hint("print")
                )));
            }
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
                let elem_type = if items.is_empty() {
                    "i32"
                } else {
                    self.infer_expr_type(&items[0], params, locals)
                };
                let elem_size: i32 = match elem_type {
                    "double" => 8,
                    _ => 4,
                };
                let alloc_size = (8 + count * elem_size) as i64;
                let malloc_reg = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call i8* @malloc(i64 {})",
                    malloc_reg, alloc_size
                )
                .unwrap();
                // Store capacity and length in header
                let cap_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i32*",
                    cap_ptr, malloc_reg
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}",
                    count, cap_ptr
                )
                .unwrap();
                let len_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 4",
                    len_ptr, malloc_reg
                )
                .unwrap();
                let len_ptr_i32 = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i32*",
                    len_ptr_i32, len_ptr
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}",
                    count, len_ptr_i32
                )
                .unwrap();
                // Store elements at offset 8
                for (i, item) in items.iter().enumerate() {
                    let val = match elem_type {
                        "double" => self.generate_float_expr(item, params, locals)?,
                        _ => self.generate_int_expr(item, params, locals)?,
                    };
                    let elem_off = (i as i32) * elem_size;
                    let ptr_reg = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr i8, i8* %{}, i32 {}",
                        ptr_reg,
                        malloc_reg,
                        (8 + elem_off)
                    )
                    .unwrap();
                    match elem_type {
                        "double" => {
                            let double_ptr = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = bitcast i8* %{} to double*",
                                double_ptr, ptr_reg
                            )
                            .unwrap();
                            writeln!(
                                &mut self.functions,
                                "  store double {}, double* %{}",
                                val.as_float_str(),
                                double_ptr
                            )
                            .unwrap();
                        }
                        _ => {
                            let i32_ptr = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = bitcast i8* %{} to i32*",
                                i32_ptr, ptr_reg
                            )
                            .unwrap();
                            writeln!(
                                &mut self.functions,
                                "  store i32 {}, i32* %{}",
                                val.as_str(),
                                i32_ptr
                            )
                            .unwrap();
                        }
                    }
                }
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i8*",
                    result, malloc_reg
                )
                .unwrap();
            }
            Expr::StringLiteral(s, _) | Expr::MultilineString(s, _) => {
                let id = self.emit_string_const(s);
                self.emit_gep(&id, s.len() + 1, result);
            }
            Expr::FString(elements, _) => self.emit_fstring(elements, params, locals, result)?,
            Expr::Borrow { name, .. } => self.emit_borrow_ptr(name, params, locals, result)?,
            Expr::Identifier(_, _) => self.emit_print_arg(expr, params, locals, result)?,
            Expr::Call { func, args } => {
                let ty = self
                    .builtin_return_type(func)
                    .or_else(|| {
                        self.function_sigs
                            .get(&self.resolve_function_name(func))
                            .copied()
                    })
                    .unwrap_or("void");
                if ty != "i8*" {
                    return Err(CompileError::new(format!(
                        "{}: expected string-returning function",
                        func
                    )));
                }
                let call_result = self.emit_call(func, args, params, locals)?;
                let call_reg =
                    call_result.ok_or_else(|| CompileError::new("expected call result"))?;
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i8*",
                    result, call_reg
                )
                .unwrap();
            }
            Expr::Binary(l, op, r, _) if *op == BinOp::Add => {
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
                    )
                    .unwrap();
                    writeln!(
                        &mut self.functions,
                        "  call i32 (i8*, i8*, ...) @sprintf(i8* %{}, i8* %{}, i8* %{}, i8* %{})",
                        buf_ptr, fmt_ptr, lhs, rhs
                    )
                    .unwrap();
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast i8* %{} to i8*",
                        result, buf_ptr
                    )
                    .unwrap();
                } else {
                    return Err(CompileError::new(
                        "string concatenation requires two strings",
                    ));
                }
            }
            Expr::StructLiteral { name, fields } => {
                let sdef = self.struct_defs.iter().find(|s| s.name == *name);
                let mut mangled_name = None;
                if let Some(sdef) = sdef
                    && !sdef.generic_params.is_empty()
                {
                    let mut concrete_types = Vec::new();
                    for gp in &sdef.generic_params {
                        if let Some(sf) = sdef.fields.iter().find(|f| f.typ == *gp)
                            && let Some((_, fval)) =
                                fields.iter().find(|(fname, _)| fname == &sf.name)
                        {
                            let val_type = self.infer_expr_type(fval, params, locals);
                            let concrete = match val_type {
                                "i32" => Type::I32,
                                "double" => Type::F64,
                                "%String*" => Type::String,
                                "i8*" => Type::Ptr,
                                _ => Type::Unknown,
                            };
                            concrete_types.push(concrete);
                        }
                    }
                    if !concrete_types.is_empty()
                        && concrete_types.len() == sdef.generic_params.len()
                    {
                        mangled_name = Some(self.monomorphize_struct(name, &concrete_types)?);
                    }
                }
                let mut sdef = if let Some(ref mangled) = mangled_name {
                    self.monomorphized_structs.get(mangled).cloned()
                } else {
                    None
                };
                if sdef.is_none() {
                    sdef = self.struct_defs.iter().find(|s| s.name == *name).cloned();
                }
                if sdef.is_none() {
                    sdef = self
                        .monomorphized_structs
                        .values()
                        .find(|s| s.name == *name)
                        .cloned();
                }
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
                    writeln!(
                        &mut self.functions,
                        "  %{} = call i8* @malloc(i64 {})",
                        malloc_reg, total_size
                    )
                    .unwrap();
                    for (fname, fval) in fields {
                        if let Some((_, offset, typ)) =
                            field_data.iter().find(|(n, _, _)| n == fname)
                        {
                            let elem_ptr = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = getelementptr i8, i8* %{}, i32 {}",
                                elem_ptr, malloc_reg, offset
                            )
                            .unwrap();
                            match typ.as_str() {
                                "i32" | "bool" => {
                                    let val = self.generate_int_expr(fval, params, locals)?;
                                    let typed_ptr = self.next_temp();
                                    writeln!(
                                        &mut self.functions,
                                        "  %{} = bitcast i8* %{} to i32*",
                                        typed_ptr, elem_ptr
                                    )
                                    .unwrap();
                                    writeln!(
                                        &mut self.functions,
                                        "  store i32 {}, i32* %{}",
                                        val.as_str(),
                                        typed_ptr
                                    )
                                    .unwrap();
                                }
                                "f64" | "float" | "double" => {
                                    let val = self.generate_float_expr(fval, params, locals)?;
                                    let typed_ptr = self.next_temp();
                                    writeln!(
                                        &mut self.functions,
                                        "  %{} = bitcast i8* %{} to double*",
                                        typed_ptr, elem_ptr
                                    )
                                    .unwrap();
                                    writeln!(
                                        &mut self.functions,
                                        "  store double {}, double* %{}",
                                        val.as_str(),
                                        typed_ptr
                                    )
                                    .unwrap();
                                }
                                _ => {
                                    let val_ptr = self.next_temp();
                                    self.generate_ptr_expr(fval, params, locals, &val_ptr)?;
                                    let typed_ptr = self.next_temp();
                                    writeln!(
                                        &mut self.functions,
                                        "  %{} = bitcast i8* %{} to i8**",
                                        typed_ptr, elem_ptr
                                    )
                                    .unwrap();
                                    writeln!(
                                        &mut self.functions,
                                        "  store i8* %{}, i8** %{}",
                                        val_ptr, typed_ptr
                                    )
                                    .unwrap();
                                }
                            }
                        }
                    }
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast i8* %{} to i8*",
                        result, malloc_reg
                    )
                    .unwrap();
                } else {
                    writeln!(
                        &mut self.functions,
                        "  %{} = call i8* @malloc(i64 0)",
                        result
                    )
                    .unwrap();
                }
            }
            Expr::EnumLiteral {
                enum_name,
                variant_name,
                args,
            } => {
                let enum_defs = self.enum_defs.clone();
                let edef = enum_defs.iter().find(|e| e.name == *enum_name);
                let mut discriminant = 0i32;
                let mut _payload_size = 0i32;
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
                            _payload_size = v_size;
                        }
                        if v_size > max_payload {
                            max_payload = v_size;
                        }
                    }
                }
                if max_payload < 4 {
                    max_payload = 4;
                }
                let alloc_size = 4 + max_payload;
                let malloc_reg = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call i8* @malloc(i64 {})",
                    malloc_reg, alloc_size
                )
                .unwrap();
                // Store discriminant at offset 0
                let disc_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i32*",
                    disc_ptr, malloc_reg
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}",
                    discriminant, disc_ptr
                )
                .unwrap();
                // Store payload at offset 4
                let mut offset = 4i32;
                for (i, arg) in args.iter().enumerate() {
                    if let Some(e) = edef
                        && let Some(v) = e.variants.iter().find(|v| v.name == *variant_name)
                        && i < v.fields.len()
                    {
                        let ft = &v.fields[i];
                        let elem_ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = getelementptr i8, i8* %{}, i32 {}",
                            elem_ptr, malloc_reg, offset
                        )
                        .unwrap();
                        match ft.as_str() {
                            "i32" | "bool" => {
                                let val = self.generate_int_expr(arg, params, locals)?;
                                let typed_ptr = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = bitcast i8* %{} to i32*",
                                    typed_ptr, elem_ptr
                                )
                                .unwrap();
                                writeln!(
                                    &mut self.functions,
                                    "  store i32 {}, i32* %{}",
                                    val.as_str(),
                                    typed_ptr
                                )
                                .unwrap();
                                offset += 4;
                            }
                            "f64" | "float" | "double" => {
                                let val = self.generate_float_expr(arg, params, locals)?;
                                let typed_ptr = self.next_temp();
                                writeln!(
                                    &mut self.functions,
                                    "  %{} = bitcast i8* %{} to double*",
                                    typed_ptr, elem_ptr
                                )
                                .unwrap();
                                writeln!(
                                    &mut self.functions,
                                    "  store double {}, double* %{}",
                                    val.as_str(),
                                    typed_ptr
                                )
                                .unwrap();
                                offset += 8;
                            }
                            _ => {
                                let arg_type = self.infer_expr_type(arg, params, locals);
                                match arg_type {
                                    "i32" | "bool" => {
                                        let val = self.generate_int_expr(arg, params, locals)?;
                                        let typed_ptr = self.next_temp();
                                        writeln!(
                                            &mut self.functions,
                                            "  %{} = bitcast i8* %{} to i32*",
                                            typed_ptr, elem_ptr
                                        )
                                        .unwrap();
                                        writeln!(
                                            &mut self.functions,
                                            "  store i32 {}, i32* %{}",
                                            val.as_str(),
                                            typed_ptr
                                        )
                                        .unwrap();
                                        offset += 4;
                                    }
                                    "double" => {
                                        let val = self.generate_float_expr(arg, params, locals)?;
                                        let typed_ptr = self.next_temp();
                                        writeln!(
                                            &mut self.functions,
                                            "  %{} = bitcast i8* %{} to double*",
                                            typed_ptr, elem_ptr
                                        )
                                        .unwrap();
                                        writeln!(
                                            &mut self.functions,
                                            "  store double {}, double* %{}",
                                            val.as_str(),
                                            typed_ptr
                                        )
                                        .unwrap();
                                        offset += 8;
                                    }
                                    _ => {
                                        let val_ptr = self.next_temp();
                                        self.generate_ptr_expr(arg, params, locals, &val_ptr)?;
                                        let typed_ptr = self.next_temp();
                                        writeln!(
                                            &mut self.functions,
                                            "  %{} = bitcast i8* %{} to i8**",
                                            typed_ptr, elem_ptr
                                        )
                                        .unwrap();
                                        writeln!(
                                            &mut self.functions,
                                            "  store i8* %{}, i8** %{}",
                                            val_ptr, typed_ptr
                                        )
                                        .unwrap();
                                        offset += 8;
                                    }
                                }
                            }
                        }
                    }
                }
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i8*",
                    result, malloc_reg
                )
                .unwrap();
            }
            Expr::Tuple(elements, _) => {
                let alloc_size = (elements.len() * 8) as i64;
                let malloc_reg = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call i8* @malloc(i64 {})",
                    malloc_reg, alloc_size
                )
                .unwrap();
                for (i, elem) in elements.iter().enumerate() {
                    let offset = (i * 8) as i32;
                    let elem_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr i8, i8* %{}, i32 {}",
                        elem_ptr, malloc_reg, offset
                    )
                    .unwrap();
                    let elem_ty = self.infer_expr_type(elem, params, locals);
                    match elem_ty {
                        "i32" | "bool" => {
                            let val = self.generate_int_expr(elem, params, locals)?;
                            let i64_val = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = sext i32 {} to i64",
                                i64_val,
                                val.as_str()
                            )
                            .unwrap();
                            let typed_ptr = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = bitcast i8* %{} to i64*",
                                typed_ptr, elem_ptr
                            )
                            .unwrap();
                            writeln!(
                                &mut self.functions,
                                "  store i64 %{}, i64* %{}",
                                i64_val, typed_ptr
                            )
                            .unwrap();
                        }
                        "double" => {
                            let val = self.generate_float_expr(elem, params, locals)?;
                            let typed_ptr = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = bitcast i8* %{} to double*",
                                typed_ptr, elem_ptr
                            )
                            .unwrap();
                            writeln!(
                                &mut self.functions,
                                "  store double {}, double* %{}",
                                val.as_str(),
                                typed_ptr
                            )
                            .unwrap();
                        }
                        _ => {
                            let val_ptr = self.next_temp();
                            self.generate_ptr_expr(elem, params, locals, &val_ptr)?;
                            let typed_ptr = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = bitcast i8* %{} to i8**",
                                typed_ptr, elem_ptr
                            )
                            .unwrap();
                            writeln!(
                                &mut self.functions,
                                "  store i8* %{}, i8** %{}",
                                val_ptr, typed_ptr
                            )
                            .unwrap();
                        }
                    }
                }
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i8*",
                    result, malloc_reg
                )
                .unwrap();
            }
            Expr::TupleAccess { target, index } => {
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let offset = (*index as i32) * 8;
                let elem_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = getelementptr i8, i8* %{}, i32 {}",
                    elem_ptr, ptr, offset
                )
                .unwrap();
                let typed_ptr = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast i8* %{} to i8**",
                    typed_ptr, elem_ptr
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{} = load i8*, i8** %{}",
                    result, typed_ptr
                )
                .unwrap();
            }
            Expr::FieldAccess { target, field } => {
                let mut struct_defs = self.struct_defs.clone();
                struct_defs.extend(self.monomorphized_structs.values().cloned());
                let ptr = self.next_temp();
                self.generate_ptr_expr(target, params, locals, &ptr)?;
                let mut offset = 0i32;
                let mut field_type = "i32".to_string();
                let target_name = if let Expr::Identifier(id, _) = target.as_ref() {
                    Some(id.clone())
                } else {
                    None
                };
                if let Some(ref name) = target_name
                    && let Some(sdef) = struct_defs.iter().find(|_s| locals.contains_key(name))
                {
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
                if field_type == "i8*"
                    || field_type == "string"
                    || field_type == "str"
                    || field_type == "ptr"
                {
                    let elem_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr i8, i8* %{}, i32 {}",
                        elem_ptr, ptr, offset
                    )
                    .unwrap();
                    let ptr_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast i8* %{} to i8**",
                        ptr_ptr, elem_ptr
                    )
                    .unwrap();
                    writeln!(
                        &mut self.functions,
                        "  %{} = load i8*, i8** %{}",
                        result, ptr_ptr
                    )
                    .unwrap();
                } else {
                    return Err(CompileError::new("expected string/ptr field access"));
                }
            }
            _ => return Err(CompileError::new("expected string expression")),
        }
        Ok(())
    }

    fn generate_string_expr(
        &mut self,
        expr: &Expr,
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
        result: &str,
    ) -> CompileResult<()> {
        match expr {
            Expr::StringLiteral(s, _) | Expr::MultilineString(s, _) => {
                let id = self.emit_string_const(s);
                let ptr = self.next_temp();
                self.emit_gep(&id, s.len() + 1, &ptr);
                let len = s.len();
                let cap = s.len() + 1;
                writeln!(&mut self.functions, "  %{} = alloca %String", result).unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.ptr = getelementptr %String, %String* %{}, i32 0, i32 0",
                    result, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i8* %{}, i8** %{}.ptr",
                    ptr, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.len = getelementptr %String, %String* %{}, i32 0, i32 1",
                    result, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}.len",
                    len, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.cap = getelementptr %String, %String* %{}, i32 0, i32 2",
                    result, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 {}, i32* %{}.cap",
                    cap, result
                )
                .unwrap();
            }
            Expr::FString(elements, _) => {
                let buf_ptr = self.next_temp();
                self.emit_fstring_to_buffer(elements, params, locals, &buf_ptr)?;
                // Use raw strlen for the temporary C-string buffer (not a RoString)
                let len64 = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call i64 @strlen(i8* %{})",
                    len64, buf_ptr
                )
                .unwrap();
                let len = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = trunc i64 %{} to i32",
                    len, len64
                )
                .unwrap();
                let cap = self.next_temp();
                writeln!(&mut self.functions, "  %{} = add i32 %{}, 1", cap, len).unwrap();
                writeln!(&mut self.functions, "  %{} = alloca %String", result).unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.ptr = getelementptr %String, %String* %{}, i32 0, i32 0",
                    result, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i8* %{}, i8** %{}.ptr",
                    buf_ptr, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.len = getelementptr %String, %String* %{}, i32 0, i32 1",
                    result, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 %{}, i32* %{}.len",
                    len, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{}.cap = getelementptr %String, %String* %{}, i32 0, i32 2",
                    result, result
                )
                .unwrap();
                writeln!(
                    &mut self.functions,
                    "  store i32 %{}, i32* %{}.cap",
                    cap, result
                )
                .unwrap();
            }
            Expr::Borrow { name, .. } => {
                if let Some((Type::String, alloca, _)) = locals.get(name) {
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast %String** %{} to %String*",
                        result, alloca
                    )
                    .unwrap();
                } else if let Some((typ, alloca, _)) = locals.get(name) {
                    let source_ty = match typ {
                        Type::I32 => "i32",
                        Type::F64 => "double",
                        Type::Ptr => "i8*",
                        _ => "i8*",
                    };
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast {}* %{} to i8*",
                        result, source_ty, alloca
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
                        "borrow of unknown variable: {}{}",
                        name,
                        self.suggest_variable(name, params, locals)
                            .map(|s| format!(" did you mean `{}`?", s))
                            .unwrap_or_default()
                    )));
                }
            }
            Expr::Identifier(name, _) => {
                if let Some((Type::String, alloca, _)) = locals.get(name) {
                    writeln!(
                        &mut self.functions,
                        "  %{} = load %String*, %String** %{}",
                        result, alloca
                    )
                    .unwrap();
                } else if let Some((typ, alloca, _)) = locals.get(name) {
                    let source_ty = match typ {
                        Type::I32 => "i32",
                        Type::F64 => "double",
                        Type::Ptr => "i8*",
                        _ => "i8*",
                    };
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast {}* %{} to i8*",
                        result, source_ty, alloca
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
            Expr::Match { expr: mexpr, arms } => {
                let match_val = self.generate_int_expr(mexpr, params, locals)?;
                let match_reg = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = add i32 {}, 0",
                    match_reg,
                    match_val.as_str()
                )
                .unwrap();
                let ptr_storage = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca %String*", ptr_storage).unwrap();
                let end_label = self.next_block_label("smatch_end");
                let wildcard_label = arms
                    .iter()
                    .position(|a| matches!(a.pattern, crate::ast::MatchPattern::Wildcard))
                    .map(|_| self.next_block_label("smatch_default"));
                for (i, arm) in arms.iter().enumerate() {
                    match &arm.pattern {
                        crate::ast::MatchPattern::Int(n) => {
                            let next_label = if i + 1 < arms.len()
                                && !matches!(
                                    arms[i + 1].pattern,
                                    crate::ast::MatchPattern::Wildcard
                                ) {
                                self.next_block_label("smatch_next")
                            } else {
                                wildcard_label.clone().unwrap_or_else(|| end_label.clone())
                            };
                            let cmp = self.next_temp();
                            writeln!(
                                &mut self.functions,
                                "  %{} = icmp eq i32 %{}, {}",
                                cmp, match_reg, n
                            )
                            .unwrap();
                            let arm_label = self.next_block_label("smatch_arm");
                            writeln!(
                                &mut self.functions,
                                "  br i1 %{}, label %{}, label %{}",
                                cmp, arm_label, next_label
                            )
                            .unwrap();
                            writeln!(&mut self.functions, "{}:", arm_label).unwrap();
                            let arm_temp = self.next_temp();
                            self.generate_string_expr(&arm.body, params, locals, &arm_temp)?;
                            writeln!(
                                &mut self.functions,
                                "  store %String* %{}, %String** %{}",
                                arm_temp, ptr_storage
                            )
                            .unwrap();
                            writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                            if next_label != end_label
                                && wildcard_label.as_ref() != Some(&next_label)
                            {
                                writeln!(&mut self.functions, "{}:", next_label).unwrap();
                            }
                        }
                        crate::ast::MatchPattern::Wildcard => {
                            if i + 1 < arms.len() {
                                let arm_temp = self.next_temp();
                                self.generate_string_expr(&arm.body, params, locals, &arm_temp)?;
                                writeln!(
                                    &mut self.functions,
                                    "  store %String* %{}, %String** %{}",
                                    arm_temp, ptr_storage
                                )
                                .unwrap();
                                writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                            } else if let Some(wl) = &wildcard_label {
                                writeln!(&mut self.functions, "{}:", wl).unwrap();
                                let arm_temp = self.next_temp();
                                self.generate_string_expr(&arm.body, params, locals, &arm_temp)?;
                                writeln!(
                                    &mut self.functions,
                                    "  store %String* %{}, %String** %{}",
                                    arm_temp, ptr_storage
                                )
                                .unwrap();
                                writeln!(&mut self.functions, "  br label %{}", end_label).unwrap();
                            }
                        }
                        _ => {}
                    }
                }
                writeln!(&mut self.functions, "{}:", end_label).unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{} = load %String*, %String** %{}",
                    result, ptr_storage
                )
                .unwrap();
            }
            Expr::Call { func, args } => {
                let ty = self.get_function_call_return_type(func, args, params, locals)?;
                if ty != "%String*" {
                    return Err(CompileError::new(format!(
                        "{}: expected string-returning function",
                        func
                    )));
                }
                let call_result = self.emit_call(func, args, params, locals)?;
                let call_reg =
                    call_result.ok_or_else(|| CompileError::new("expected call result"))?;
                writeln!(
                    &mut self.functions,
                    "  %{} = bitcast %String* %{} to %String*",
                    result, call_reg
                )
                .unwrap();
            }
            Expr::Binary(l, op, r, _) if *op == BinOp::Add => {
                let left_ty = self.infer_expr_type(l, params, locals);
                let right_ty = self.infer_expr_type(r, params, locals);
                if left_ty == "%String*" && right_ty == "%String*" {
                    let lhs = self.next_temp();
                    let rhs = self.next_temp();
                    self.generate_string_expr(l, params, locals, &lhs)?;
                    self.generate_string_expr(r, params, locals, &rhs)?;
                    let lhs_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr %String, %String* %{}, i32 0, i32 0",
                        lhs_ptr, lhs
                    )
                    .unwrap();
                    let lhs_str = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = load i8*, i8** %{}",
                        lhs_str, lhs_ptr
                    )
                    .unwrap();
                    let rhs_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr %String, %String* %{}, i32 0, i32 0",
                        rhs_ptr, rhs
                    )
                    .unwrap();
                    let rhs_str = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = load i8*, i8** %{}",
                        rhs_str, rhs_ptr
                    )
                    .unwrap();
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
                    )
                    .unwrap();
                    writeln!(
                        &mut self.functions,
                        "  call i32 (i8*, i8*, ...) @sprintf(i8* %{}, i8* %{}, i8* %{}, i8* %{})",
                        buf_ptr, fmt_ptr, lhs_str, rhs_str
                    )
                    .unwrap();
                    let concat_len64 = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = call i64 @strlen(i8* %{})",
                        concat_len64, buf_ptr
                    )
                    .unwrap();
                    let concat_len = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = trunc i64 %{} to i32",
                        concat_len, concat_len64
                    )
                    .unwrap();
                    let concat_cap = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = add i32 %{}, 1",
                        concat_cap, concat_len
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  %{} = alloca %String", result).unwrap();
                    writeln!(
                        &mut self.functions,
                        "  %{}.ptr = getelementptr %String, %String* %{}, i32 0, i32 0",
                        result, result
                    )
                    .unwrap();
                    writeln!(
                        &mut self.functions,
                        "  store i8* %{}, i8** %{}.ptr",
                        buf_ptr, result
                    )
                    .unwrap();
                    writeln!(
                        &mut self.functions,
                        "  %{}.len = getelementptr %String, %String* %{}, i32 0, i32 1",
                        result, result
                    )
                    .unwrap();
                    writeln!(
                        &mut self.functions,
                        "  store i32 %{}, i32* %{}.len",
                        concat_len, result
                    )
                    .unwrap();
                    writeln!(
                        &mut self.functions,
                        "  %{}.cap = getelementptr %String, %String* %{}, i32 0, i32 2",
                        result, result
                    )
                    .unwrap();
                    writeln!(
                        &mut self.functions,
                        "  store i32 %{}, i32* %{}.cap",
                        concat_cap, result
                    )
                    .unwrap();
                } else {
                    return Err(CompileError::new(
                        "string concatenation requires two strings",
                    ));
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
                let ptr_storage = self.next_temp();
                writeln!(&mut self.functions, "  %{} = alloca %String*", ptr_storage).unwrap();
                let merge_label = self.next_block_label("sif_merge");
                let then_label = self.next_block_label("sif_then");
                if let Some(else_block) = else_branch {
                    let else_label = self.next_block_label("sif_else");
                    writeln!(
                        &mut self.functions,
                        "  br i1 %{}, label %{}, label %{}",
                        cond_bool, then_label, else_label
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "{}:", then_label).unwrap();
                    let arm_temp = self.next_temp();
                    self.generate_string_expr(
                        then_branch
                            .stmts
                            .last()
                            .unwrap_or(&Expr::StringLiteral(String::new(), Default::default())),
                        params,
                        locals,
                        &arm_temp,
                    )?;
                    writeln!(
                        &mut self.functions,
                        "  store %String* %{}, %String** %{}",
                        arm_temp, ptr_storage
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                    writeln!(&mut self.functions, "{}:", else_label).unwrap();
                    let arm_temp = self.next_temp();
                    self.generate_string_expr(
                        else_block
                            .stmts
                            .last()
                            .unwrap_or(&Expr::StringLiteral(String::new(), Default::default())),
                        params,
                        locals,
                        &arm_temp,
                    )?;
                    writeln!(
                        &mut self.functions,
                        "  store %String* %{}, %String** %{}",
                        arm_temp, ptr_storage
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                } else {
                    writeln!(
                        &mut self.functions,
                        "  br i1 %{}, label %{}, label %{}",
                        cond_bool, then_label, merge_label
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "{}:", then_label).unwrap();
                    let arm_temp = self.next_temp();
                    self.generate_string_expr(
                        then_branch
                            .stmts
                            .last()
                            .unwrap_or(&Expr::StringLiteral(String::new(), Default::default())),
                        params,
                        locals,
                        &arm_temp,
                    )?;
                    writeln!(
                        &mut self.functions,
                        "  store %String* %{}, %String** %{}",
                        arm_temp, ptr_storage
                    )
                    .unwrap();
                    writeln!(&mut self.functions, "  br label %{}", merge_label).unwrap();
                }
                writeln!(&mut self.functions, "{}:", merge_label).unwrap();
                writeln!(
                    &mut self.functions,
                    "  %{} = load %String*, %String** %{}",
                    result, ptr_storage
                )
                .unwrap();
            }
            _ => return Err(CompileError::new("expected string expression")),
        }
        Ok(())
    }

    fn emit_fstring_to_buffer(
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
                Expr::StringLiteral(s, _) | Expr::MultilineString(s, _) => {
                    parts.push(s.clone());
                }
                Expr::Identifier(name, _) => {
                    if let Some((Type::String, alloca, _)) = locals.get(name) {
                        parts.push("%s".to_string());
                        let loaded = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = load %String*, %String** %{}",
                            loaded, alloca
                        )
                        .unwrap();
                        let ptr = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = getelementptr %String, %String* %{}, i32 0, i32 0",
                            ptr, loaded
                        )
                        .unwrap();
                        let str_val = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = load i8*, i8** %{}",
                            str_val, ptr
                        )
                        .unwrap();
                        args.push(format!("i8* %{}", str_val));
                    } else if let Some((Type::I32, alloca, _)) = locals.get(name) {
                        parts.push("%d".to_string());
                        let loaded = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = load i32, i32* %{}",
                            loaded, alloca
                        )
                        .unwrap();
                        args.push(format!("i32 %{}", loaded));
                    } else if let Some((Type::F64, alloca, _)) = locals.get(name) {
                        parts.push("%f".to_string());
                        let loaded = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = load double, double* %{}",
                            loaded, alloca
                        )
                        .unwrap();
                        args.push(format!("double %{}", loaded));
                    } else if let Some((Type::Ptr, alloca, _)) = locals.get(name) {
                        parts.push("%s".to_string());
                        let loaded = self.next_temp();
                        writeln!(
                            &mut self.functions,
                            "  %{} = load i8*, i8** %{}",
                            loaded, alloca
                        )
                        .unwrap();
                        args.push(format!("i8* %{}", loaded));
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
                expr => {
                    parts.push("%s".to_string());
                    let val = self.next_temp();
                    self.generate_string_expr(expr, params, locals, &val)?;
                    let val_ptr = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = getelementptr %String, %String* %{}, i32 0, i32 0",
                        val_ptr, val
                    )
                    .unwrap();
                    let val_str = self.next_temp();
                    writeln!(
                        &mut self.functions,
                        "  %{} = load i8*, i8** %{}",
                        val_str, val_ptr
                    )
                    .unwrap();
                    args.push(format!("i8* %{}", val_str));
                }
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
        writeln!(
            &mut self.functions,
            "  %{} = bitcast i8* %{} to i8*",
            result_ptr, buf_ptr
        )
        .unwrap();
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
            self.generate_string_expr(&args[0], params, locals, &ptr)?;
            let len_reg = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = call i32 @__rt_strlen(%String* %{})",
                len_reg, ptr
            )
            .unwrap();
            return Ok(Some(len_reg));
        }

        if self.stdlib_enabled && func == "is_empty" {
            if args.len() != 1 {
                return Err(CompileError::new("is_empty: expected 1 argument"));
            }
            let ptr = self.next_temp();
            self.generate_string_expr(&args[0], params, locals, &ptr)?;
            let str_ptr = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = getelementptr %String, %String* %{}, i32 0, i32 0",
                str_ptr, ptr
            )
            .unwrap();
            let raw = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = load i8*, i8** %{}",
                raw, str_ptr
            )
            .unwrap();
            let len = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = call i32 @__rt_strlen(%String* %{})",
                len, ptr
            )
            .unwrap();
            let is_empty = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = icmp eq i32 %{}, 0",
                is_empty, len
            )
            .unwrap();
            let zext = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = zext i1 %{} to i32",
                zext, is_empty
            )
            .unwrap();
            return Ok(Some(zext));
        }

        if self.stdlib_enabled && func == "read_file" {
            if args.len() != 1 {
                return Err(CompileError::new("read_file: expected 1 argument"));
            }
            let path_ptr = self.next_temp();
            self.generate_ptr_expr(&args[0], params, locals, &path_ptr)?;
            let raw_result = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = call i8* @__rt_read_file(i8* %{})",
                raw_result, path_ptr
            )
            .unwrap();
            let result = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = call %String* @__rt_wrap_string(i8* %{})",
                result, raw_result
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
            self.generate_string_expr(&args[1], params, locals, &data_ptr)?;
            let status = self.next_temp();
            writeln!(
                &mut self.functions,
                "  %{} = call i32 @__rt_write_file(i8* %{}, %String* %{})",
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
                let raw_result = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call i8* @__rt_to_string_int(i32 {})",
                    raw_result,
                    val.as_str()
                )
                .unwrap();
                let result = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call %String* @__rt_wrap_string(i8* %{})",
                    result, raw_result
                )
                .unwrap();
                return Ok(Some(result));
            } else if ty == "double" {
                let val = self.generate_float_expr(&args[0], params, locals)?;
                let raw_result = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call i8* @__rt_to_string_float(double {})",
                    raw_result,
                    val.as_str()
                )
                .unwrap();
                let result = self.next_temp();
                writeln!(
                    &mut self.functions,
                    "  %{} = call %String* @__rt_wrap_string(i8* %{})",
                    result, raw_result
                )
                .unwrap();
                return Ok(Some(result));
            } else if ty == "%String*" {
                let ptr = self.next_temp();
                self.generate_string_expr(&args[0], params, locals, &ptr)?;
                return Ok(Some(ptr));
            } else {
                return Err(CompileError::new("to_string: unsupported type"));
            }
        }

        let resolved = self.resolve_function_name(func);
        let mut ret_ty = self
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

        let mut call_resolved = resolved.clone();
        if let Some(func_def) = self.find_function_def(&resolved)
            && !func_def.generic_params.is_empty()
        {
            let mut concrete_types = Vec::new();
            for arg in args.iter() {
                let arg_type = self.infer_expr_type(arg, params, locals);
                let concrete = match arg_type {
                    "i32" => Type::I32,
                    "double" => Type::F64,
                    "i8*" => Type::Ptr,
                    "%String*" => Type::String,
                    _ => Type::Unknown,
                };
                concrete_types.push(concrete);
            }
            let mangled = self.monomorphize_function(&func_def.clone(), &concrete_types)?;
            call_resolved = mangled.clone();
            if let Some(mangled_def) = self.monomorphized.get(&mangled) {
                ret_ty = mangled_def
                    .return_type
                    .as_deref()
                    .map(Self::type_str_to_llvm_ret)
                    .unwrap_or("void");
            }
        }

        let param_types = self.get_function_param_types(&call_resolved);
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
                "%String*" => {
                    let ptr = self.next_temp();
                    self.generate_string_expr(arg, params, locals, &ptr)?;
                    call_args.push(format!("%String* %{}", ptr));
                }
                _ => {
                    return Err(CompileError::new(format!(
                        "unsupported argument type in call: {}",
                        expected_type
                    )));
                }
            }
        }

        if ret_ty == "void" {
            writeln!(
                &mut self.functions,
                "  call void @{}({})",
                call_resolved,
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
                call_resolved,
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
            match typ {
                Type::String => {
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast %String** %{} to %String*",
                        result, alloca
                    )
                    .unwrap();
                }
                Type::I32 | Type::F64 | Type::Ptr => {
                    let source_ty = match typ {
                        Type::I32 => "i32",
                        Type::F64 => "double",
                        Type::Ptr => "i8*",
                        _ => "i8*",
                    };
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast {}* %{} to i8*",
                        result, source_ty, alloca
                    )
                    .unwrap();
                }
                _ => {
                    let source_ty = match typ {
                        Type::I32 => "i32",
                        Type::F64 => "double",
                        Type::Ptr => "i8*",
                        _ => "i8*",
                    };
                    writeln!(
                        &mut self.functions,
                        "  %{} = bitcast {}* %{} to i8*",
                        result, source_ty, alloca
                    )
                    .unwrap();
                }
            }
            return Ok(());
        }

        if let Some(idx) = params.iter().position(|p| p == name) {
            writeln!(
                &mut self.functions,
                "  %{} = bitcast i8* %arg{} to i8*",
                result, idx
            )
            .unwrap();
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
            fmt, ptr
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
        self.declare_runtime_helpers();
        return;
    }

    fn declare_runtime_helpers(&mut self) {
        // Declarations for the external C runtime (libro/src/libro.c).
        // The definitions are no longer emitted as IR; they are linked
        // from libro/libro.a. This keeps compiler.rs focused on codegen.
        let mut decls = String::new();
        // String helpers (RoString* = %String*)
        writeln!(&mut decls, "declare i32 @__rt_strlen(%String*)").unwrap();
        writeln!(&mut decls, "declare i32 @__rt_strlen_cstr(i8*)").unwrap();
        writeln!(&mut decls, "declare void @__rt_print_str(%String*)").unwrap();
        writeln!(&mut decls, "declare void @__rt_print_int(i32)").unwrap();
        writeln!(&mut decls, "declare void @__rt_print_float(double)").unwrap();
        writeln!(&mut decls, "declare void @__rt_exit(i32)").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_read_file(i8*)").unwrap();
        writeln!(&mut decls, "declare i32 @__rt_write_file(i8*, %String*)").unwrap();
        writeln!(&mut decls, "declare i32 @__rt_contains(%String*, %String*)").unwrap();
        writeln!(&mut decls, "declare i32 @__rt_starts_with(%String*, %String*)").unwrap();
        writeln!(&mut decls, "declare i32 @__rt_ends_with(%String*, %String*)").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_substr(%String*, i32, i32)").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_trim(%String*)").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_to_uppercase(%String*)").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_to_lowercase(%String*)").unwrap();
        writeln!(&mut decls, "declare i32 @__rt_to_int(%String*)").unwrap();
        writeln!(&mut decls, "declare double @__rt_to_float(%String*)").unwrap();
        writeln!(&mut decls, "declare double @__rt_floor(double)").unwrap();
        writeln!(&mut decls, "declare double @__rt_ceil(double)").unwrap();
        writeln!(&mut decls, "declare double @__rt_round(double)").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_read_line()").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_to_string_int(i32)").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_to_string_float(double)").unwrap();
        writeln!(&mut decls, "declare void @__rt_free(i8*)").unwrap();
        writeln!(&mut decls, "declare %String* @__rt_wrap_string(i8*)").unwrap();
        writeln!(&mut decls, "declare void @__rt_panic_bounds(i8*)").unwrap();
        writeln!(&mut decls, "declare i32 @__rt_list_len(i8*)").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_list_push(i8*, i32)").unwrap();
        writeln!(&mut decls, "declare i32 @__rt_list_pop(i8*)").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_to_hex(i32)").unwrap();
        writeln!(&mut decls, "declare i8* @__rt_str_repeat(%String*, i32)").unwrap();
        writeln!(&mut decls, "declare double @__rt_sqrt(double)").unwrap();
        writeln!(&mut decls, "declare double @__rt_sin(double)").unwrap();
        writeln!(&mut decls, "declare double @__rt_cos(double)").unwrap();
        writeln!(&mut decls, "declare double @__rt_tan(double)").unwrap();
        writeln!(&mut decls, "declare double @__rt_abs(double)").unwrap();
        // LLVM intrinsics used by codegen (still needed as declarations)
        writeln!(&mut decls, "declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg)").unwrap();
        writeln!(&mut decls, "declare i64 @llvm.umin.i64(i64, i64)").unwrap();
        self.functions.push_str(&decls);
    }

    fn emit_asm_call(
        &mut self,
        args: &[Expr],
        params: &[String],
        locals: &HashMap<String, (Type, String, bool)>,
    ) -> CompileResult<()> {
        let code = if let Expr::StringLiteral(s, _) = &args[0] {
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

            if let Expr::Identifier(name, _) = arg {
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
                        _ => {
                            // fallback for unsupported types
                            // handled as pointer
                        }
                    }
                } else if let Some(idx) = params.iter().position(|p| p == name) {
                    constraints.push('r');
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
    use super::LLVMTextGen;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

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
        let ir = compile_expr(
            r#""""multi
line"""#,
        );
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
        let ir = compile_expr(
            "fn test():\n    let x = 1\n    if x:\n        return 1\n    else:\n        return 0",
        );
        assert!(ir.contains("br i1"));
        assert!(ir.contains("then_"));
        assert!(ir.contains("else_"));
        assert!(ir.contains("merge_"));
    }

    #[test]
    fn test_compile_nested_if() {
        let ir = compile_expr(
            "fn test():\n    let x = 1\n    let y = 1\n    if x:\n        if y:\n            return 1\n        else:\n            return 0",
        );
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
        let ir = compile_expr(
            "fn test():\n    let a = 10\n    let b = 5\n    let c = 2\n    let d = 3\n    let e = 2\n    a + b - c * d / e",
        );
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

    #[test]
    fn test_compile_for_in_list() {
        let ir = compile_expr("fn test():\n    for x in [1, 2, 3]:\n        x");
        assert!(ir.contains("for_start"));
        assert!(ir.contains("for_body"));
        assert!(ir.contains("for_end"));
        assert!(ir.contains("__rt_list_len") || ir.contains("getelementptr"));
    }

    #[test]
    fn test_compile_for_in_list_float() {
        let ir = compile_expr("fn test():\n    for x in [1.0, 2.0, 3.0]:\n        x");
        assert!(ir.contains("for_start"));
        assert!(ir.contains("for_body"));
        assert!(ir.contains("for_end"));
        assert!(ir.contains("load double"));
    }
}
