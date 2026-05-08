use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    F64,
    Bool,
    String,
    Void,
    Unknown,
}

impl Type {
    pub fn from_str(s: &str) -> Self {
        match s {
            "i32" => Type::I32,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            "str" => Type::String,
            _ => Type::Unknown,
        }
    }

    pub fn to_llvm(&self) -> &'static str {
        match self {
            Type::I32 => "i32",
            Type::F64 => "double",
            Type::Bool => "i32",
            Type::String => "i8*",
            Type::Void => "void",
            Type::Unknown => "i8*",
        }
    }

    pub fn can_arith(&self) -> bool {
        matches!(self, Type::I32 | Type::F64)
    }

    pub fn can_compare(&self) -> bool {
        matches!(self, Type::I32 | Type::F64 | Type::Bool)
    }
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub line: Option<usize>,
}

impl TypeError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            line: None,
        }
    }

    pub fn with_line(mut self, line: usize) -> Self {
        self.line = Some(line);
        self
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(line) = self.line {
            write!(f, "Type error at line {}: {}", line, self.message)
        } else {
            write!(f, "Type error: {}", self.message)
        }
    }
}

impl std::error::Error for TypeError {}

type TypeResult<T> = Result<T, TypeError>;

#[derive(Debug, Clone)]
struct TypeEnv {
    locals: HashMap<String, Type>,
    params: HashMap<String, Type>,
    globals: HashMap<String, Type>,
    functions: HashMap<String, Type>,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            params: HashMap::new(),
            globals: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Type {
        self.locals
            .get(name)
            .or_else(|| self.params.get(name))
            .or_else(|| self.globals.get(name))
            .cloned()
            .unwrap_or(Type::Unknown)
    }

    fn insert_local(&mut self, name: String, typ: Type) {
        self.locals.insert(name, typ);
    }

    fn insert_param(&mut self, name: String, typ: Type) {
        self.params.insert(name, typ);
    }

    fn insert_global(&mut self, name: String, typ: Type) {
        self.globals.insert(name, typ);
    }

    fn insert_function(&mut self, name: String, typ: Type) {
        self.functions.insert(name, typ);
    }
}

pub struct TypeChecker {
    env: TypeEnv,
    stdlib_enabled: bool,
    function_aliases: HashMap<String, String>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
            stdlib_enabled: false,
            function_aliases: HashMap::new(),
        }
    }

    pub fn set_function_aliases(&mut self, aliases: HashMap<String, String>) {
        self.function_aliases = aliases;
    }

    pub fn check_program(&mut self, program: &Program, stdlib_enabled: bool) -> TypeResult<()> {
        self.stdlib_enabled = stdlib_enabled;

        self.register_globals(&program.globals)?;
        self.register_functions(&program.functions)?;

        self.resolve_function_types(&program)?;

        for func in &program.functions {
            self.check_function(func)?;
        }

        Ok(())
    }

    fn register_globals(&mut self, globals: &[Expr]) -> TypeResult<()> {
        for global in globals {
            match global {
                Expr::Import(_) => {}
                Expr::Let { name, typ, value, is_const: _ } => {
                    let value_type = self.infer_expr(value)?;
                    let annotated = typ.as_ref().map(|t| Type::from_str(t));
                    let final_type = annotated.unwrap_or(value_type);
                    self.env.insert_global(name.clone(), final_type);
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn register_functions(&mut self, functions: &[FunctionDef]) -> TypeResult<()> {
        for func in functions {
            self.env.insert_function(func.name.clone(), Type::Void);
        }
        Ok(())
    }

    fn resolve_function_types(&mut self, program: &Program) -> TypeResult<()> {
        for _ in 0..8 {
            let mut changed = false;
            for func in &program.functions {
                let prev = self.env.functions.get(&func.name).cloned().unwrap_or(Type::Void);
                let inferred = self.infer_function_return_type(func)?;

                if prev != inferred && prev != Type::Void {
                    return Err(TypeError::new(format!(
                        "function {} has conflicting return types: {} and {}",
                        func.name,
                        self.type_name(&prev),
                        self.type_name(&inferred)
                    )));
                }

                if prev != inferred {
                    self.env.functions.insert(func.name.clone(), inferred.clone());
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        Ok(())
    }

    fn infer_function_return_type(&self, func: &FunctionDef) -> TypeResult<Type> {
        if func.name == "main" {
            return Ok(Type::I32);
        }

        for expr in &func.body {
            if let Expr::Return(inner) = expr {
                return Ok(self.infer_expr_without_env(inner));
            }
            if let Expr::If { then_branch, else_branch, .. } = expr {
                let then_type = self.find_return_in_block(then_branch);
                if then_type != Type::Void {
                    return Ok(then_type);
                }
                if let Some(else_block) = else_branch {
                    let else_type = self.find_return_in_block(else_block);
                    if else_type != Type::Void {
                        return Ok(else_type);
                    }
                }
            }
        }
        Ok(Type::Void)
    }

    fn find_return_in_block(&self, block: &Block) -> Type {
        for expr in &block.stmts {
            if let Expr::Return(inner) = expr {
                return self.infer_expr_without_env(inner);
            }
            if let Expr::If { then_branch, else_branch, .. } = expr {
                let then_type = self.find_return_in_block(then_branch);
                if then_type != Type::Void {
                    return then_type;
                }
                if let Some(else_block) = else_branch {
                    let else_type = self.find_return_in_block(else_block);
                    if else_type != Type::Void {
                        return else_type;
                    }
                }
            }
        }
        Type::Void
    }

    fn check_function(&mut self, func: &FunctionDef) -> TypeResult<()> {
        let param_types = vec![Type::String; func.params.len()];
        for (i, param) in func.params.iter().enumerate() {
            self.env.insert_param(param.clone(), param_types.get(i).cloned().unwrap_or(Type::String));
        }

        self.env.locals.clear();

        for expr in &func.body {
            self.check_expr(expr)?;
        }

        for param in &func.params {
            self.env.params.remove(param);
        }
        self.env.locals.clear();

        Ok(())
    }

    fn infer_expr(&mut self, expr: &Expr) -> TypeResult<Type> {
        match expr {
            Expr::Import(_) => Ok(Type::Void),
            Expr::Literal(Literal::Int(_)) => Ok(Type::I32),
            Expr::Literal(Literal::Float(_)) => Ok(Type::F64),
            Expr::Literal(Literal::Bool(_)) => Ok(Type::Bool),
            Expr::Literal(Literal::Str(_)) => Ok(Type::String),
            Expr::StringLiteral(_) | Expr::MultilineString(_) | Expr::FString(_) => Ok(Type::String),
            Expr::Borrow { .. } => Ok(Type::String),
            Expr::Identifier(name) => Ok(self.env.get(name)),
            Expr::Call { func, args } => self.check_call(func, args),
            Expr::Binary(l, op, r) => self.check_binary_op(l, op, r),
            Expr::Unary(op, inner) => self.check_unary_op(op, inner),
            Expr::If { condition, then_branch, else_branch } => {
                let cond_type = self.infer_expr(condition)?;
                if cond_type != Type::Bool && cond_type != Type::I32 {
                    return Err(TypeError::new("if condition must be boolean"));
                }

                let then_type = self.infer_block_type(then_branch);
                if let Some(else_block) = else_branch {
                    let else_type = self.infer_block_type(else_block);
                    if then_type != Type::Void && else_type != Type::Void && then_type != else_type {
                        return Err(TypeError::new(format!(
                            "if branches have mismatched types: {} and {}",
                            self.type_name(&then_type),
                            self.type_name(&else_type)
                        )));
                    }
                    if then_type != Type::Void {
                        return Ok(then_type);
                    }
                    return Ok(else_type);
                }
                Ok(Type::Void)
            }
            Expr::Let { name, typ, value, is_const: _ } => {
                let value_type = self.infer_expr(value)?;
                let annotated = typ.as_ref().map(|t| Type::from_str(t));

                if let Some(expected) = &annotated {
                    if !self.types_compatible(&value_type, expected) {
                        return Err(TypeError::new(format!(
                            "cannot assign {} to variable of type {}",
                            self.type_name(&value_type),
                            self.type_name(expected)
                        )));
                    }
                }

                let final_type = annotated.unwrap_or(value_type);
                self.env.insert_local(name.clone(), final_type);
                Ok(Type::Void)
            }
            Expr::Assign { name, value } => {
                let value_type = self.infer_expr(value)?;
                let var_type = self.env.get(name);

                if var_type == Type::Unknown {
                    return Err(TypeError::new(format!("unknown variable: {}", name)));
                }

                if !self.types_compatible(&value_type, &var_type) {
                    return Err(TypeError::new(format!(
                        "cannot assign {} to variable of type {}",
                        self.type_name(&value_type),
                        self.type_name(&var_type)
                    )));
                }

                Ok(Type::Void)
            }
            Expr::Return(inner) => self.infer_expr(inner),
        }
    }

    fn infer_expr_without_env(&self, expr: &Expr) -> Type {
        match expr {
            Expr::Literal(Literal::Int(_)) => Type::I32,
            Expr::Literal(Literal::Float(_)) => Type::F64,
            Expr::Literal(Literal::Bool(_)) => Type::Bool,
            Expr::Literal(Literal::Str(_)) => Type::String,
            Expr::StringLiteral(_) | Expr::MultilineString(_) | Expr::FString(_) => Type::String,
            Expr::Call { func, args: _ } => {
                if let Some(ret) = self.env.functions.get(func) {
                    ret.clone()
                } else if let Some(ret) = self.builtin_return_type(func) {
                    ret
                } else {
                    Type::Void
                }
            }
            _ => Type::Void,
        }
    }

    fn infer_block_type(&self, block: &Block) -> Type {
        for expr in &block.stmts {
            if let Expr::Return(inner) = expr {
                return self.infer_expr_without_env(inner);
            }
        }
        Type::Void
    }

    fn resolve_function_name(&self, func: &str) -> String {
        self.function_aliases
            .get(func)
            .cloned()
            .unwrap_or_else(|| func.to_string())
    }

    fn check_call(&mut self, func: &str, _args: &[Expr]) -> TypeResult<Type> {
        if let Some(ret) = self.builtin_return_type(func) {
            return Ok(ret);
        }

        let resolved = self.resolve_function_name(func);

        if let Some(ret) = self.env.functions.get(&resolved) {
            return Ok(ret.clone());
        }

        let err_msg = if !func.contains("__") && !func.starts_with("__rt_") {
            format!("unknown function: {} (try: import std)", func)
        } else {
            format!("unknown function: {}", func)
        };

        Err(TypeError::new(err_msg))
    }

    fn builtin_return_type(&self, func: &str) -> Option<Type> {
        match func {
            "__rt_strlen" => return Some(Type::I32),
            "__rt_read_file" => return Some(Type::String),
            "__rt_write_file" => return Some(Type::I32),
            "__rt_exit" | "__rt_print_str" | "__rt_print_int" | "__rt_print_float" => {
                return Some(Type::Void)
            }
            _ => {}
        }

        if !self.stdlib_enabled {
            return None;
        }

        match func {
            "print" | "asm" | "exit" | "write_file" => Some(Type::Void),
            "len" => Some(Type::I32),
            "read_file" => Some(Type::String),
            _ => None,
        }
    }

    fn check_binary_op(&mut self, l: &Expr, op: &BinOp, r: &Expr) -> TypeResult<Type> {
        let is_comparison = matches!(
            op,
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
        );
        let is_logical = matches!(op, BinOp::And | BinOp::Or);

        if is_comparison || is_logical {
            let l_type = self.infer_expr(l)?;
            let r_type = self.infer_expr(r)?;

            if !l_type.can_compare() || !r_type.can_compare() {
                return Err(TypeError::new(format!(
                    "cannot compare values of type {} and {}",
                    self.type_name(&l_type),
                    self.type_name(&r_type)
                )));
            }

            return Ok(Type::I32);
        }

        let l_type = self.infer_expr(l)?;
        let r_type = self.infer_expr(r)?;

        if !l_type.can_arith() || !r_type.can_arith() {
            return Err(TypeError::new(format!(
                "cannot perform arithmetic on types {} and {}",
                self.type_name(&l_type),
                self.type_name(&r_type)
            )));
        }

        if matches!(l_type, Type::F64) || matches!(r_type, Type::F64) {
            Ok(Type::F64)
        } else {
            Ok(Type::I32)
        }
    }

    fn check_unary_op(&mut self, op: &UnaryOp, inner: &Expr) -> TypeResult<Type> {
        let inner_type = self.infer_expr(inner)?;

        match op {
            UnaryOp::Not => {
                if !inner_type.can_compare() {
                    return Err(TypeError::new(format!(
                        "cannot apply not to type {}",
                        self.type_name(&inner_type)
                    )));
                }
                Ok(Type::I32)
            }
            UnaryOp::Neg => {
                if !inner_type.can_arith() {
                    return Err(TypeError::new(format!(
                        "cannot negate type {}",
                        self.type_name(&inner_type)
                    )));
                }
                Ok(inner_type)
            }
            UnaryOp::Pos => {
                if !inner_type.can_arith() {
                    return Err(TypeError::new(format!(
                        "cannot apply unary + to type {}",
                        self.type_name(&inner_type)
                    )));
                }
                Ok(inner_type)
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> TypeResult<Type> {
        match expr {
            Expr::Import(_) => Ok(Type::Void),
            Expr::Literal(_) => Ok(Type::Void),
            Expr::StringLiteral(_) | Expr::MultilineString(_) | Expr::FString(_) => Ok(Type::Void),
            Expr::Identifier(_) => Ok(Type::Void),
            Expr::Borrow { .. } => Ok(Type::Void),
            Expr::Call { func, args } => {
                self.check_call(func, args)?;
                Ok(Type::Void)
            }
            Expr::Binary(l, op, r) => {
                self.check_binary_op(l, op, r)?;
                Ok(Type::Void)
            }
            Expr::Unary(op, inner) => {
                self.check_unary_op(op, inner)?;
                Ok(Type::Void)
            }
            Expr::If { condition, then_branch, else_branch } => {
                let cond_type = self.infer_expr(condition)?;
                if cond_type != Type::Bool && cond_type != Type::I32 {
                    return Err(TypeError::new("if condition must be boolean"));
                }

                let then_env = self.env.clone();
                let mut then_checker = TypeChecker {
                    env: then_env,
                    stdlib_enabled: self.stdlib_enabled,
                    function_aliases: self.function_aliases.clone(),
                };
                for expr in &then_branch.stmts {
                    then_checker.check_expr(expr)?;
                }

                if let Some(else_block) = else_branch {
                    let else_env = self.env.clone();
                    let mut else_checker = TypeChecker {
                        env: else_env,
                        stdlib_enabled: self.stdlib_enabled,
                        function_aliases: self.function_aliases.clone(),
                    };
                    for expr in &else_block.stmts {
                        else_checker.check_expr(expr)?;
                    }
                }

                Ok(Type::Void)
            }
            Expr::Let { name, typ, value, is_const: _ } => {
                let value_type = self.infer_expr(value)?;
                let annotated = typ.as_ref().map(|t| Type::from_str(t));

                if let Some(expected) = &annotated {
                    if !self.types_compatible(&value_type, expected) {
                        return Err(TypeError::new(format!(
                            "cannot assign {} to variable of type {}",
                            self.type_name(&value_type),
                            self.type_name(expected)
                        )));
                    }
                }

                let final_type = annotated.unwrap_or(value_type);
                self.env.insert_local(name.clone(), final_type);
                Ok(Type::Void)
            }
            Expr::Assign { name, value } => {
                let value_type = self.infer_expr(value)?;
                let var_type = self.env.get(name);

                if var_type == Type::Unknown {
                    return Err(TypeError::new(format!("unknown variable: {}", name)));
                }

                if !self.types_compatible(&value_type, &var_type) {
                    return Err(TypeError::new(format!(
                        "cannot assign {} to variable of type {}",
                        self.type_name(&value_type),
                        self.type_name(&var_type)
                    )));
                }

                Ok(Type::Void)
            }
            Expr::Return(inner) => {
                self.infer_expr(inner)?;
                Ok(Type::Void)
            }
        }
    }

    fn types_compatible(&self, actual: &Type, expected: &Type) -> bool {
        if actual == expected {
            return true;
        }

        match (actual, expected) {
            (Type::I32, Type::F64) => true,
            (Type::F64, Type::I32) => true,
            (Type::Bool, Type::I32) => true,
            (Type::I32, Type::Bool) => true,
            _ => false,
        }
    }

    fn type_name(&self, typ: &Type) -> String {
        match typ {
            Type::I32 => "i32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "str".to_string(),
            Type::Void => "void".to_string(),
            Type::Unknown => "unknown".to_string(),
        }
    }
}