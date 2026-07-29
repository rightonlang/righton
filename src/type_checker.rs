use crate::ast::*;
use crate::diagnostics::Diagnostic;
use std::collections::HashMap;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    F64,
    Bool,
    String,
    Struct(String, Vec<Type>),
    Enum(String),
    Tuple(Vec<Type>),
    Void,
    Unknown,
    Generic(String),
}

impl std::str::FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(open) = s.find('[')
            && s.ends_with(']')
        {
            let base = &s[..open];
            let args_str = &s[open + 1..s.len() - 1];
            let args: Vec<Type> = args_str
                .split(',')
                .map(|a| Type::from_str(a.trim()).unwrap())
                .collect();
            return Ok(Type::Struct(base.to_string(), args));
        }
        match s {
            "i32" => Ok(Type::I32),
            "f64" | "float" | "double" => Ok(Type::F64),
            "bool" => Ok(Type::Bool),
            "str" | "string" | "ptr" => Ok(Type::String),
            _ => Ok(Type::Unknown),
        }
    }
}

impl Type {
    pub fn to_llvm(&self) -> &'static str {
        match self {
            Type::I32 => "i32",
            Type::F64 => "double",
            Type::Bool => "i32",
            Type::String => "i8*",
            Type::Struct(_, _) => "i8*",
            Type::Enum(_) => "i8*",
            Type::Tuple(_) => "i8*",
            Type::Void => "void",
            Type::Unknown => "i8*",
            Type::Generic(_) => "i8*",
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

    pub fn to_diagnostic(&self) -> Diagnostic {
        let span = self.line.map(|l| SourceSpan::new(l, 0));
        let diag = Diagnostic::error(Some("E0002"), self.message.clone())
            .with_span(span.unwrap_or(SourceSpan::unknown()));
        if let Some(hint) = Self::hint_for_message(&self.message) {
            diag.with_hint(hint)
        } else {
            diag
        }
    }

    fn hint_for_message(msg: &str) -> Option<String> {
        if msg.contains("unknown variable") {
            return Some("ensure the variable is declared before use".into());
        }
        if msg.contains("if condition must be boolean") {
            return Some("use a comparison like 'x == 1' or a boolean variable".into());
        }
        if msg.contains("while condition must be boolean") {
            return Some("use a comparison like 'x > 0' or a boolean variable".into());
        }
        if msg.contains("list index must be an integer") {
            return Some("use an i32 value as the index".into());
        }
        if msg.contains("cannot index into this type") {
            return Some("only lists and strings support indexing".into());
        }
        if msg.contains("cannot assign") {
            return Some("check that the types match, or remove the type annotation".into());
        }
        if msg.contains("all list elements must have the same type") {
            return Some("mix types by converting explicitly, e.g. to_string()".into());
        }
        if msg.contains("all match arms must return the same type") {
            return Some("ensure every arm returns the same type, or use a common wrapper".into());
        }
        if msg.contains("unknown field") {
            return Some("check the struct definition for available fields".into());
        }
        if msg.contains("unknown variant") {
            return Some("check the enum definition for available variants".into());
        }
        if msg.contains("if branches have mismatched types") {
            return Some("both branches must return the same type".into());
        }
        None
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
    structs: Vec<crate::ast::StructDef>,
    enums: Vec<crate::ast::EnumDef>,
    type_aliases: HashMap<String, String>,
    generics: HashMap<String, Type>,
    generic_functions: HashMap<String, (Vec<String>, Option<String>)>,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            params: HashMap::new(),
            globals: HashMap::new(),
            functions: HashMap::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            type_aliases: HashMap::new(),
            generics: HashMap::new(),
            generic_functions: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Type {
        self.locals
            .get(name)
            .or_else(|| self.params.get(name))
            .or_else(|| self.generics.get(name))
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

    fn insert_generic(&mut self, name: String, typ: Type) {
        self.generics.insert(name, typ);
    }

    fn insert_global(&mut self, name: String, typ: Type) {
        self.globals.insert(name, typ);
    }

    fn insert_function(&mut self, name: String, typ: Type) {
        self.functions.insert(name, typ);
    }

    fn insert_generic_function(
        &mut self,
        name: String,
        generic_params: Vec<String>,
        return_type: Option<String>,
    ) {
        self.generic_functions
            .insert(name, (generic_params, return_type));
    }

    fn resolve_type(&self, s: &str, self_struct: Option<&str>) -> Type {
        self.resolve_type_with_generics(s, &self.generics, self_struct)
    }

    fn resolve_type_with_generics(
        &self,
        s: &str,
        generics: &HashMap<String, Type>,
        self_struct: Option<&str>,
    ) -> Type {
        if s == "Self"
            && let Some(struct_name) = self_struct
        {
            return Type::Struct(struct_name.to_string(), Vec::new());
        }
        if let Some(typ) = generics.get(s) {
            return typ.clone();
        }
        let mut current = s.to_string();
        let mut seen = std::collections::HashSet::new();
        for _ in 0..16 {
            if let Some(r) = self.type_aliases.get(&current) {
                if !seen.insert(current.clone()) {
                    break;
                }
                current = r.clone();
            } else {
                break;
            }
        }
        if let Some(open) = current.find('[')
            && current.ends_with(']')
        {
            let base = &current[..open];
            let args_str = &current[open + 1..current.len() - 1];
            let args: Vec<Type> = args_str
                .split(',')
                .map(|a| self.resolve_type_with_generics(a.trim(), generics, self_struct))
                .collect();
            return Type::Struct(base.to_string(), args);
        }
        match Type::from_str(&current) {
            Ok(Type::Unknown) => Type::Generic(current),
            Ok(typ) => typ,
            Err(_) => Type::Generic(current),
        }
    }
}

pub struct TypeChecker {
    env: TypeEnv,
    stdlib_enabled: bool,
    function_aliases: HashMap<String, String>,
    self_struct_type: Option<String>,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
            stdlib_enabled: false,
            function_aliases: HashMap::new(),
            self_struct_type: None,
        }
    }

    pub fn set_function_aliases(&mut self, aliases: HashMap<String, String>) {
        self.function_aliases = aliases;
    }

    pub fn check_program(&mut self, program: &Program, stdlib_enabled: bool) -> TypeResult<()> {
        self.stdlib_enabled = stdlib_enabled;
        self.env.structs = program.structs.clone();
        self.env.enums = program.enums.clone();
        for alias in &program.type_aliases {
            self.env
                .type_aliases
                .insert(alias.name.clone(), alias.value.clone());
        }

        self.register_globals(&program.globals)?;
        self.register_functions(&program.functions)?;

        // Register impl methods as functions
        for impl_def in &program.impls {
            for method in &impl_def.methods {
                self.env.insert_function(method.name.clone(), Type::Void);
            }
        }

        self.resolve_function_types(program)?;

        // Check impl methods
        for func in &program.functions {
            self.check_function(func)?;
        }
        for impl_def in &program.impls {
            for method in &impl_def.methods {
                self.self_struct_type = Some(impl_def.struct_name.clone());
                self.check_function(method)?;
            }
        }
        self.self_struct_type = None;

        Ok(())
    }

    fn register_globals(&mut self, globals: &[Expr]) -> TypeResult<()> {
        for global in globals {
            match global {
                Expr::Import(_, _) => {}
                Expr::Let {
                    name,
                    typ,
                    value,
                    is_const: _,
                } => {
                    let value_type = self.infer_expr(value)?;
                    let annotated = typ
                        .as_ref()
                        .map(|t| self.env.resolve_type(t, self.self_struct_type.as_deref()));

                    if let Some(expected) = &annotated
                        && !self.types_compatible(&value_type, expected)
                    {
                        return Err(TypeError::new(format!(
                            "cannot assign {} to variable of type {}",
                            self.type_name(&value_type),
                            self.type_name(expected)
                        )));
                    }

                    let final_type = annotated.unwrap_or(value_type);
                    self.env.insert_global(name.clone(), final_type);
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
                }
                _ => {}
            };
        }
        Ok(())
    }

    fn register_functions(&mut self, functions: &[FunctionDef]) -> TypeResult<()> {
        for func in functions {
            self.env.insert_function(func.name.clone(), Type::Void);
            if !func.generic_params.is_empty() {
                self.env.insert_generic_function(
                    func.name.clone(),
                    func.generic_params.clone(),
                    func.return_type.clone(),
                );
            }
        }
        Ok(())
    }

    fn resolve_function_types(&mut self, program: &Program) -> TypeResult<()> {
        for _ in 0..8 {
            let mut changed = false;
            for func in program
                .functions
                .iter()
                .chain(program.impls.iter().flat_map(|i| i.methods.iter()))
            {
                let prev = self
                    .env
                    .functions
                    .get(&func.name)
                    .cloned()
                    .unwrap_or(Type::Void);
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
                    self.env
                        .functions
                        .insert(func.name.clone(), inferred.clone());
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
            if let Expr::Return(inner, _) = expr {
                return Ok(self.infer_expr_without_env(inner));
            }
            if let Expr::If {
                then_branch,
                else_branch,
                ..
            } = expr
            {
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
            if let Expr::Return(inner, _) = expr {
                return self.infer_expr_without_env(inner);
            }
            if let Expr::If {
                then_branch,
                else_branch,
                ..
            } = expr
            {
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
        for gp in &func.generic_params {
            self.env
                .insert_generic(gp.clone(), Type::Generic(gp.clone()));
        }

        let mut param_types = Vec::new();
        for expr in &func.body {
            if let Expr::Let {
                name,
                typ,
                value,
                is_const: _,
            } = expr
                && func.params.contains(name)
            {
                let value_type = self.infer_expr(value)?;
                let annotated = typ
                    .as_ref()
                    .map(|t| self.env.resolve_type(t, self.self_struct_type.as_deref()));
                let final_type = annotated.unwrap_or(value_type);
                param_types.push(final_type);
            }
        }

        for (i, param) in func.params.iter().enumerate() {
            let typ = if param == "self" {
                if let Some(ref struct_name) = self.self_struct_type {
                    Type::Struct(struct_name.clone(), Vec::new())
                } else {
                    param_types.get(i).cloned().unwrap_or(Type::I32)
                }
            } else {
                param_types.get(i).cloned().unwrap_or(Type::I32)
            };
            self.env.insert_param(param.clone(), typ);
        }

        self.env.locals.clear();

        for expr in &func.body {
            if let Expr::Let {
                name,
                typ,
                value,
                is_const: _,
            } = expr
            {
                let value_type = self.infer_expr(value)?;
                let annotated = typ
                    .as_ref()
                    .map(|t| self.env.resolve_type(t, self.self_struct_type.as_deref()));
                let final_type = annotated.unwrap_or(value_type);
                self.env.insert_local(name.clone(), final_type);
            }
            self.check_expr(expr)?;
        }

        for param in &func.params {
            self.env.params.remove(param);
        }
        self.env.locals.clear();
        for gp in &func.generic_params {
            self.env.generics.remove(gp);
        }

        Ok(())
    }

    fn infer_expr(&mut self, expr: &Expr) -> TypeResult<Type> {
        match expr {
            Expr::Import(_, _) => Ok(Type::Void),
            Expr::List(items) => {
                if items.is_empty() {
                    return Ok(Type::Struct("List".to_string(), vec![]));
                }
                let first_type = self.infer_expr(&items[0])?;
                for item in &items[1..] {
                    let t = self.infer_expr(item)?;
                    if t != first_type {
                        return Err(TypeError::new("all list elements must have the same type"));
                    }
                }
                Ok(Type::Struct("List".to_string(), vec![first_type]))
            }
            Expr::Index { target, index } => {
                let target_type = self.infer_expr(target)?;
                let index_type = self.infer_expr(index)?;
                if index_type != Type::I32 {
                    return Err(TypeError::new("list index must be an integer"));
                }
                match &target_type {
                    Type::String | Type::I32 | Type::Unknown => {}
                    Type::Struct(name, elem_types) if name == "List" => {
                        if let Some(elem_ty) = elem_types.first() {
                            return Ok(elem_ty.clone());
                        }
                    }
                    _ => return Err(TypeError::new("cannot index into this type")),
                }
                Ok(Type::I32) // array elements are i32 for now
            }
            Expr::AssignIndex { name, index, value } => {
                let var_type = self.env.get(name);
                if var_type == Type::Unknown {
                    return Err(TypeError::new(format!("unknown variable: {}", name)));
                }
                let index_type = self.infer_expr(index)?;
                if index_type != Type::I32 {
                    return Err(TypeError::new("list index must be an integer"));
                }
                let value_type = self.infer_expr(value)?;
                if value_type != Type::I32 {
                    return Err(TypeError::new("list element value must be i32"));
                }
                Ok(Type::Void)
            }
            Expr::Literal(Literal::Int(_), _) => Ok(Type::I32),
            Expr::Literal(Literal::Float(_), _) => Ok(Type::F64),
            Expr::Literal(Literal::Bool(_), _) => Ok(Type::Bool),
            Expr::Literal(Literal::Str(_), _) => Ok(Type::String),
            Expr::StringLiteral(_, _) | Expr::MultilineString(_, _) | Expr::FString(_, _) => {
                Ok(Type::String)
            }
            Expr::Borrow { .. } => Ok(Type::String),
            Expr::Identifier(name, _) => Ok(self.env.get(name)),
            Expr::Call { func, args } => self.check_call(func, args),
            Expr::Binary(l, op, r, _) => self.check_binary_op(l, op, r),
            Expr::Unary(op, inner, _) => self.check_unary_op(op, inner),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.infer_expr(condition)?;
                if cond_type != Type::Bool && cond_type != Type::I32 {
                    return Err(TypeError::new("if condition must be boolean"));
                }

                let then_type = self.infer_block_type(then_branch);
                if let Some(else_block) = else_branch {
                    let else_type = self.infer_block_type(else_block);
                    if then_type != Type::Void && else_type != Type::Void && then_type != else_type
                    {
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
            Expr::Let {
                name,
                typ,
                value,
                is_const: _,
            } => {
                let value_type = self.infer_expr(value)?;
                let annotated = typ
                    .as_ref()
                    .map(|t| self.env.resolve_type(t, self.self_struct_type.as_deref()));

                if let Some(expected) = &annotated
                    && !self.types_compatible(&value_type, expected)
                {
                    return Err(TypeError::new(format!(
                        "cannot assign {} to variable of type {}",
                        self.type_name(&value_type),
                        self.type_name(expected)
                    )));
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
            Expr::Return(inner, _) => self.infer_expr(inner),
            Expr::While { condition, body } => {
                let cond_type = self.infer_expr(condition)?;
                if cond_type != Type::Bool && cond_type != Type::I32 {
                    return Err(TypeError::new("while condition must be boolean"));
                }
                self.infer_block_type(body);
                Ok(Type::Void)
            }
            Expr::For {
                variable,
                iterable,
                body,
            } => {
                let iter_type = self.infer_expr(iterable)?;
                if iter_type != Type::String && iter_type != Type::I32 {
                    return Err(TypeError::new("for iterable must be string or i32"));
                }
                self.env.insert_local(variable.clone(), Type::I32);
                self.infer_block_type(body);
                Ok(Type::Void)
            }
            Expr::ForIn {
                variable,
                iterable,
                body,
            } => {
                let iter_type = self.infer_expr(iterable)?;
                match &iter_type {
                    Type::String => {
                        self.env.insert_local(variable.clone(), Type::I32);
                    }
                    Type::Struct(name, elem_types) if name == "List" => {
                        let elem_type = elem_types.first().cloned().unwrap_or(Type::I32);
                        self.env.insert_local(variable.clone(), elem_type);
                    }
                    _ => {
                        return Err(TypeError::new("for iterable must be string or list"));
                    }
                }
                self.infer_block_type(body);
                Ok(Type::Void)
            }
            Expr::ForRange {
                variable,
                start,
                end,
                body,
            } => {
                let start_type = self.infer_expr(start)?;
                let end_type = self.infer_expr(end)?;
                if start_type != Type::I32 || end_type != Type::I32 {
                    return Err(TypeError::new("for range bounds must be integers"));
                }
                self.env.insert_local(variable.clone(), Type::I32);
                self.infer_block_type(body);
                Ok(Type::Void)
            }
            Expr::Match { expr, arms } => {
                let match_type = self.infer_expr(expr)?;
                if match_type != Type::I32 && !matches!(match_type, Type::Enum(_)) {
                    return Err(TypeError::new(
                        "match expression must be an integer or enum",
                    ));
                }
                if arms.is_empty() {
                    return Err(TypeError::new("match must have at least one arm"));
                }
                let mut seen_wildcard = false;
                for arm in arms {
                    if matches!(arm.pattern, crate::ast::MatchPattern::Wildcard) {
                        seen_wildcard = true;
                    } else if seen_wildcard {
                        return Err(TypeError::new("wildcard arm must be the last arm"));
                    }

                    // For enum variant patterns, validate variant exists and add bindings
                    if let crate::ast::MatchPattern::Variant { name, bindings } = &arm.pattern {
                        if let Type::Enum(ref enum_name) = match_type {
                            let enums = self.env.enums.clone();
                            let edef = enums.iter().find(|e| &e.name == enum_name);
                            if let Some(e) = edef {
                                let variant = e.variants.iter().find(|v| &v.name == name);
                                if variant.is_none() {
                                    return Err(TypeError::new(format!(
                                        "unknown variant {} for enum {}",
                                        name, enum_name
                                    )));
                                }
                                let v = variant.unwrap();
                                if bindings.len() != v.fields.len() {
                                    return Err(TypeError::new(format!(
                                        "variant {} expects {} bindings, got {}",
                                        name,
                                        v.fields.len(),
                                        bindings.len()
                                    )));
                                }
                                for (i, binding) in bindings.iter().enumerate() {
                                    let field_type = Type::from_str(&v.fields[i]).unwrap();
                                    self.env.insert_local(binding.clone(), field_type);
                                }
                            } else {
                                return Err(TypeError::new(format!(
                                    "unknown enum type {}",
                                    enum_name
                                )));
                            }
                        } else {
                            return Err(TypeError::new(
                                "variant pattern can only be used on enum types",
                            ));
                        }
                    }
                }
                // Infer from first arm
                let ret_type = self.infer_expr(&arms[0].body)?;
                for arm in &arms[1..] {
                    let t = self.infer_expr(&arm.body)?;
                    if t != ret_type && ret_type != Type::Void {
                        return Err(TypeError::new("all match arms must return the same type"));
                    }
                }
                Ok(ret_type)
            }
            Expr::Break | Expr::Continue => Ok(Type::Void),
            Expr::FieldAccess { target, field } => {
                let target_type = self.infer_expr(target)?;
                if let Type::Struct(struct_name, _) = &target_type {
                    if let Some(sdef) = self.env.structs.iter().find(|s| &s.name == struct_name)
                        && let Some(sf) = sdef.fields.iter().find(|f| &f.name == field)
                    {
                        return Ok(self.env.resolve_type_with_generics(
                            &sf.typ,
                            &self.env.generics,
                            self.self_struct_type.as_deref(),
                        ));
                    }
                    return Err(TypeError::new(format!("unknown field: {}", field)));
                }
                Err(TypeError::new("field access on non-struct type"))
            }
            Expr::FieldAssign {
                target,
                field,
                value,
            } => {
                let target_type = self.infer_expr(target)?;
                let value_type = self.infer_expr(value)?;
                if let Type::Struct(struct_name, _) = &target_type {
                    if let Some(sdef) = self.env.structs.iter().find(|s| &s.name == struct_name)
                        && let Some(sf) = sdef.fields.iter().find(|f| &f.name == field)
                    {
                        let expected = self.env.resolve_type_with_generics(
                            &sf.typ,
                            &self.env.generics,
                            self.self_struct_type.as_deref(),
                        );
                        if !self.types_compatible(&value_type, &expected) {
                            return Err(TypeError::new(format!(
                                "cannot assign {} to field {} of type {}",
                                self.type_name(&value_type),
                                field,
                                self.type_name(&expected)
                            )));
                        }
                        return Ok(Type::Void);
                    }
                    return Err(TypeError::new(format!("unknown field: {}", field)));
                }
                Err(TypeError::new("field assignment on non-struct type"))
            }
            Expr::StructLiteral { name, fields } => {
                let struct_def = self.env.structs.iter().find(|s| &s.name == name).cloned();
                if let Some(sdef) = struct_def {
                    for (fname, fval) in fields {
                        let val_type = self.infer_expr(fval)?;
                        if let Some(sf) = sdef.fields.iter().find(|f| &f.name == fname) {
                            let expected = self.env.resolve_type_with_generics(
                                &sf.typ,
                                &self.env.generics,
                                self.self_struct_type.as_deref(),
                            );
                            if !self.types_compatible(&val_type, &expected) {
                                return Err(TypeError::new(format!(
                                    "field {} has type {}, got {}",
                                    fname,
                                    sf.typ,
                                    self.type_name(&val_type)
                                )));
                            }
                        } else {
                            return Err(TypeError::new(format!("unknown field: {}", fname)));
                        }
                    }
                    return Ok(Type::Struct(name.clone(), Vec::new()));
                }
                Err(TypeError::new(format!("unknown struct: {}", name)))
            }
            Expr::Tuple(elements, _) => {
                let mut types = Vec::new();
                for elem in elements {
                    types.push(self.infer_expr(elem)?);
                }
                Ok(Type::Tuple(types))
            }
            Expr::TupleAccess { target, index } => {
                let target_type = self.infer_expr(target)?;
                if let Type::Tuple(types) = &target_type {
                    if *index < types.len() {
                        return Ok(types[*index].clone());
                    }
                    return Err(TypeError::new(format!(
                        "tuple index {} out of bounds",
                        index
                    )));
                }
                Err(TypeError::new("tuple access on non-tuple type"))
            }
            Expr::EnumLiteral {
                enum_name,
                variant_name,
                args,
            } => {
                let enum_def = self
                    .env
                    .enums
                    .iter()
                    .find(|e| &e.name == enum_name)
                    .cloned();
                if let Some(edef) = enum_def {
                    let variant = edef.variants.iter().find(|v| &v.name == variant_name);
                    if let Some(v) = variant {
                        if v.fields.len() != args.len() {
                            return Err(TypeError::new(format!(
                                "variant {} expects {} fields, got {}",
                                variant_name,
                                v.fields.len(),
                                args.len()
                            )));
                        }
                        for arg in args {
                            self.infer_expr(arg)?;
                        }
                        return Ok(Type::Enum(enum_name.clone()));
                    }
                    return Err(TypeError::new(format!("unknown variant: {}", variant_name)));
                }
                Err(TypeError::new(format!("unknown enum: {}", enum_name)))
            }
        }
    }

    fn infer_expr_without_env(&self, expr: &Expr) -> Type {
        match expr {
            Expr::Literal(Literal::Int(_), _) => Type::I32,
            Expr::Literal(Literal::Float(_), _) => Type::F64,
            Expr::Literal(Literal::Bool(_), _) => Type::Bool,
            Expr::Literal(Literal::Str(_), _) => Type::String,
            Expr::StringLiteral(_, _) | Expr::MultilineString(_, _) | Expr::FString(_, _) => {
                Type::String
            }
            Expr::List(_) => Type::String,
            Expr::Index { .. } => Type::I32,
            Expr::Match { arms, .. } => {
                if let Some(first) = arms.first() {
                    self.infer_expr_without_env(&first.body)
                } else {
                    Type::Void
                }
            }
            Expr::Call { func, args: _ } => {
                if let Some(ret) = self.env.functions.get(func) {
                    ret.clone()
                } else if let Some(ret) = self.builtin_return_type(func) {
                    ret
                } else {
                    Type::Void
                }
            }
            Expr::EnumLiteral { enum_name, .. } => Type::Enum(enum_name.clone()),
            Expr::Tuple(elements, _) => {
                if let Some(first) = elements.first() {
                    self.infer_expr_without_env(first)
                } else {
                    Type::Void
                }
            }
            Expr::TupleAccess { .. } => Type::I32,
            _ => Type::Void,
        }
    }

    fn infer_block_type(&self, block: &Block) -> Type {
        for expr in &block.stmts {
            if let Expr::Return(inner, _) = expr {
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

    fn check_call(&mut self, func: &str, args: &[Expr]) -> TypeResult<Type> {
        if let Some(ret) = self.builtin_return_type(func) {
            return Ok(ret);
        }

        let resolved = self.resolve_function_name(func);

        if let Some(ret) = self.env.functions.get(&resolved) {
            if let Some((generic_params, return_type_annot)) =
                self.env.generic_functions.get(&resolved).cloned()
                && !generic_params.is_empty()
                && let Some(ret_str) = return_type_annot
            {
                let mut type_map: HashMap<String, Type> = HashMap::new();
                for (i, gp) in generic_params.iter().enumerate() {
                    if let Some(arg) = args.get(i) {
                        let arg_type = self.infer_expr(arg)?;
                        type_map.insert(gp.clone(), arg_type);
                    }
                }
                return Ok(self.env.resolve_type_with_generics(
                    &ret_str,
                    &type_map,
                    self.self_struct_type.as_deref(),
                ));
            }
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
            "__rt_wrap_string" => return Some(Type::String),
            "__rt_read_file" => return Some(Type::String),
            "__rt_write_file" => return Some(Type::I32),
            "__rt_exit" | "__rt_print_str" | "__rt_print_int" | "__rt_print_float"
            | "__rt_free" => return Some(Type::Void),
            "__rt_contains" | "__rt_starts_with" | "__rt_ends_with" | "__rt_to_int" => {
                return Some(Type::I32);
            }
            "__rt_substr"
            | "__rt_trim"
            | "__rt_to_uppercase"
            | "__rt_to_lowercase"
            | "__rt_read_line"
            | "__rt_to_string_int"
            | "__rt_to_string_float" => return Some(Type::String),
            "__rt_to_float" | "__rt_floor" | "__rt_ceil" | "__rt_round" | "__rt_sqrt"
            | "__rt_sin" | "__rt_cos" | "__rt_tan" | "__rt_abs" => return Some(Type::F64),
            "__rt_list_len" | "__rt_list_pop" => return Some(Type::I32),
            "__rt_list_push" => return Some(Type::String),
            "__rt_to_hex" | "__rt_str_repeat" => return Some(Type::String),
            _ => {}
        }

        if !self.stdlib_enabled {
            return None;
        }

        match func {
            "print" | "asm" | "exit" | "write_file" => Some(Type::Void),
            "len" => Some(Type::I32),
            "read_file" => Some(Type::String),
            "contains" | "starts_with" | "ends_with" | "to_int" | "is_empty" => Some(Type::I32),
            "substr" | "trim" | "to_uppercase" | "to_lowercase" | "to_string" | "read_line" => {
                Some(Type::String)
            }
            "to_float" | "floor" | "ceil" | "round" => Some(Type::F64),
            "sqrt" | "sin" | "cos" | "tan" => Some(Type::F64),
            "to_hex" | "str_repeat" => Some(Type::String),
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

        // String concatenation
        if matches!(op, BinOp::Add) && l_type == Type::String && r_type == Type::String {
            return Ok(Type::String);
        }

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
            Expr::Import(_, _) => Ok(Type::Void),
            Expr::Literal(_, _) => Ok(Type::Void),
            Expr::StringLiteral(_, _) | Expr::MultilineString(_, _) | Expr::FString(_, _) => {
                Ok(Type::Void)
            }
            Expr::Identifier(_, _) => Ok(Type::Void),
            Expr::Borrow { .. } => Ok(Type::Void),
            Expr::List(items) => {
                for item in items {
                    self.check_expr(item)?;
                }
                Ok(Type::Void)
            }
            Expr::Index { target, index } => {
                self.check_expr(target)?;
                self.check_expr(index)?;
                Ok(Type::Void)
            }
            Expr::AssignIndex { name, index, value } => {
                let var_type = self.env.get(name);
                if var_type == Type::Unknown {
                    return Err(TypeError::new(format!("unknown variable: {}", name)));
                }
                self.check_expr(index)?;
                self.check_expr(value)?;
                Ok(Type::Void)
            }
            Expr::Call { func, args } => {
                self.check_call(func, args)?;
                Ok(Type::Void)
            }
            Expr::Binary(l, op, r, _) => {
                self.check_binary_op(l, op, r)?;
                Ok(Type::Void)
            }
            Expr::Unary(op, inner, _) => {
                self.check_unary_op(op, inner)?;
                Ok(Type::Void)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.infer_expr(condition)?;
                if cond_type != Type::Bool && cond_type != Type::I32 {
                    return Err(TypeError::new("if condition must be boolean"));
                }

                let then_env = self.env.clone();
                let mut then_checker = TypeChecker {
                    env: then_env,
                    stdlib_enabled: self.stdlib_enabled,
                    function_aliases: self.function_aliases.clone(),
                    self_struct_type: self.self_struct_type.clone(),
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
                        self_struct_type: self.self_struct_type.clone(),
                    };
                    for expr in &else_block.stmts {
                        else_checker.check_expr(expr)?;
                    }
                }

                Ok(Type::Void)
            }
            Expr::Let {
                name,
                typ,
                value,
                is_const: _,
            } => {
                let value_type = self.infer_expr(value)?;
                let annotated = typ
                    .as_ref()
                    .map(|t| self.env.resolve_type(t, self.self_struct_type.as_deref()));

                if let Some(expected) = &annotated
                    && !self.types_compatible(&value_type, expected)
                {
                    return Err(TypeError::new(format!(
                        "cannot assign {} to variable of type {}",
                        self.type_name(&value_type),
                        self.type_name(expected)
                    )));
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
            Expr::Return(inner, _) => {
                self.infer_expr(inner)?;
                Ok(Type::Void)
            }
            Expr::While { condition, body } => {
                let cond_type = self.infer_expr(condition)?;
                if cond_type != Type::Bool && cond_type != Type::I32 {
                    return Err(TypeError::new("while condition must be boolean"));
                }
                for expr in &body.stmts {
                    self.check_expr(expr)?;
                }
                Ok(Type::Void)
            }
            Expr::For {
                variable,
                iterable,
                body,
            } => {
                let iter_type = self.infer_expr(iterable)?;
                if iter_type != Type::String && iter_type != Type::I32 {
                    return Err(TypeError::new("for iterable must be string or i32"));
                }
                self.env.insert_local(variable.clone(), Type::I32);
                for expr in &body.stmts {
                    self.check_expr(expr)?;
                }
                Ok(Type::Void)
            }
            Expr::ForIn {
                variable,
                iterable,
                body,
            } => {
                let iter_type = self.infer_expr(iterable)?;
                match &iter_type {
                    Type::String => {
                        self.env.insert_local(variable.clone(), Type::I32);
                    }
                    Type::Struct(name, elem_types) if name == "List" => {
                        let elem_type = elem_types.first().cloned().unwrap_or(Type::I32);
                        self.env.insert_local(variable.clone(), elem_type);
                    }
                    _ => {
                        return Err(TypeError::new("for iterable must be string or list"));
                    }
                }
                for expr in &body.stmts {
                    self.check_expr(expr)?;
                }
                Ok(Type::Void)
            }
            Expr::ForRange {
                variable,
                start,
                end,
                body,
            } => {
                let start_type = self.infer_expr(start)?;
                let end_type = self.infer_expr(end)?;
                if start_type != Type::I32 || end_type != Type::I32 {
                    return Err(TypeError::new("for range bounds must be integers"));
                }
                self.env.insert_local(variable.clone(), Type::I32);
                for expr in &body.stmts {
                    self.check_expr(expr)?;
                }
                Ok(Type::Void)
            }
            Expr::Match { expr, arms } => {
                self.check_expr(expr)?;
                for arm in arms {
                    self.check_expr(&arm.body)?;
                }
                Ok(Type::Void)
            }
            Expr::Break | Expr::Continue => Ok(Type::Void),
            Expr::FieldAccess { target, .. } => {
                self.check_expr(target)?;
                Ok(Type::Void)
            }
            Expr::FieldAssign { target, value, .. } => {
                self.check_expr(target)?;
                self.check_expr(value)?;
                Ok(Type::Void)
            }
            Expr::StructLiteral { fields, .. } => {
                for (_, val) in fields {
                    self.check_expr(val)?;
                }
                Ok(Type::Void)
            }
            Expr::Tuple(elements, _) => {
                for elem in elements {
                    self.check_expr(elem)?;
                }
                Ok(Type::Void)
            }
            Expr::TupleAccess { target, .. } => {
                self.check_expr(target)?;
                Ok(Type::Void)
            }
            Expr::EnumLiteral { args, .. } => {
                for arg in args {
                    self.check_expr(arg)?;
                }
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
            (Type::Struct(a, _), Type::Struct(b, _)) => a == b,
            (Type::Tuple(a), Type::Tuple(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|(x, y)| self.types_compatible(x, y))
            }
            (Type::Generic(_), _) => true,
            (_, Type::Generic(_)) => true,
            _ => false,
        }
    }

    fn type_name(&self, typ: &Type) -> String {
        match typ {
            Type::I32 => "i32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "str".to_string(),
            Type::Struct(name, _) => name.clone(),
            Type::Enum(name) => format!("enum {}", name),
            Type::Tuple(types) => format!(
                "({})",
                types
                    .iter()
                    .map(|t| self.type_name(t))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Void => "void".to_string(),
            Type::Unknown => "unknown".to_string(),
            Type::Generic(name) => name.clone(),
        }
    }
}
