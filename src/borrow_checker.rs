use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BorrowCheckError {
    pub message: String,
}

impl BorrowCheckError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for BorrowCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for BorrowCheckError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BindingKind {
    Copy,
    OwnedPtr,
    BorrowImm,
    BorrowMut,
    External,
}

#[derive(Debug, Clone)]
struct Binding {
    kind: BindingKind,
    valid: bool,
    imm_borrows: usize,
    has_mut_borrow: bool,
    borrowed_from: Option<String>,
}

impl Binding {
    fn new(kind: BindingKind) -> Self {
        Self {
            kind,
            valid: true,
            imm_borrows: 0,
            has_mut_borrow: false,
            borrowed_from: None,
        }
    }

    fn is_moveable(&self) -> bool {
        matches!(self.kind, BindingKind::OwnedPtr | BindingKind::BorrowMut)
    }

    fn is_reference(&self) -> bool {
        matches!(self.kind, BindingKind::BorrowImm | BindingKind::BorrowMut)
    }
}

pub struct BorrowChecker {
    bindings: HashMap<String, Binding>,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn check_program(&mut self, program: &Program) -> Result<(), BorrowCheckError> {
        self.bindings.clear();
        self.check_stmts(&program.globals)?;

        for func in &program.functions {
            self.check_function(func)?;
        }

        Ok(())
    }

    fn check_function(&mut self, func: &FunctionDef) -> Result<(), BorrowCheckError> {
        let saved = self.bindings.clone();
        self.bindings.clear();

        for param in &func.params {
            self.bindings.insert(param.clone(), Binding::new(BindingKind::External));
        }

        self.check_stmts(&func.body)?;
        self.bindings = saved;
        Ok(())
    }

    fn check_stmts(&mut self, stmts: &[Expr]) -> Result<(), BorrowCheckError> {
        for stmt in stmts {
            self.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Expr) -> Result<(), BorrowCheckError> {
        self.visit_expr(stmt)?;

        match stmt {
            Expr::Let { name, value, .. } => self.handle_let(name, value),
            Expr::Assign { name, value } => self.handle_assign(name, value),
            Expr::Return(inner) => self.handle_return(inner),
            Expr::If { .. } => Ok(()),
            _ => Ok(()),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<(), BorrowCheckError> {
        match expr {
            Expr::Import(_) => Ok(()),
            Expr::Identifier(name) => self.ensure_valid(name),
            Expr::Borrow { name, mutable } => self.check_borrow(name, *mutable),
            Expr::Binary(left, _, right) => {
                self.visit_expr(left)?;
                self.visit_expr(right)
            }
            Expr::Unary(_, inner) | Expr::Return(inner) => self.visit_expr(inner),
            Expr::Call { args, .. } => {
                for arg in args {
                    self.visit_expr(arg)?;
                }
                Ok(())
            }
            Expr::FString(_) => Ok(()),
            Expr::Let { value, .. } | Expr::Assign { value, .. } => self.visit_expr(value),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.visit_expr(condition)?;

                let saved = self.bindings.clone();
                self.check_stmts(&then_branch.stmts)?;

                self.bindings = saved.clone();
                if let Some(else_branch) = else_branch {
                    self.check_stmts(&else_branch.stmts)?;
                }
                self.bindings = saved;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn handle_let(&mut self, name: &str, value: &Expr) -> Result<(), BorrowCheckError> {
        match value {
            Expr::Borrow { name: src, mutable } => self.bind_borrow(name, src, *mutable),
            Expr::Identifier(src) => self.bind_from_identifier(name, src, true),
            Expr::StringLiteral(_) | Expr::MultilineString(_) | Expr::FString(_) => {
                self.replace_binding(name, Binding::new(BindingKind::OwnedPtr));
                Ok(())
            }
            _ => {
                self.replace_binding(name, Binding::new(BindingKind::Copy));
                Ok(())
            }
        }
    }

    fn handle_assign(&mut self, name: &str, value: &Expr) -> Result<(), BorrowCheckError> {
        let target = self
            .bindings
            .get(name)
            .ok_or_else(|| BorrowCheckError::new(format!("unknown variable: {}", name)))?;

        if target.is_reference() {
            return Err(BorrowCheckError::new(format!(
                "cannot assign to borrowed reference: {}",
                name
            )));
        }

        if target.imm_borrows > 0 || target.has_mut_borrow {
            return Err(BorrowCheckError::new(format!(
                "cannot assign to '{}' while it is borrowed",
                name
            )));
        }

        match value {
            Expr::Borrow { name: src, mutable } => self.bind_borrow(name, src, *mutable),
            Expr::Identifier(src) => self.bind_from_identifier(name, src, true),
            Expr::StringLiteral(_) | Expr::MultilineString(_) | Expr::FString(_) => {
                self.replace_binding(name, Binding::new(BindingKind::OwnedPtr));
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn handle_return(&mut self, inner: &Expr) -> Result<(), BorrowCheckError> {
        match inner {
            Expr::Borrow { .. } => Err(BorrowCheckError::new(
                "borrowed value cannot escape the current scope",
            )),
            Expr::Identifier(name) => {
                let binding = self
                    .bindings
                    .get(name)
                    .ok_or_else(|| BorrowCheckError::new(format!("unknown variable: {}", name)))?;

                if binding.is_reference() {
                    return Err(BorrowCheckError::new(
                        "borrowed value cannot escape the current scope",
                    ));
                }

                if binding.is_moveable() && (binding.imm_borrows > 0 || binding.has_mut_borrow) {
                    return Err(BorrowCheckError::new(format!(
                        "cannot move '{}' while it is borrowed",
                        name
                    )));
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn bind_borrow(
        &mut self,
        name: &str,
        src: &str,
        mutable: bool,
    ) -> Result<(), BorrowCheckError> {
        self.ensure_valid(src)?;

        {
            let binding = self
                .bindings
                .get(src)
                .ok_or_else(|| BorrowCheckError::new(format!("unknown variable: {}", src)))?;

            if mutable {
                if binding.imm_borrows > 0 || binding.has_mut_borrow {
                    return Err(BorrowCheckError::new(format!(
                        "cannot mutably borrow '{}': it is already borrowed",
                        src
                    )));
                }
            } else if binding.has_mut_borrow {
                return Err(BorrowCheckError::new(format!(
                    "cannot immutably borrow '{}': it already has a mutable borrow",
                    src
                )));
            }
        }

        let binding = self
            .bindings
            .get_mut(src)
            .ok_or_else(|| BorrowCheckError::new(format!("unknown variable: {}", src)))?;

        if mutable {
            binding.has_mut_borrow = true;
            let mut new_binding = Binding::new(BindingKind::BorrowMut);
            new_binding.borrowed_from = Some(src.to_string());
            self.replace_binding(name, new_binding);
        } else {
            binding.imm_borrows += 1;
            let mut new_binding = Binding::new(BindingKind::BorrowImm);
            new_binding.borrowed_from = Some(src.to_string());
            self.replace_binding(name, new_binding);
        }

        Ok(())
    }

    fn bind_from_identifier(
        &mut self,
        dst: &str,
        src: &str,
        allow_move: bool,
    ) -> Result<(), BorrowCheckError> {
        self.ensure_valid(src)?;

        let source = self
            .bindings
            .get(src)
            .cloned()
            .ok_or_else(|| BorrowCheckError::new(format!("unknown variable: {}", src)))?;

        if source.is_moveable() && allow_move {
            if source.imm_borrows > 0 || source.has_mut_borrow {
                return Err(BorrowCheckError::new(format!(
                    "cannot move '{}' while it is borrowed",
                    src
                )));
            }

            if let Some(binding) = self.bindings.get_mut(src) {
                binding.valid = false;
            }

            self.replace_binding(dst, Binding::new(source.kind));
            return Ok(());
        }

        if matches!(source.kind, BindingKind::BorrowMut) {
            return Err(BorrowCheckError::new(format!(
                "mutable borrow '{}' cannot be copied",
                src
            )));
        }

        self.replace_binding(dst, Binding::new(source.kind));
        Ok(())
    }

    fn check_borrow(&self, name: &str, mutable: bool) -> Result<(), BorrowCheckError> {
        self.ensure_valid(name)?;

        let binding = self
            .bindings
            .get(name)
            .ok_or_else(|| BorrowCheckError::new(format!("unknown variable: {}", name)))?;

        if mutable {
            if binding.imm_borrows > 0 || binding.has_mut_borrow {
                return Err(BorrowCheckError::new(format!(
                    "cannot mutably borrow '{}': it is already borrowed",
                    name
                )));
            }
        } else if binding.has_mut_borrow {
            return Err(BorrowCheckError::new(format!(
                "cannot immutably borrow '{}': it already has a mutable borrow",
                name
            )));
        }

        Ok(())
    }

    fn ensure_valid(&self, name: &str) -> Result<(), BorrowCheckError> {
        match self.bindings.get(name) {
            Some(binding) if binding.valid => Ok(()),
            Some(_) => Err(BorrowCheckError::new(format!(
                "use of moved value: {}",
                name
            ))),
            None => Err(BorrowCheckError::new(format!("unknown variable: {}", name))),
        }
    }

    fn replace_binding(&mut self, name: &str, new_binding: Binding) {
        if let Some(old) = self.bindings.remove(name) {
            if let Some(src) = old.borrowed_from {
                if let Some(source) = self.bindings.get_mut(&src) {
                    if old.kind == BindingKind::BorrowMut {
                        source.has_mut_borrow = false;
                    } else if source.imm_borrows > 0 {
                        source.imm_borrows -= 1;
                    }
                }
            }
        }

        self.bindings.insert(name.to_string(), new_binding);
    }
}
