use crate::ast::*;
use crate::lexer::{Lexer, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    fn with_location(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self {
            message: format!("{} (line={}, col={})", message.into(), line, column),
        }
    }

    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current: Option<TokenKind>,
    current_col: usize,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        let first = lexer.next_token();
        let col = first.column;
        Self {
            lexer,
            current: Some(first.kind),
            current_col: col,
        }
    }

    fn advance(&mut self) {
        let tok = self.lexer.next_token();
        self.current_col = tok.column;
        self.current = Some(tok.kind);
    }

    fn current_span(&self) -> SourceSpan {
        SourceSpan::new(self.lexer.line, self.current_col)
    }

    fn eat(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        if self.current == Some(expected.clone()) {
            self.advance();
            Ok(())
        } else if self.current == Some(TokenKind::Newline) {
            self.advance();
            self.eat(expected)
        } else {
            Err(ParseError::with_location(
                format!("expected {:?}, but found {:?}", expected, self.current),
                self.lexer.line,
                self.lexer.column,
            ))
        }
    }

    fn get_precedence(tok: &Option<TokenKind>) -> i32 {
        match tok {
            Some(TokenKind::Or) => 5,
            Some(TokenKind::And) => 6,
            Some(TokenKind::StarStar) => 40,
            Some(TokenKind::Star) | Some(TokenKind::Slash) | Some(TokenKind::Percent) => 30,
            Some(TokenKind::Plus) | Some(TokenKind::Minus) => 20,
            Some(TokenKind::EqualEqual)
            | Some(TokenKind::NotEqual)
            | Some(TokenKind::Less)
            | Some(TokenKind::LessEqual)
            | Some(TokenKind::Greater)
            | Some(TokenKind::GreaterEqual) => 10,
            _ => -1,
        }
    }

    fn is_right_associative(tok: &Option<TokenKind>) -> bool {
        matches!(tok, Some(TokenKind::StarStar))
    }

    fn is_implicit_mul_start(tok: &Option<TokenKind>) -> bool {
        matches!(
            tok,
            Some(TokenKind::Identifier(_))
                | Some(TokenKind::IntLiteral(_))
                | Some(TokenKind::FloatLiteral(_))
                | Some(TokenKind::BoolLiteral(_))
                | Some(TokenKind::LParen)
                | Some(TokenKind::StringLiteral(_))
                | Some(TokenKind::MultilineString(_))
                | Some(TokenKind::FString(_))
        )
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.current.clone() {
            Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
                let span = self.current_span();
                let op = self.current.clone();
                self.advance();
                let expr = self.parse_primary()?;
                let unop = match op {
                    Some(TokenKind::Plus) => UnaryOp::Pos,
                    Some(TokenKind::Minus) => UnaryOp::Neg,
                    _ => unreachable!(),
                };
                Ok(Expr::Unary(unop, Box::new(expr), span))
            }
            Some(TokenKind::Ampersand) => {
                let span = self.current_span();
                self.advance();
                let mutable = if self.current == Some(TokenKind::Mut) {
                    self.advance();
                    true
                } else {
                    false
                };

                let name = if let Some(TokenKind::Identifier(name)) = self.current.clone() {
                    self.advance();
                    name
                } else {
                    return Err(ParseError::new("expected name after borrow operator"));
                };

                Ok(Expr::Borrow { name, mutable, span })
            }
            Some(TokenKind::Not) => {
                let span = self.current_span();
                self.advance();
                let expr = self.parse_primary()?;
                Ok(Expr::Unary(UnaryOp::Not, Box::new(expr), span))
            }
            Some(TokenKind::IntLiteral(n)) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::Int(n), span))
            }
            Some(TokenKind::FloatLiteral(n)) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::Float(n), span))
            }
            Some(TokenKind::BoolLiteral(b)) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Literal(Literal::Bool(b), span))
            }
            Some(TokenKind::Identifier(name)) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::Identifier(name, span))
            }
            Some(TokenKind::LParen) => {
                let span = self.current_span();
                self.advance();
                let first = self.parse_expr()?;
                if self.current == Some(TokenKind::Comma) {
                    let mut elements = vec![first];
                    while self.current == Some(TokenKind::Comma) {
                        self.advance();
                        elements.push(self.parse_expr()?);
                    }
                    self.eat(TokenKind::RParen)?;
                    Ok(Expr::Tuple(elements, span))
                } else {
                    self.eat(TokenKind::RParen)?;
                    Ok(first)
                }
            }
            Some(TokenKind::StringLiteral(s)) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::StringLiteral(s, span))
            }
            Some(TokenKind::MultilineString(s)) => {
                let span = self.current_span();
                self.advance();
                Ok(Expr::MultilineString(s, span))
            }
            Some(TokenKind::FString(s)) => {
                let span = self.current_span();
                self.advance();
                Ok(self.parse_fstring(&s, span))
            }
            Some(TokenKind::LBracket) => {
                self.advance();
                let mut elements = Vec::new();
                if self.current != Some(TokenKind::RBracket) {
                    loop {
                        elements.push(self.parse_expr()?);
                        if self.current == Some(TokenKind::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                self.eat(TokenKind::RBracket)?;
                Ok(Expr::List(elements))
            }
            Some(TokenKind::Invalid(ch)) => Err(ParseError::with_location(
                format!("unknown symbol '{}'", ch),
                self.lexer.line,
                self.lexer.column,
            )),
            _ => Err(ParseError::new(format!(
                "expected primary expression, found {:?} (line={}, col={})",
                self.current, self.lexer.line, self.lexer.column
            ))),
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        self.eat(TokenKind::If)?;
        let condition = Box::new(self.parse_expr()?);
        self.eat(TokenKind::Colon)?;

        let then_branch = self.parse_block()?;

        let else_branch = self.parse_else_or_elif()?;

        Ok(Expr::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    fn parse_else_or_elif(&mut self) -> Result<Option<Box<Block>>, ParseError> {
        if self.current == Some(TokenKind::Elif) {
            self.advance();
            // elif cond: body desugars to else { if cond: body ... }
            let elif_condition = Box::new(self.parse_expr()?);
            self.eat(TokenKind::Colon)?;
            let elif_body = self.parse_block()?;
            let rest = self.parse_else_or_elif()?;
            Ok(Some(Box::new(Block {
                stmts: vec![Expr::If {
                    condition: elif_condition,
                    then_branch: Box::new(elif_body),
                    else_branch: rest,
                }]
            })))
        } else if self.current == Some(TokenKind::Else) {
            self.advance();
            self.eat(TokenKind::Colon)?;
            if self.current == Some(TokenKind::If) {
                let nested_if = self.parse_if_expr()?;
                Ok(Some(Box::new(Block { stmts: vec![nested_if] })))
            } else {
                let else_block = self.parse_block()?;
                Ok(Some(Box::new(else_block)))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_while_expr(&mut self) -> Result<Expr, ParseError> {
        self.eat(TokenKind::While)?;
        let condition = Box::new(self.parse_expr()?);
        self.eat(TokenKind::Colon)?;

        let body = self.parse_block()?;

        Ok(Expr::While {
            condition,
            body: Box::new(body),
        })
    }

    fn parse_for_expr(&mut self) -> Result<Expr, ParseError> {
        self.eat(TokenKind::For)?;

        let variable = if let Some(TokenKind::Identifier(name)) = self.current.clone() {
            self.advance();
            name
        } else {
            return Err(ParseError::new("expected variable name after 'for'"));
        };

        if self.current == Some(TokenKind::In) {
            self.advance();
            let start = Box::new(self.parse_expr()?);
            self.eat(TokenKind::DotDot)?;
            let end = Box::new(self.parse_expr()?);
            self.eat(TokenKind::Colon)?;
            let body = self.parse_block()?;
            Ok(Expr::ForRange {
                variable,
                start,
                end,
                body: Box::new(body),
            })
        } else {
            self.eat(TokenKind::Equal)?;
            let iterable = Box::new(self.parse_expr()?);
            self.eat(TokenKind::Colon)?;
            let body = self.parse_block()?;
            Ok(Expr::For {
                variable,
                iterable,
                body: Box::new(body),
            })
        }
    }

    fn parse_break_expr(&mut self) -> Result<Expr, ParseError> {
        self.eat(TokenKind::Break)?;
        Ok(Expr::Break)
    }

    fn parse_continue_expr(&mut self) -> Result<Expr, ParseError> {
        self.eat(TokenKind::Continue)?;
        Ok(Expr::Continue)
    }

    fn parse_import_stmt(&mut self) -> Result<Expr, ParseError> {
        let span = self.current_span();
        self.eat(TokenKind::Import)?;

        let spec = match self.current.clone() {
            Some(TokenKind::StringLiteral(path)) => {
                self.advance();
                path
            }
            Some(TokenKind::Identifier(name)) => {
                self.advance();
                let mut parts = vec![name];
                while self.current == Some(TokenKind::Dot) {
                    self.advance();
                    if let Some(TokenKind::Identifier(part)) = self.current.clone() {
                        self.advance();
                        parts.push(part);
                    } else {
                        return Err(ParseError::with_location(
                            "expected module name after '.'",
                            self.lexer.line,
                            self.lexer.column,
                        ));
                    }
                }
                parts.join("/")
            }
            _ => {
                return Err(ParseError::with_location(
                    "expected module name or string literal after import",
                    self.lexer.line,
                    self.lexer.column,
                ));
            }
        };

        Ok(Expr::Import(spec, span))
    }

    pub fn parse_program(&mut self, profile: String, name: String) -> Result<Program, ParseError> {
        let mut functions = Vec::new();
        let mut globals = Vec::new();
        let mut structs = Vec::new();
        let mut enums = Vec::new();
        let mut type_aliases = Vec::new();
        let mut impls = Vec::new();

        while self.current != Some(TokenKind::Eof) {
            while self.current == Some(TokenKind::Newline) {
                self.advance();
            }

            if self.current == Some(TokenKind::Eof) {
                break;
            }

            match self.current.clone() {
                Some(TokenKind::Import) => globals.push(self.parse_import_stmt()?),
                Some(TokenKind::Struct) => structs.push(self.parse_struct_def()?),
                Some(TokenKind::Enum) => enums.push(self.parse_enum_def()?),
                Some(TokenKind::Type) => type_aliases.push(self.parse_type_alias()?),
                Some(TokenKind::Fn) => functions.push(self.parse_function()?),
                Some(TokenKind::Extern) => functions.push(self.parse_extern_function()?),
                Some(TokenKind::Impl) => impls.push(self.parse_impl_def()?),
                Some(TokenKind::Let)
                | Some(TokenKind::Const)
                | Some(TokenKind::Identifier(_))
                | Some(TokenKind::IntLiteral(_))
                | Some(TokenKind::FloatLiteral(_))
                | Some(TokenKind::BoolLiteral(_))
                | Some(TokenKind::LParen)
                | Some(TokenKind::LBracket)
                | Some(TokenKind::Plus)
                | Some(TokenKind::Minus)
                | Some(TokenKind::Not)
                | Some(TokenKind::StringLiteral(_))
                | Some(TokenKind::MultilineString(_))
                | Some(TokenKind::Ampersand)
                | Some(TokenKind::If)
                | Some(TokenKind::While)
                | Some(TokenKind::For)
                | Some(TokenKind::Match)
                | Some(TokenKind::Return)
                | Some(TokenKind::EqualEqual)
                | Some(TokenKind::FString(_)) => {
                    globals.push(self.parse_expr()?);
                }
                Some(TokenKind::Invalid(ch)) => {
                    return Err(ParseError::with_location(
                        format!("unknown symbol '{}'", ch),
                        self.lexer.line,
                        self.lexer.column,
                    ));
                }
                Some(_tok) => {
                    return Err(ParseError::new(format!(
                        "expected define of the function or expression, but found {:?} (line={}, col={})",
                        _tok,
                        self.lexer.line,
                        self.lexer.column
                    )));
                }
                None => break,
            }
        }

        Ok(Program {
            globals,
            functions,
            structs,
            enums,
            type_aliases,
            impls,
            profile,
            name,
        })
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let mut stmts = Vec::new();

        while self.current == Some(TokenKind::Newline) {
            self.advance();
        }

        match self.current {
            Some(TokenKind::Else) | Some(TokenKind::Elif) | Some(TokenKind::Eof) | Some(TokenKind::Fn) => {
                return Ok(Block { stmts });
            }
            _ => {}
        }

        let block_indent = self.current_col;

        loop {
            while self.current == Some(TokenKind::Newline) {
                self.advance();
                match self.current {
                    Some(TokenKind::Else) | Some(TokenKind::Elif) | Some(TokenKind::Eof) | Some(TokenKind::Fn) => {
                        return Ok(Block { stmts });
                    }
                    _ => {}
                }
            }

            if self.current_col < block_indent {
                break;
            }

            match self.current {
                Some(TokenKind::Else) | Some(TokenKind::Elif) | Some(TokenKind::Eof) | Some(TokenKind::Fn) => break,
                None => break,
                _ => {}
            }

            let expr = self.parse_expr()?;
            stmts.push(expr);
        }

        Ok(Block { stmts })
    }

    fn parse_function(&mut self) -> Result<FunctionDef, ParseError> {
        self.eat(TokenKind::Fn)?;

        let name = if let Some(TokenKind::Identifier(n)) = self.current.clone() {
            self.advance();
            n
        } else {
            return Err(ParseError::new("expected name of the function"));
        };

        self.eat(TokenKind::LParen)?;
        let mut params = Vec::new();
        let mut param_types = Vec::new();
        while let Some(TokenKind::Identifier(p)) = self.current.clone() {
            params.push(p);
            self.advance();
            if self.current == Some(TokenKind::Colon) {
                self.advance();
                param_types.push(Some(self.parse_type()?));
            } else {
                param_types.push(None);
            }
            if self.current == Some(TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        self.eat(TokenKind::RParen)?;
        let mut return_type = None;
        if self.current == Some(TokenKind::Minus) {
            self.advance();
            if self.current == Some(TokenKind::Greater) {
                self.advance();
                return_type = Some(self.parse_type()?);
            }
        }
        self.eat(TokenKind::Colon)?;

        let block = self.parse_block()?;

        Ok(FunctionDef {
            name,
            params,
            param_types,
            return_type,
            body: block.stmts,
        })
    }

    fn parse_extern_function(&mut self) -> Result<FunctionDef, ParseError> {
        self.eat(TokenKind::Extern)?;
        self.eat(TokenKind::Fn)?;

        let name = if let Some(TokenKind::Identifier(n)) = self.current.clone() {
            self.advance();
            n
        } else {
            return Err(ParseError::new("expected name of the external function"));
        };

        self.eat(TokenKind::LParen)?;
        let mut params = Vec::new();
        let mut param_types = Vec::new();
        while let Some(TokenKind::Identifier(p)) = self.current.clone() {
            params.push(p);
            self.advance();
            if self.current == Some(TokenKind::Colon) {
                self.advance();
                param_types.push(Some(self.parse_type()?));
            } else {
                return Err(ParseError::new("extern function parameters must have type annotations"));
            }
            if self.current == Some(TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        self.eat(TokenKind::RParen)?;
        let mut return_type = None;
        if self.current == Some(TokenKind::Minus) {
            self.advance();
            if self.current == Some(TokenKind::Greater) {
                self.advance();
                return_type = Some(self.parse_type()?);
            }
        }

        Ok(FunctionDef {
            name,
            params,
            param_types,
            return_type,
            body: Vec::new(),
        })
    }

    fn parse_type(&mut self) -> Result<String, ParseError> {
        match self.current.clone() {
            Some(TokenKind::Identifier(s)) => {
                let type_name = s;
                self.advance();
                Ok(type_name)
            }
            Some(tok) => Err(ParseError::new(format!(
                "expected type annotation but found {:?} (line={}, col={})",
                tok,
                self.lexer.line,
                self.lexer.column
            ))),
            None => Err(ParseError::new("expected type annotation")),
        }
    }

    fn parse_struct_def(&mut self) -> Result<StructDef, ParseError> {
        self.advance(); // consume 'struct'
        let name = if let Some(TokenKind::Identifier(n)) = self.current.clone() {
            self.advance();
            n
        } else {
            return Err(ParseError::new("expected name of the struct"));
        };
        self.eat(TokenKind::Colon)?;
        let block = self.parse_block()?;
        let mut fields = Vec::new();
        for expr in block.stmts {
            if let Expr::Let { name, typ, .. } = expr {
                let field_type = typ.unwrap_or_else(|| "i32".to_string());
                fields.push(StructField { name, typ: field_type });
            }
        }
        Ok(StructDef { name, fields })
    }

    fn parse_enum_def(&mut self) -> Result<EnumDef, ParseError> {
        self.advance(); // consume 'enum'
        let name = if let Some(TokenKind::Identifier(n)) = self.current.clone() {
            self.advance();
            n
        } else {
            return Err(ParseError::new("expected name of the enum"));
        };
        self.eat(TokenKind::Colon)?;
        let block = self.parse_block()?;
        let mut variants = Vec::new();
        for expr in block.stmts {
            if let Expr::Identifier(vname, _) = expr {
                variants.push(EnumVariant { name: vname, fields: Vec::new() });
            } else if let Expr::Call { func, args } = expr {
                let fields: Vec<String> = args.into_iter().map(|a| {
                    if let Expr::Identifier(n, _) = a { n } else { "value".to_string() }
                }).collect();
                variants.push(EnumVariant { name: func, fields });
            }
        }
        Ok(EnumDef { name, variants })
    }

    fn parse_type_alias(&mut self) -> Result<TypeAlias, ParseError> {
        self.advance(); // consume 'type'
        let name = if let Some(TokenKind::Identifier(n)) = self.current.clone() {
            self.advance();
            n
        } else {
            return Err(ParseError::new("expected name for type alias"));
        };
        self.eat(TokenKind::Equal)?;
        let value = if let Some(TokenKind::Identifier(v)) = self.current.clone() {
            self.advance();
            v
        } else {
            return Err(ParseError::new("expected type value for type alias"));
        };
        Ok(TypeAlias { name, value })
    }

    fn parse_impl_def(&mut self) -> Result<ImplDef, ParseError> {
        self.advance(); // consume 'impl'
        let struct_name = if let Some(TokenKind::Identifier(n)) = self.current.clone() {
            self.advance();
            n
        } else {
            return Err(ParseError::new("expected struct name for impl block"));
        };
        self.eat(TokenKind::Colon)?;

        // Skip newlines
        while self.current == Some(TokenKind::Newline) {
            self.advance();
        }

        let mut methods = Vec::new();
        let block_indent = self.current_col;

        loop {
            // Skip newlines between methods
            while self.current == Some(TokenKind::Newline) {
                self.advance();
            }

            // Check if we've gone back out (less indentation)
            if self.current_col < block_indent {
                break;
            }

            // Check for EOF or top-level keywords
            match self.current {
                Some(TokenKind::Eof) | None => break,
                _ => {}
            }

            // Each method starts with 'fn'
            if self.current == Some(TokenKind::Fn) {
                methods.push(self.parse_function()?);
            } else {
                break;
            }
        }

        Ok(ImplDef { struct_name, methods })
    }

    fn parse_binary(&mut self, mut left: Expr, min_prec: i32) -> Result<Expr, ParseError> {
        loop {
            let op_opt = self.current.clone();
            let is_implicit = Self::is_implicit_mul_start(&op_opt);
            let prec = if is_implicit { 30 } else { Self::get_precedence(&op_opt) };
            if prec < min_prec {
                break;
            }

            if !is_implicit {
                self.advance();
            }

            let binop = if is_implicit {
                BinOp::Mul
            } else {
                match op_opt {
                    Some(TokenKind::Plus) => BinOp::Add,
                    Some(TokenKind::Minus) => BinOp::Sub,
                    Some(TokenKind::Star) => BinOp::Mul,
                    Some(TokenKind::Slash) => BinOp::Div,
                    Some(TokenKind::Percent) => BinOp::DivMod,
                    Some(TokenKind::StarStar) => BinOp::Pow,
                    Some(TokenKind::EqualEqual) => BinOp::Eq,
                    Some(TokenKind::NotEqual) => BinOp::Ne,
                    Some(TokenKind::Less) => BinOp::Lt,
                    Some(TokenKind::LessEqual) => BinOp::Le,
                    Some(TokenKind::Greater) => BinOp::Gt,
                    Some(TokenKind::GreaterEqual) => BinOp::Ge,
                    Some(TokenKind::And) => BinOp::And,
                    Some(TokenKind::Or) => BinOp::Or,
                    _ => {
                        return Err(ParseError::new(format!(
                            "unknown operator: {:?} (line={}, col={})",
                            op_opt, self.lexer.line, self.lexer.column
                        )))
                    }
                }
            };

            let mut right = self.parse_primary()?;
            let span = left.span();
            let next_min_prec = if Self::is_right_associative(&op_opt) {
                prec
            } else {
                prec + 1
            };

            loop {
                let next_prec = Self::get_precedence(&self.current);
                let next_is_implicit = Self::is_implicit_mul_start(&self.current);
                let next_prec = if next_is_implicit { 30 } else { next_prec };
                let should_recurse = if Self::is_right_associative(&op_opt) {
                    next_prec >= prec
                } else {
                    next_prec > prec
                };
                if should_recurse {
                    right = self.parse_binary(right, next_min_prec)?;
                } else {
                    break;
                }
            }

            left = Expr::Binary(Box::new(left), binop, Box::new(right), span);
        }
        Ok(left)
    }

    fn parse_match_expr(&mut self) -> Result<Expr, ParseError> {
        self.eat(TokenKind::Match)?;
        let expr = Box::new(self.parse_expr()?);
        self.eat(TokenKind::LCurly)?;
        let mut arms = Vec::new();
        loop {
            while self.current == Some(TokenKind::Newline) {
                self.advance();
            }
            if self.current == Some(TokenKind::RCurly) {
                break;
            }
            let pattern = match self.current.clone() {
                Some(TokenKind::IntLiteral(n)) => {
                    self.advance();
                    MatchPattern::Int(n)
                }
                Some(TokenKind::Identifier(ref name)) if name == "_" => {
                    self.advance();
                    MatchPattern::Wildcard
                }
                Some(TokenKind::Identifier(ref name)) => {
                    let variant_name = name.clone();
                    self.advance();
                    let mut bindings = Vec::new();
                    if self.current == Some(TokenKind::LParen) {
                        self.advance();
                        while self.current != Some(TokenKind::RParen) {
                            if let Some(TokenKind::Identifier(b)) = self.current.clone() {
                                self.advance();
                                bindings.push(b);
                            } else {
                                break;
                            }
                            if self.current == Some(TokenKind::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        self.eat(TokenKind::RParen)?;
                    }
                    MatchPattern::Variant { name: variant_name, bindings }
                }
                _ => break,
            };
            self.eat(TokenKind::Colon)?;
            let body = Box::new(self.parse_expr()?);
            arms.push(MatchArm { pattern, body });
            if self.current == Some(TokenKind::Comma) {
                self.advance();
            }
            while self.current == Some(TokenKind::Newline) {
                self.advance();
            }
            if self.current == Some(TokenKind::RCurly) {
                break;
            }
        }
        self.eat(TokenKind::RCurly)?;
        if arms.is_empty() {
            return Err(ParseError::new("match must have at least one arm"));
        }
        Ok(Expr::Match { expr, arms })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        while self.current == Some(TokenKind::Newline) {
            self.advance();
        }

        match self.current.clone() {
            Some(TokenKind::If) => self.parse_if_expr(),
            Some(TokenKind::While) => self.parse_while_expr(),
            Some(TokenKind::For) => self.parse_for_expr(),
            Some(TokenKind::Match) => self.parse_match_expr(),
            Some(TokenKind::Break) => self.parse_break_expr(),
            Some(TokenKind::Continue) => self.parse_continue_expr(),
            Some(TokenKind::Else) => Err(ParseError::with_location(
                "syntax error: 'else' without 'if'",
                self.lexer.line,
                self.lexer.column,
            )),
            Some(TokenKind::Elif) => Err(ParseError::with_location(
                "syntax error: 'elif' without 'if'",
                self.lexer.line,
                self.lexer.column,
            )),
            Some(TokenKind::Const) | Some(TokenKind::Let) => {
                let is_const = self.current == Some(TokenKind::Const);
                self.advance();
                let name = if let Some(TokenKind::Identifier(n)) = self.current.clone() {
                    self.advance();
                    n
                } else {
                    return Err(ParseError::with_location(
                        "expected name of the variable",
                        self.lexer.line,
                        self.lexer.column,
                    ));
                };

                let typ = if self.current == Some(TokenKind::Colon) {
                    self.advance();
                    if let Some(TokenKind::Identifier(t)) = self.current.clone() {
                        self.advance();
                        Some(t)
                    } else {
                        return Err(ParseError::with_location(
                            "expected type after ':'",
                            self.lexer.line,
                            self.lexer.column,
                        ));
                    }
                } else {
                    None
                };

                self.eat(TokenKind::Equal)?;
                let value = Box::new(self.parse_expr()?);
                Ok(Expr::Let {
                    name,
                    typ,
                    value,
                    is_const,
                })
            }
            Some(TokenKind::Return) => {
                let span = self.current_span();
                self.advance();
                if self.current == Some(TokenKind::Newline) || self.current == Some(TokenKind::Eof) {
                    Ok(Expr::Return(Box::new(Expr::Literal(Literal::Int(0), span)), span))
                } else {
                    let value = Box::new(self.parse_expr()?);
                    Ok(Expr::Return(value, span))
                }
            }
            Some(_tok) => {
                let mut left = self.parse_primary()?;

                if let Expr::Identifier(name, _) = &left {
                    match self.current.clone() {
                        Some(TokenKind::DoubleColon) => {
                            let enum_name = name.clone();
                            self.advance();
                            let variant_name = if let Some(TokenKind::Identifier(v)) = self.current.clone() {
                                self.advance();
                                v
                            } else {
                                return Err(ParseError::new(format!(
                                    "expected variant name after '::' (line={}, col={})",
                                    self.lexer.line, self.lexer.column
                                )));
                            };
                            let mut args = Vec::new();
                            if self.current == Some(TokenKind::LParen) {
                                self.advance();
                                while self.current != Some(TokenKind::RParen) {
                                    args.push(self.parse_expr()?);
                                    if self.current == Some(TokenKind::Comma) {
                                        self.advance();
                                    } else {
                                        break;
                                    }
                                }
                                self.eat(TokenKind::RParen)?;
                            }
                            left = Expr::EnumLiteral { enum_name, variant_name, args };
                        }
                        Some(TokenKind::Equal) => {
                            self.advance();
                            let value = Box::new(self.parse_expr()?);
                            left = Expr::Assign {
                                name: name.clone(),
                                value,
                            };
                        }
                        Some(TokenKind::PlusEqual) => {
                            self.advance();
                            let rhs = self.parse_expr()?;
                            left = Expr::Assign {
                                name: name.clone(),
                                value: Box::new(Expr::Binary(
                                    Box::new(Expr::Identifier(name.clone(), SourceSpan::unknown())),
                                    BinOp::Add,
                                    Box::new(rhs),
                                    SourceSpan::unknown(),
                                )),
                            };
                        }
                        Some(TokenKind::MinusEqual) => {
                            self.advance();
                            let rhs = self.parse_expr()?;
                            left = Expr::Assign {
                                name: name.clone(),
                                value: Box::new(Expr::Binary(
                                    Box::new(Expr::Identifier(name.clone(), SourceSpan::unknown())),
                                    BinOp::Sub,
                                    Box::new(rhs),
                                    SourceSpan::unknown(),
                                )),
                            };
                        }
                        Some(TokenKind::StarEqual) => {
                            self.advance();
                            let rhs = self.parse_expr()?;
                            left = Expr::Assign {
                                name: name.clone(),
                                value: Box::new(Expr::Binary(
                                    Box::new(Expr::Identifier(name.clone(), SourceSpan::unknown())),
                                    BinOp::Mul,
                                    Box::new(rhs),
                                    SourceSpan::unknown(),
                                )),
                            };
                        }
                        Some(TokenKind::SlashEqual) => {
                            self.advance();
                            let rhs = self.parse_expr()?;
                            left = Expr::Assign {
                                name: name.clone(),
                                value: Box::new(Expr::Binary(
                                    Box::new(Expr::Identifier(name.clone(), SourceSpan::unknown())),
                                    BinOp::Div,
                                    Box::new(rhs),
                                    SourceSpan::unknown(),
                                )),
                            };
                        }
                        Some(TokenKind::PercentEqual) => {
                            self.advance();
                            let rhs = self.parse_expr()?;
                            left = Expr::Assign {
                                name: name.clone(),
                                value: Box::new(Expr::Binary(
                                    Box::new(Expr::Identifier(name.clone(), SourceSpan::unknown())),
                                    BinOp::DivMod,
                                    Box::new(rhs),
                                    SourceSpan::unknown(),
                                )),
                            };
                        }
                        Some(TokenKind::StarStarEqual) => {
                            self.advance();
                            let rhs = self.parse_expr()?;
                            left = Expr::Assign {
                                name: name.clone(),
                                value: Box::new(Expr::Binary(
                                    Box::new(Expr::Identifier(name.clone(), SourceSpan::unknown())),
                                    BinOp::Pow,
                                    Box::new(rhs),
                                    SourceSpan::unknown(),
                                )),
                            };
                        }
                        Some(TokenKind::LBracket) => {
                            self.advance();
                            let index = self.parse_expr()?;
                            self.eat(TokenKind::RBracket)?;
                            if self.current == Some(TokenKind::Equal) {
                                self.advance();
                                let value = self.parse_expr()?;
                                left = Expr::AssignIndex {
                                    name: name.clone(),
                                    index: Box::new(index),
                                    value: Box::new(value),
                                };
                            } else {
                                left = Expr::Index {
                                    target: Box::new(left),
                                    index: Box::new(index),
                                };
                            }
                        }
                        Some(TokenKind::LParen) => {
                            self.advance();
                            let mut args = Vec::new();
                            while self.current != Some(TokenKind::RParen) {
                                args.push(self.parse_expr()?);
                                if self.current == Some(TokenKind::Comma) {
                                    self.advance();
                                } else {
                                    break;
                                }
                            }
                            self.eat(TokenKind::RParen)?;
                            left = Expr::Call {
                                func: name.clone(),
                                args,
                            };
                        }
                        Some(TokenKind::LCurly) => {
                            self.advance();
                            let mut fields = Vec::new();
                            loop {
                                while self.current == Some(TokenKind::Newline) {
                                    self.advance();
                                }
                                if self.current == Some(TokenKind::RCurly) {
                                    break;
                                }
                                if let Some(TokenKind::Identifier(fname)) = self.current.clone() {
                                    self.advance();
                                    self.eat(TokenKind::Colon)?;
                                    let fvalue = self.parse_expr()?;
                                    fields.push((fname, fvalue));
                                    if self.current == Some(TokenKind::Comma) {
                                        self.advance();
                                    } else {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            }
                            self.eat(TokenKind::RCurly)?;
                            left = Expr::StructLiteral {
                                name: name.clone(),
                                fields,
                            };
                        }
                        _ => {}
                    }
                }

                // Field access / method call / tuple access: receiver.field or receiver.method(args) or tup.0
                while self.current == Some(TokenKind::Dot) {
                    self.advance();
                    match self.current.clone() {
                        Some(TokenKind::IntLiteral(n)) => {
                            self.advance();
                            left = Expr::TupleAccess {
                                target: Box::new(left),
                                index: n as usize,
                            };
                        }
                        Some(TokenKind::Identifier(field)) => {
                            self.advance();
                            if self.current == Some(TokenKind::LParen) {
                                // Method call: receiver.method(args)
                                let mut args = vec![left];
                                self.advance();
                                while self.current != Some(TokenKind::RParen) {
                                    args.push(self.parse_expr()?);
                                    if self.current == Some(TokenKind::Comma) {
                                        self.advance();
                                    } else {
                                        break;
                                    }
                                }
                                self.eat(TokenKind::RParen)?;
                                left = Expr::Call {
                                    func: field,
                                    args,
                                };
                            } else if self.current == Some(TokenKind::Equal) {
                                // Field assignment: receiver.field = expr
                                self.advance();
                                let value = Box::new(self.parse_expr()?);
                                left = Expr::FieldAssign {
                                    target: Box::new(left),
                                    field,
                                    value,
                                };
                            } else {
                                // Field access: receiver.field
                                left = Expr::FieldAccess {
                                    target: Box::new(left),
                                    field,
                                };
                            }
                        }
                        _ => {
                            return Err(ParseError::new(format!(
                                "expected field name or tuple index after '.' (line={}, col={})",
                                self.lexer.line, self.lexer.column
                            )));
                        }
                    }
                }

                // Indexing syntax: expr[index]
                while self.current == Some(TokenKind::LBracket) {
                    self.advance();
                    let index = self.parse_expr()?;
                    self.eat(TokenKind::RBracket)?;
                    left = Expr::Index {
                        target: Box::new(left),
                        index: Box::new(index),
                    };
                }

                self.parse_binary(left, 0)
            }
            _ => Err(ParseError::new(format!(
                "unexpected token in expression: {:?} (line={}, col={})",
                self.current, self.lexer.line, self.lexer.column
            ))),
        }
    }

    fn parse_fstring(&self, s: &str, span: SourceSpan) -> Expr {
        let mut elements = Vec::new();
        let mut buf = String::new();
        let mut chars = s.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '{' {
                if !buf.is_empty() {
                    elements.push(Expr::StringLiteral(buf.clone(), SourceSpan::unknown()));
                    buf.clear();
                }
                let mut var_name = String::new();
                while let Some(&ch) = chars.peek() {
                    chars.next();
                    if ch == '}' {
                        break;
                    }
                    var_name.push(ch);
                }
                elements.push(Expr::Identifier(var_name, SourceSpan::unknown()));
            } else {
                buf.push(c);
            }
        }

        if !buf.is_empty() {
            elements.push(Expr::StringLiteral(buf, SourceSpan::unknown()));
        }

        Expr::FString(elements, span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Program {
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        parser
            .parse_program("debug".to_string(), "test".to_string())
            .unwrap()
    }

    fn extract_global_expr(program: &Program, index: usize) -> &Expr {
        &program.globals[index]
    }

    #[test]
    fn test_parse_literal() {
        let program = parse("42");
        assert_eq!(program.globals.len(), 1);
        match extract_global_expr(&program, 0) {
            Expr::Literal(Literal::Int(n), _) => assert_eq!(*n, 42),
            _ => panic!("expected integer literal"),
        }
    }

    #[test]
    fn test_parse_identifier() {
        let program = parse("x");
        match extract_global_expr(&program, 0) {
            Expr::Identifier(name, _) => assert_eq!(name, "x"),
            _ => panic!("expected identifier"),
        }
    }

    #[test]
    fn test_parse_binary_operations() {
        let program = parse("a + b * c");
        match extract_global_expr(&program, 0) {
            Expr::Binary(_, op, _, _) => match op {
                BinOp::Add => assert!(true),
                _ => panic!("expected Add operator"),
            },
            _ => panic!("expected binary expression"),
        }
    }

    #[test]
    fn test_parse_unary_operations() {
        let program = parse("-x");
        match extract_global_expr(&program, 0) {
            Expr::Unary(op, _, _) => match op {
                UnaryOp::Neg => assert!(true),
                _ => panic!("expected Neg operator"),
            },
            _ => panic!("expected unary expression"),
        }
    }

    #[test]
    fn test_parse_assignment() {
        let program = parse("x = 10");
        match extract_global_expr(&program, 0) {
            Expr::Assign { name, .. } => assert_eq!(name, "x"),
            _ => panic!("expected assignment"),
        }
    }

    #[test]
    fn test_parse_borrow_expression() {
        let program = parse("&mut x");
        match extract_global_expr(&program, 0) {
            Expr::Borrow { name, mutable, .. } => {
                assert_eq!(name, "x");
                assert!(*mutable);
            }
            _ => panic!("expected borrow expression"),
        }
    }

    #[test]
    fn test_parse_let_declaration() {
        let program = parse("let x: i32 = 42");
        match extract_global_expr(&program, 0) {
            Expr::Let { name, typ, is_const, .. } => {
                assert_eq!(name, "x");
                assert_eq!(typ, &Some("i32".to_string()));
                assert!(!is_const);
            }
            _ => panic!("expected let declaration"),
        }
    }

    #[test]
    fn test_parse_const_declaration() {
        let program = parse("const y = 100");
        match extract_global_expr(&program, 0) {
            Expr::Let { name, is_const, .. } => {
                assert_eq!(name, "y");
                assert!(is_const);
            }
            _ => panic!("expected const declaration"),
        }
    }

    #[test]
    fn test_parse_function_call() {
        let program = parse("print(42)");
        match extract_global_expr(&program, 0) {
            Expr::Call { func, args } => {
                assert_eq!(func, "print");
                assert_eq!(args.len(), 1);
            }
            _ => panic!("expected function call"),
        }
    }

    #[test]
    fn test_parse_import_statement() {
        let program = parse("import utils.math");
        match extract_global_expr(&program, 0) {
            Expr::Import(spec, _) => assert_eq!(spec, "utils/math"),
            _ => panic!("expected import statement"),
        }
    }

    #[test]
    fn test_parse_function_definition() {
        let program = parse("fn add(x, y):\n    return x + y");
        assert_eq!(program.functions.len(), 1);
        let func = &program.functions[0];
        assert_eq!(func.name, "add");
        assert_eq!(func.params, vec!["x".to_string(), "y".to_string()]);
        assert_eq!(func.body.len(), 1);
    }

    #[test]
    fn test_parse_if_expression() {
        let program = parse("if x == 1:\n    return 1\nelse:\n    return 0");
        match extract_global_expr(&program, 0) {
            Expr::If { condition: _, then_branch, else_branch } => {
                assert!(then_branch.stmts.len() > 0);
                assert!(else_branch.is_some());
            }
            _ => panic!("expected if expression"),
        }
    }

    #[test]
    fn test_parse_nested_if() {
        let program = parse("if x:\n    if y:\n        return 1\n    else:\n        return 0");
        match extract_global_expr(&program, 0) {
            Expr::If { then_branch, .. } => {
                match &then_branch.stmts[0] {
                    Expr::If { .. } => assert!(true),
                    _ => panic!("expected nested if"),
                }
            }
            _ => panic!("expected if expression"),
        }
    }

    #[test]
    fn test_parse_return_statement() {
        let program = parse("return 42");
        match extract_global_expr(&program, 0) {
            Expr::Return(expr, _) => {
                match &**expr {
                    Expr::Literal(Literal::Int(n), _) => assert_eq!(*n, 42),
                    _ => panic!("expected integer literal in return"),
                }
            }
            _ => panic!("expected return statement"),
        }
    }

    #[test]
    fn test_parse_string_literal() {
        let program = parse(r#""hello""#);
        match extract_global_expr(&program, 0) {
            Expr::StringLiteral(s, _) => assert_eq!(s, "hello"),
            _ => panic!("expected string literal"),
        }
    }

    #[test]
    fn test_parse_multiline_string() {
        let program = parse(r#""""line1
line2""""#);
        match extract_global_expr(&program, 0) {
            Expr::MultilineString(s, _) => assert_eq!(s, "line1\nline2"),
            _ => panic!("expected multiline string"),
        }
    }

    #[test]
    fn test_parse_fstring() {
        let program = parse(r#"f"Hello {name}"#);
        match extract_global_expr(&program, 0) {
            Expr::FString(elements, _) => {
                assert_eq!(elements.len(), 2);
                match &elements[0] {
                    Expr::StringLiteral(s, _) => assert_eq!(s, "Hello "),
                    _ => panic!("expected string literal element"),
                }
                match &elements[1] {
                    Expr::Identifier(n, _) => assert_eq!(n, "name"),
                    _ => panic!("expected identifier element"),
                }
            }
            _ => panic!("expected fstring"),
        }
    }

    #[test]
    fn test_parse_complex_expression() {
        let program = parse("a + b * c - d / 2");
        // Just checking it parses without panic
        assert_eq!(program.globals.len(), 1);
    }

    #[test]
    fn test_parse_parenthesized_expression() {
        let program = parse("(a + b) * c");
        assert_eq!(program.globals.len(), 1);
    }

    #[test]
    fn test_parse_comparison_operators() {
        let program = parse("a == b");
        match extract_global_expr(&program, 0) {
            Expr::Binary(_, op, _, _) => match op {
                BinOp::Eq => assert!(true),
                _ => panic!("expected Eq operator"),
            },
            _ => panic!("expected binary expression"),
        }
    }

    #[test]
    fn test_parse_power_is_right_associative() {
        let program = parse("2 ** 3 ** 2");
        match extract_global_expr(&program, 0) {
            Expr::Binary(_, BinOp::Pow, rhs, _) => match &**rhs {
                Expr::Binary(_, BinOp::Pow, _, _) => assert!(true),
                _ => panic!("expected right-associative power"),
            },
            _ => panic!("expected power expression"),
        }
    }

    #[test]
    fn test_parse_chained_comparison() {
        let program = parse("a < b < c");
        match extract_global_expr(&program, 0) {
            Expr::Binary(left, BinOp::Lt, _, _) => match &**left {
                Expr::Binary(_, BinOp::Lt, _, _) => assert!(true),
                _ => panic!("expected chained comparison to nest left"),
            },
            _ => panic!("expected comparison expression"),
        }
    }

    #[test]
    fn test_parse_mixed_numeric_precedence() {
        let program = parse("1 + 2.5 * 3");
        match extract_global_expr(&program, 0) {
            Expr::Binary(_, BinOp::Add, rhs, _) => match &**rhs {
                Expr::Binary(_, BinOp::Mul, _, _) => assert!(true),
                _ => panic!("expected multiplication to bind tighter than addition"),
            },
            _ => panic!("expected addition expression"),
        }
    }

    #[test]
    fn test_parse_implicit_multiplication_edge_cases() {
        let program = parse("2x + 3(y + 1)");
        match extract_global_expr(&program, 0) {
            Expr::Binary(left, BinOp::Add, right, _) => {
                match &**left {
                    Expr::Binary(_, BinOp::Mul, _, _) => assert!(true),
                    _ => panic!("expected implicit multiplication on left"),
                }
                match &**right {
                    Expr::Binary(_, BinOp::Mul, _, _) => assert!(true),
                    _ => panic!("expected implicit multiplication on right"),
                }
            }
            _ => panic!("expected addition root"),
        }
    }

    #[test]
    fn test_parse_with_whitespace() {
        let program = parse("  let   x   =   42  ");
        match extract_global_expr(&program, 0) {
            Expr::Let { name, .. } => assert_eq!(name, "x"),
            _ => panic!("expected let declaration"),
        }
    }

    #[test]
    fn test_parse_with_comments() {
        let program = parse("x // comment\ny");
        assert_eq!(program.globals.len(), 1);
    }

    #[test]
    fn test_parse_invalid_symbol_reports_location() {
        let mut lexer = Lexer::new("@");
        let mut parser = Parser::new(&mut lexer);
        let err = parser.parse_program("debug".to_string(), "test".to_string()).unwrap_err();
        assert!(err.to_string().contains("unknown symbol '@'"));
        assert!(err.to_string().contains("line=1"));
    }

    #[test]
    fn test_parse_empty_program() {
        let program = parse("");
        assert_eq!(program.globals.len(), 0);
        assert_eq!(program.functions.len(), 0);
    }
}
