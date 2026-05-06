use crate::ast::*;
use crate::lexer::{Lexer, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
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
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        let first = lexer.next_token().kind;
        Self {
            lexer,
            current: Some(first),
        }
    }

    fn advance(&mut self) {
        let tok = self.lexer.next_token();
        self.current = Some(tok.kind);
    }

    fn eat(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        if self.current == Some(expected.clone()) {
            self.advance();
            Ok(())
        } else if self.current == Some(TokenKind::Newline) {
            self.advance();
            self.eat(expected)
        } else {
            Err(ParseError::new(format!(
                "expected {:?}, but found {:?} (line={}, col={})",
                expected,
                self.current,
                self.lexer.line,
                self.lexer.column
            )))
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
                let op = self.current.clone();
                self.advance();
                let expr = self.parse_primary()?;
                let unop = match op {
                    Some(TokenKind::Plus) => UnaryOp::Pos,
                    Some(TokenKind::Minus) => UnaryOp::Neg,
                    _ => unreachable!(),
                };
                Ok(Expr::Unary(unop, Box::new(expr)))
            }
            Some(TokenKind::Not) => {
                self.advance();
                let expr = self.parse_primary()?;
                Ok(Expr::Unary(UnaryOp::Not, Box::new(expr)))
            }
            Some(TokenKind::IntLiteral(n)) => {
                self.advance();
                Ok(Expr::Literal(Literal::Int(n)))
            }
            Some(TokenKind::FloatLiteral(n)) => {
                self.advance();
                Ok(Expr::Literal(Literal::Float(n)))
            }
            Some(TokenKind::BoolLiteral(b)) => {
                self.advance();
                Ok(Expr::Literal(Literal::Bool(b)))
            }
            Some(TokenKind::Identifier(name)) => {
                self.advance();
                Ok(Expr::Identifier(name))
            }
            Some(TokenKind::LParen) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.eat(TokenKind::RParen)?;
                Ok(expr)
            }
            Some(TokenKind::StringLiteral(s)) => {
                self.advance();
                Ok(Expr::StringLiteral(s))
            }
            Some(TokenKind::MultilineString(s)) => {
                self.advance();
                Ok(Expr::MultilineString(s))
            }
            Some(TokenKind::FString(s)) => {
                self.advance();
                Ok(self.parse_fstring(&s))
            }
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

        let else_branch = if self.current == Some(TokenKind::Else) {
            self.advance();
            self.eat(TokenKind::Colon)?;

            if self.current == Some(TokenKind::If) {
                let nested_if = self.parse_if_expr()?;
                Some(Box::new(Block { stmts: vec![nested_if] }))
            } else {
                let else_block = self.parse_block()?;
                Some(Box::new(else_block))
            }
        } else {
            None
        };

        Ok(Expr::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    pub fn parse_program(&mut self, profile: String, name: String) -> Result<Program, ParseError> {
        let mut functions = Vec::new();
        let mut globals = Vec::new();

        while self.current != Some(TokenKind::Eof) {
            while self.current == Some(TokenKind::Newline) {
                self.advance();
            }

            if self.current == Some(TokenKind::Eof) {
                break;
            }

            match self.current.clone() {
                Some(TokenKind::Fn) => functions.push(self.parse_function()?),
                Some(TokenKind::Let)
                | Some(TokenKind::Const)
                | Some(TokenKind::Identifier(_))
                | Some(TokenKind::IntLiteral(_))
                | Some(TokenKind::FloatLiteral(_))
                | Some(TokenKind::BoolLiteral(_))
                | Some(TokenKind::LParen)
                | Some(TokenKind::Plus)
                | Some(TokenKind::Minus)
                | Some(TokenKind::Not)
                | Some(TokenKind::StringLiteral(_))
                | Some(TokenKind::MultilineString(_))
                | Some(TokenKind::If)
                | Some(TokenKind::Return)
                | Some(TokenKind::EqualEqual)
                | Some(TokenKind::FString(_)) => {
                    globals.push(self.parse_expr()?);
                }
                Some(tok) => {
                    return Err(ParseError::new(format!(
                        "expected define of the function or expression, but found {:?} (line={}, col={})",
                        tok,
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
            Some(TokenKind::Else) | Some(TokenKind::Eof) | Some(TokenKind::Fn) => {
                return Ok(Block { stmts });
            }
            _ => {}
        }

        loop {
            while self.current == Some(TokenKind::Newline) {
                self.advance();
                match self.current {
                    Some(TokenKind::Else) | Some(TokenKind::Eof) | Some(TokenKind::Fn) => {
                        return Ok(Block { stmts });
                    }
                    _ => {}
                }
            }

            match self.current {
                Some(TokenKind::Else) | Some(TokenKind::Eof) | Some(TokenKind::Fn) => break,
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
        while let Some(TokenKind::Identifier(p)) = self.current.clone() {
            params.push(p);
            self.advance();
            if self.current == Some(TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        self.eat(TokenKind::RParen)?;
        self.eat(TokenKind::Colon)?;

        let block = self.parse_block()?;

        Ok(FunctionDef {
            name,
            params,
            body: block.stmts,
        })
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

            left = Expr::Binary(Box::new(left), binop, Box::new(right));
        }
        Ok(left)
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        while self.current == Some(TokenKind::Newline) {
            self.advance();
        }

        match self.current.clone() {
            Some(TokenKind::If) => self.parse_if_expr(),
            Some(TokenKind::Else) => {
                Err(ParseError::new(format!(
                    "syntax error: 'else' without 'if' (line={}, col={})",
                    self.lexer.line,
                    self.lexer.column
                )))
            }
            Some(TokenKind::Const) | Some(TokenKind::Let) => {
                let is_const = self.current == Some(TokenKind::Const);
                self.advance();
                let name = if let Some(TokenKind::Identifier(n)) = self.current.clone() {
                    self.advance();
                    n
                } else {
                    return Err(ParseError::new("expected name of the variable"));
                };

                let typ = if self.current == Some(TokenKind::Colon) {
                    self.advance();
                    if let Some(TokenKind::Identifier(t)) = self.current.clone() {
                        self.advance();
                        Some(t)
                    } else {
                        return Err(ParseError::new("expected type after :"));
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
                self.advance();
                if self.current == Some(TokenKind::Newline) || self.current == Some(TokenKind::Eof) {
                    Ok(Expr::Return(Box::new(Expr::Literal(Literal::Int(0)))))
                } else {
                    let value = Box::new(self.parse_expr()?);
                    Ok(Expr::Return(value))
                }
            }
            Some(tok) => {
                let mut left = self.parse_primary()?;

                if let Expr::Identifier(name) = &left {
                    match self.current.clone() {
                        Some(TokenKind::Equal) => {
                            self.advance();
                            let value = Box::new(self.parse_expr()?);
                            left = Expr::Assign {
                                name: name.clone(),
                                value,
                            };
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
                        _ => {}
                    }
                }

                self.parse_binary(left, 0)
            }
            _ => Err(ParseError::new(format!(
                "unexpected token in expression: {:?} (line={}, col={})",
                self.current, self.lexer.line, self.lexer.column
            ))),
        }
    }

    fn parse_fstring(&self, s: &str) -> Expr {
        let mut elements = Vec::new();
        let mut buf = String::new();
        let mut chars = s.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '{' {
                if !buf.is_empty() {
                    elements.push(Expr::StringLiteral(buf.clone()));
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
                elements.push(Expr::Identifier(var_name));
            } else {
                buf.push(c);
            }
        }

        if !buf.is_empty() {
            elements.push(Expr::StringLiteral(buf));
        }

        Expr::FString(elements)
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
            Expr::Literal(Literal::Int(n)) => assert_eq!(*n, 42),
            _ => panic!("expected integer literal"),
        }
    }

    #[test]
    fn test_parse_identifier() {
        let program = parse("x");
        match extract_global_expr(&program, 0) {
            Expr::Identifier(name) => assert_eq!(name, "x"),
            _ => panic!("expected identifier"),
        }
    }

    #[test]
    fn test_parse_binary_operations() {
        let program = parse("a + b * c");
        match extract_global_expr(&program, 0) {
            Expr::Binary(_, op, _) => match op {
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
            Expr::Unary(op, _) => match op {
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
            Expr::If { condition, then_branch, else_branch } => {
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
            Expr::Return(expr) => {
                match &**expr {
                    Expr::Literal(Literal::Int(n)) => assert_eq!(*n, 42),
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
            Expr::StringLiteral(s) => assert_eq!(s, "hello"),
            _ => panic!("expected string literal"),
        }
    }

    #[test]
    fn test_parse_multiline_string() {
        let program = parse(r#""""line1
line2""""#);
        match extract_global_expr(&program, 0) {
            Expr::MultilineString(s) => assert_eq!(s, "line1\nline2"),
            _ => panic!("expected multiline string"),
        }
    }

    #[test]
    fn test_parse_fstring() {
        let program = parse(r#"f"Hello {name}"#);
        match extract_global_expr(&program, 0) {
            Expr::FString(elements) => {
                assert_eq!(elements.len(), 2);
                match &elements[0] {
                    Expr::StringLiteral(s) => assert_eq!(s, "Hello "),
                    _ => panic!("expected string literal element"),
                }
                match &elements[1] {
                    Expr::Identifier(n) => assert_eq!(n, "name"),
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
            Expr::Binary(_, op, _) => match op {
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
            Expr::Binary(_, BinOp::Pow, rhs) => match &**rhs {
                Expr::Binary(_, BinOp::Pow, _) => assert!(true),
                _ => panic!("expected right-associative power"),
            },
            _ => panic!("expected power expression"),
        }
    }

    #[test]
    fn test_parse_chained_comparison() {
        let program = parse("a < b < c");
        match extract_global_expr(&program, 0) {
            Expr::Binary(left, BinOp::Lt, _) => match &**left {
                Expr::Binary(_, BinOp::Lt, _) => assert!(true),
                _ => panic!("expected chained comparison to nest left"),
            },
            _ => panic!("expected comparison expression"),
        }
    }

    #[test]
    fn test_parse_mixed_numeric_precedence() {
        let program = parse("1 + 2.5 * 3");
        match extract_global_expr(&program, 0) {
            Expr::Binary(_, BinOp::Add, rhs) => match &**rhs {
                Expr::Binary(_, BinOp::Mul, _) => assert!(true),
                _ => panic!("expected multiplication to bind tighter than addition"),
            },
            _ => panic!("expected addition expression"),
        }
    }

    #[test]
    fn test_parse_implicit_multiplication_edge_cases() {
        let program = parse("2x + 3(y + 1)");
        match extract_global_expr(&program, 0) {
            Expr::Binary(left, BinOp::Add, right) => {
                match &**left {
                    Expr::Binary(_, BinOp::Mul, _) => assert!(true),
                    _ => panic!("expected implicit multiplication on left"),
                }
                match &**right {
                    Expr::Binary(_, BinOp::Mul, _) => assert!(true),
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
    fn test_parse_empty_program() {
        let program = parse("");
        assert_eq!(program.globals.len(), 0);
        assert_eq!(program.functions.len(), 0);
    }
}
