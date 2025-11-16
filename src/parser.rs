use crate::ast::*;
use crate::lexer::{Lexer, TokenKind};

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

    fn eat(&mut self, expected: TokenKind) {
        if self.current == Some(expected.clone()) {
            self.advance();
        } else if self.current == Some(TokenKind::Newline) {
            self.advance();
            self.eat(expected);
        } else {
            panic!(
                "expected {:?}, but given {:?} (line={}, col={})",
                expected,
                self.current,
                self.lexer.line,
                self.lexer.column
            );
        }
    }

    fn get_precedence(tok: &Option<TokenKind>) -> i32 {
        match tok {
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

    fn parse_primary(&mut self) -> Expr {
        match self.current.clone() {
            Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
                let op = self.current.clone();
                self.advance();
                let expr = self.parse_primary();
                let unop = match op {
                    Some(TokenKind::Plus) => UnaryOp::Pos,
                    Some(TokenKind::Minus) => UnaryOp::Neg,
                    _ => unreachable!(),
                };
                Expr::Unary(unop, Box::new(expr))
            }
            Some(TokenKind::IntLiteral(n)) => {
                self.advance();
                Expr::Literal(Literal::Int(n))
            }
            Some(TokenKind::Identifier(name)) => {
                self.advance();
                Expr::Identifier(name)
            }
            Some(TokenKind::LParen) => {
                self.advance();
                let expr = self.parse_expr();
                self.eat(TokenKind::RParen);
                expr
            }
            Some(TokenKind::StringLiteral(s)) => {
                self.advance();
                Expr::StringLiteral(s)
            }
            Some(TokenKind::MultilineString(s)) => {
                self.advance();
                Expr::MultilineString(s)
            }
            Some(TokenKind::FString(s)) => {
                self.advance();
                self.parse_fstring(&s)
            }
            _ => panic!("expected primary expression, found {:?}", self.current),
        }
    }

    fn parse_if_expr(&mut self) -> Expr {
        self.eat(TokenKind::If);
        let condition = Box::new(self.parse_expr());
        self.eat(TokenKind::Colon);

        let then_branch = self.parse_block().expect("error in the then-block if");

        let else_branch = if self.current == Some(TokenKind::Else) {
            self.advance();
            self.eat(TokenKind::Colon);

            if self.current == Some(TokenKind::If) {
                let nested_if = self.parse_if_expr();
                Some(Box::new(Block { stmts: vec![nested_if] }))
            } else {
                let else_block = self.parse_block().expect("error in else-block");
                Some(Box::new(else_block))
            }
        } else {
            None
        };

        Expr::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        }
    }

    pub fn parse_program(&mut self, profile: String, name: String) -> Program {
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
                Some(TokenKind::Fn) => functions.push(self.parse_function()),
                Some(TokenKind::Let)
                | Some(TokenKind::Const)
                | Some(TokenKind::Identifier(_))
                | Some(TokenKind::IntLiteral(_))
                | Some(TokenKind::LParen)
                | Some(TokenKind::Plus)
                | Some(TokenKind::Minus)
                | Some(TokenKind::StringLiteral(_))
                | Some(TokenKind::MultilineString(_))
                | Some(TokenKind::If)
                | Some(TokenKind::EqualEqual)
                | Some(TokenKind::FString(_)) => {
                    globals.push(self.parse_expr());
                }
                Some(tok) => {
                    panic!(
                        "expected define of the function or expression, but found {:?} (line={}, col={})",
                        tok,
                        self.lexer.line,
                        self.lexer.column
                    );
                }
                None => break,
            }
        }

        Program {
            globals,
            functions,
            profile,
            name,
        }
    }

    fn parse_block(&mut self) -> Result<Block, String> {
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

            let expr = self.parse_expr();
            stmts.push(expr);
        }

        Ok(Block { stmts })
    }

    fn parse_function(&mut self) -> FunctionDef {
        self.eat(TokenKind::Fn);

        let name = if let Some(TokenKind::Identifier(n)) = self.current.clone() {
            self.advance();
            n
        } else {
            panic!("expected name of the function");
        };

        self.eat(TokenKind::LParen);
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
        self.eat(TokenKind::RParen);
        self.eat(TokenKind::Colon);

        let block = self.parse_block().expect(&format!("error when parsing body of a function {}", name));

        FunctionDef {
            name,
            params,
            body: block.stmts,
        }
    }

    fn parse_binary(&mut self, mut left: Expr, min_prec: i32) -> Expr {
        loop {
            let op_opt = self.current.clone();
            let prec = Self::get_precedence(&op_opt);
            if prec < min_prec {
                break;
            }

            let is_implicit = matches!(
                op_opt,
                Some(TokenKind::Identifier(_))
                    | Some(TokenKind::IntLiteral(_))
                    | Some(TokenKind::LParen)
            );

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
                    _ => panic!("unknown operator: {:?}", op_opt),
                }
            };

            let mut right = self.parse_primary();

            loop {
                let next_prec = Self::get_precedence(&self.current);
                if next_prec > prec {
                    right = self.parse_binary(right, prec + 1);
                } else {
                    break;
                }
            }

            left = Expr::Binary(Box::new(left), binop, Box::new(right));
        }
        left
    }

    fn parse_expr(&mut self) -> Expr {
        while self.current == Some(TokenKind::Newline) {
            self.advance();
        }

        match self.current.clone() {
            Some(TokenKind::If) => self.parse_if_expr(),
            Some(TokenKind::Else) => {
                panic!(
                    "syntax error: 'else' without 'if' (line={}, col={})",
                    self.lexer.line,
                    self.lexer.column
                );
            }
            Some(TokenKind::Const) | Some(TokenKind::Let) => {
                let is_const = self.current == Some(TokenKind::Const);
                self.advance();
                let name = if let Some(TokenKind::Identifier(n)) = self.current.clone() {
                    self.advance();
                    n
                } else {
                    panic!("expected name of the variable");
                };

                let typ = if self.current == Some(TokenKind::Colon) {
                    self.advance();
                    if let Some(TokenKind::Identifier(t)) = self.current.clone() {
                        self.advance();
                        Some(t)
                    } else {
                        panic!("expected type after :");
                    }
                } else {
                    None
                };

                self.eat(TokenKind::Equal);
                let value = Box::new(self.parse_expr());
                Expr::Let {
                    name,
                    typ,
                    value,
                    is_const,
                }
            }
            Some(TokenKind::Return) => {
                self.advance();
                if self.current == Some(TokenKind::Newline) || self.current == Some(TokenKind::Eof) {
                    Expr::Return(Box::new(Expr::Literal(Literal::Int(0))))
                } else {
                    let value = Box::new(self.parse_expr());
                    Expr::Return(value)
                }
            }
            Some(tok) => {
                let mut left = self.parse_primary();

                if let Expr::Identifier(name) = &left {
                    match self.current.clone() {
                        Some(TokenKind::Equal) => {
                            self.advance();
                            let value = Box::new(self.parse_expr());
                            left = Expr::Assign {
                                name: name.clone(),
                                value,
                            };
                        }
                        Some(TokenKind::LParen) => {
                            self.advance();
                            let mut args = Vec::new();
                            while self.current != Some(TokenKind::RParen) {
                                args.push(self.parse_expr());
                                if self.current == Some(TokenKind::Comma) {
                                    self.advance();
                                } else {
                                    break;
                                }
                            }
                            self.eat(TokenKind::RParen);
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
            _ => panic!("unexpected token in expression: {:?}", self.current),
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