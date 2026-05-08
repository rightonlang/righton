#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Fn,
    Let,
    Const,
    Equal,
    Identifier(String),
    StringLiteral(String),
    FString(String),
    MultilineString(String),
    Colon,
    LParen,
    RParen,
    Newline,
    Eof,
    IntLiteral(i32),
    FloatLiteral(f64),
    BoolLiteral(bool),
    Return,
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %
    StarStar,
    Else,
    If,
    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Comma,
    Dot,
    Ampersand,
    And,
    Or,
    Not,
    Mut,
    Import,
    Invalid(char),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Lexer {
    src: Vec<char>,
    pos: usize,
    pub(crate) line: usize,
    pub(crate) column: usize,
    pub current_line_indent: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            src: input.chars().collect(),
            pos: 0,
            line: 1,
            column: 0,
            current_line_indent: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.src.get(self.pos).cloned()
    }

    fn next(&mut self) -> Option<char> {
        let c = self.src.get(self.pos).cloned();
        if let Some(ch) = c {
            self.pos += 1;
            if ch == '\n' {
                self.line += 1;
                self.column = 0;
                self.current_line_indent = 0;
            } else {
                self.column += 1;
            }
        }
        c
    }

    fn skip_unnecessary(&mut self) {
        loop {
            match self.peek() {
                Some(' ') => {
                    if self.column == self.current_line_indent {
                        self.current_line_indent += 1;
                    }
                    self.next();
                }
                Some('\t') => {
                    if self.column == self.current_line_indent {
                        self.current_line_indent += 4;
                    }
                    self.next();
                }
                Some('/') => {
                    if self.src.get(self.pos + 1) == Some(&'/') {
                        self.next();
                        self.next();
                        while let Some(c) = self.peek() {
                            self.next();
                            if c == '\n' {
                                break;
                            }
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_unnecessary();
        let start_col = self.column;
        let ch = match self.next() {
            Some(c) => c,
            None => {
                return Token {
                    kind: TokenKind::Eof,
                    line: self.line,
                    column: start_col,
                };
            }
        };

        match ch {
            ':' => Token {
                kind: TokenKind::Colon,
                line: self.line,
                column: start_col,
            },
            ',' => Token {
                kind: TokenKind::Comma,
                line: self.line,
                column: start_col,
            },
            '.' => Token {
                kind: TokenKind::Dot,
                line: self.line,
                column: start_col,
            },
            '&' => Token {
                kind: TokenKind::Ampersand,
                line: self.line,
                column: start_col,
            },
            '(' => Token {
                kind: TokenKind::LParen,
                line: self.line,
                column: start_col,
            },
            ')' => Token {
                kind: TokenKind::RParen,
                line: self.line,
                column: start_col,
            },
            '\n' => Token {
                kind: TokenKind::Newline,
                line: self.line,
                column: self.column,
            },
            '"' => {
                if self.peek() == Some('"') {
                    self.next();
                    if self.peek() == Some('"') {
                        self.next();
                        let mut s = String::new();
                        while let Some(_) = self.peek() {
                            if self.peek() == Some('"')
                                && self.src.get(self.pos + 1) == Some(&'"')
                                && self.src.get(self.pos + 2) == Some(&'"')
                            {
                                self.next();
                                self.next();
                                self.next();
                                break;
                            }
                            let c = self.next().unwrap();
                            s.push(c);
                        }
                        return Token {
                            kind: TokenKind::MultilineString(s),
                            line: self.line,
                            column: start_col,
                        };
                    } else {
                    }
                }

                let mut s = String::new();
                while let Some(c) = self.peek() {
                    self.next();
                    if c == '"' {
                        break;
                    }
                    s.push(c);
                }
                Token {
                    kind: TokenKind::StringLiteral(s),
                    line: self.line,
                    column: start_col,
                }
            }

            '+' => Token {
                kind: TokenKind::Plus,
                line: self.line,
                column: start_col,
            },
            '-' => Token {
                kind: TokenKind::Minus,
                line: self.line,
                column: start_col,
            },
            '*' => {
                if self.peek() == Some('*') {
                    self.next();
                    Token {
                        kind: TokenKind::StarStar,
                        line: self.line,
                        column: start_col,
                    }
                } else {
                    Token {
                        kind: TokenKind::Star,
                        line: self.line,
                        column: start_col,
                    }
                }
            }
            '/' => Token {
                kind: TokenKind::Slash,
                line: self.line,
                column: start_col,
            },
            '%' => Token {
                kind: TokenKind::Percent,
                line: self.line,
                column: start_col,
            },

            //strs
            'f' => {
                if self.peek() == Some('"') {
                    self.next();
                    if self.peek() == Some('"') && self.src.get(self.pos + 1) == Some(&'"') {
                        self.next();
                        self.next();
                        let mut s = String::new();
                        while let Some(_) = self.peek() {
                            if self.peek() == Some('"')
                                && self.src.get(self.pos + 1) == Some(&'"')
                                && self.src.get(self.pos + 2) == Some(&'"')
                            {
                                self.next();
                                self.next();
                                self.next();
                                break;
                            }
                            let c = self.next().unwrap();
                            s.push(c);
                        }
                        Token {
                            kind: TokenKind::FString(s),
                            line: self.line,
                            column: start_col,
                        }
                    } else {
                        let mut s = String::new();
                        while let Some(c) = self.peek() {
                            self.next();
                            if c == '"' {
                                break;
                            }
                            s.push(c);
                        }
                        Token {
                            kind: TokenKind::FString(s),
                            line: self.line,
                            column: start_col,
                        }
                    }
                } else {
                    let mut ident = String::new();
                    ident.push('f');
                    while let Some(ch) = self.peek() {
                        if ch.is_alphanumeric() || ch == '_' {
                            ident.push(ch);
                            self.next();
                        } else {
                            break;
                        }
                    }
                    let kind = if ident == "fn" {
                        TokenKind::Fn
                    } else if ident == "false" {
                        TokenKind::BoolLiteral(false)
                    } else {
                        TokenKind::Identifier(ident)
                    };
                    Token {
                        kind,
                        line: self.line,
                        column: start_col,
                    }
                }
            }
            '=' => {
                if self.peek() == Some('=') {
                    self.next();
                    return Token {
                        kind: TokenKind::EqualEqual,
                        line: self.line,
                        column: start_col,
                    };
                }
                Token {
                    kind: TokenKind::Equal,
                    line: self.line,
                    column: start_col,
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.next();
                    Token {
                        kind: TokenKind::NotEqual,
                        line: self.line,
                        column: start_col,
                    }
                } else {
                    Token {
                        kind: TokenKind::Invalid('!'),
                        line: self.line,
                        column: start_col,
                    }
                }
            }
            '<' => {
                if self.peek() == Some('=') {
                    self.next();
                    Token {
                        kind: TokenKind::LessEqual,
                        line: self.line,
                        column: start_col,
                    }
                } else {
                    Token {
                        kind: TokenKind::Less,
                        line: self.line,
                        column: start_col,
                    }
                }
            }
            '>' => {
                if self.peek() == Some('=') {
                    self.next();
                    Token {
                        kind: TokenKind::GreaterEqual,
                        line: self.line,
                        column: start_col,
                    }
                } else {
                    Token {
                        kind: TokenKind::Greater,
                        line: self.line,
                        column: start_col,
                    }
                }
            }

            c if c.is_alphabetic() || c == '_' => {
                let mut ident = String::new();
                ident.push(c);
                while let Some(ch) = self.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        ident.push(ch);
                        self.next();
                    } else {
                        break;
                    }
                }
                let kind = match ident.as_str() {
                    "fn" => TokenKind::Fn,
                    "let" => TokenKind::Let,
                    "const" => TokenKind::Const,
                    "return" => TokenKind::Return,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "true" => TokenKind::BoolLiteral(true),
                    "false" => TokenKind::BoolLiteral(false),
                    "and" => TokenKind::And,
                    "or" => TokenKind::Or,
                    "not" => TokenKind::Not,
                    "mut" => TokenKind::Mut,
                    "import" => TokenKind::Import,
                    _ => TokenKind::Identifier(ident),
                };
                Token {
                    kind,
                    line: self.line,
                    column: start_col,
                }
            }
            c if c.is_digit(10) => {
                let mut num = c.to_digit(10).unwrap() as f64;
                let mut is_float = false;
                while let Some(ch) = self.peek() {
                    if let Some(d) = ch.to_digit(10) {
                        num = num * 10.0 + d as f64;
                        self.next();
                    } else if ch == '.' {
                        is_float = true;
                        self.next();
                        let mut decimal = 0.0;
                        let mut decimal_digits = 0;
                        while let Some(ch) = self.peek() {
                            if let Some(d) = ch.to_digit(10) {
                                decimal = decimal * 10.0 + d as f64;
                                decimal_digits += 1;
                                self.next();
                            } else {
                                break;
                            }
                        }
                        num = num + decimal / 10.0_f64.powf(decimal_digits as f64);
                        break;
                    } else {
                        break;
                    }
                }
                Token {
                    kind: if is_float { TokenKind::FloatLiteral(num) } else { TokenKind::IntLiteral(num as i32) },
                    line: self.line,
                    column: start_col,
                }
            }
            _ => {
                Token {
                    kind: TokenKind::Invalid(ch),
                    line: self.line,
                    column: start_col,
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        tokens
    }

    #[test]
    fn test_simple_identifier() {
        let tokens = tokenize("x");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Identifier("x".to_string()));
    }

    #[test]
    fn test_keywords() {
        let tokens = tokenize("fn let const return if else mut import");
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Fn,
                TokenKind::Let,
                TokenKind::Const,
                TokenKind::Return,
                TokenKind::If,
                TokenKind::Else,
                TokenKind::Mut,
                TokenKind::Import,
            ]
        );
    }

    #[test]
    fn test_integer_literals() {
        let tokens = tokenize("42 0 123456");
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::IntLiteral(42),
                TokenKind::IntLiteral(0),
                TokenKind::IntLiteral(123456),
            ]
        );
    }

    #[test]
    fn test_operators() {
        let tokens = tokenize("+ - * / % ** == != < <= > >=");
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Percent,
                TokenKind::StarStar,
                TokenKind::EqualEqual,
                TokenKind::NotEqual,
                TokenKind::Less,
                TokenKind::LessEqual,
                TokenKind::Greater,
                TokenKind::GreaterEqual,
            ]
        );
    }

    #[test]
    fn test_delimiters() {
        let tokens = tokenize("( ) : , . &");
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::Colon,
                TokenKind::Comma,
                TokenKind::Dot,
                TokenKind::Ampersand,
            ]
        );
    }

    #[test]
    fn test_string_literals() {
        let tokens = tokenize(r#""hello" "world""#);
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::StringLiteral("hello".to_string()),
                TokenKind::StringLiteral("world".to_string()),
            ]
        );
    }

    #[test]
    fn test_multiline_string() {
        let tokens = tokenize(r#""""multi
line""""#);
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(kinds.len(), 1);
        match &kinds[0] {
            TokenKind::MultilineString(s) => {
                assert_eq!(s, "multi\nline");
            }
            _ => panic!("expected MultilineString"),
        }
    }

    #[test]
    fn test_fstring() {
        let tokens = tokenize(r#"f"hello {name}" "#);
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(kinds.len(), 1);
        match &kinds[0] {
            TokenKind::FString(s) => {
                assert_eq!(s, "hello {name}");
            }
            _ => panic!("expected FString"),
        }
    }

    #[test]
    fn test_newline_tokens() {
        let tokens = tokenize("x\ny\n");
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![TokenKind::Identifier("x".to_string()), TokenKind::Newline, TokenKind::Identifier("y".to_string()), TokenKind::Newline]);
    }

    #[test]
    fn test_comments() {
        let tokens = tokenize("x // this is a comment\ny");
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(kinds, vec![TokenKind::Identifier("x".to_string()), TokenKind::Identifier("y".to_string())]);
    }

    #[test]
    fn test_invalid_symbol() {
        let tokens = tokenize("@");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0].kind, TokenKind::Invalid('@')));
    }

    #[test]
    fn test_complex_expression() {
        let tokens = tokenize("a + b * (c - 2)");
        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Identifier("a".to_string()),
                TokenKind::Plus,
                TokenKind::Identifier("b".to_string()),
                TokenKind::Star,
                TokenKind::LParen,
                TokenKind::Identifier("c".to_string()),
                TokenKind::Minus,
                TokenKind::IntLiteral(2),
                TokenKind::RParen,
            ]
        );
    }

    #[test]
    fn test_line_and_column() {
        let tokens = tokenize("ab\ncd");
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[0].column, 0);
        assert_eq!(tokens[1].line, 2);
        assert_eq!(tokens[1].column, 0);
    }
}
