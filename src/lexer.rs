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
                column: start_col,
            },
            '"' => {
                if self.peek() == Some('"') {
                    self.next();
                    if self.peek() == Some('"') {
                        self.next();
                        let mut s = String::new();
                        while let Some(_) = self.peek() {
                            let c = self.next().unwrap();
                            if c == '"'
                                && self.peek() == Some('"')
                                && self.src.get(self.pos + 1) == Some(&'"')
                            {
                                self.next();
                                self.next();
                                break;
                            }
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
                            let c = self.next().unwrap();
                            if c == '"'
                                && self.peek() == Some('"')
                                && self.src.get(self.pos + 1) == Some(&'"')
                            {
                                self.next();
                                self.next();
                                break;
                            }
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
                    panic!("unknown symbol '!'");
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
                    _ => TokenKind::Identifier(ident),
                };
                Token {
                    kind,
                    line: self.line,
                    column: start_col,
                }
            }
            c if c.is_digit(10) => {
                let mut num = c.to_digit(10).unwrap() as i32;
                while let Some(ch) = self.peek() {
                    if let Some(d) = ch.to_digit(10) {
                        num = num * 10 + d as i32;
                        self.next();
                    } else {
                        break;
                    }
                }
                Token {
                    kind: TokenKind::IntLiteral(num),
                    line: self.line,
                    column: start_col,
                }
            }
            _ => {
                eprintln!(
                    "unknown symbol '{}' on {}:{}",
                    ch, self.line, self.column
                );
                self.next_token()
            }
        }
    }
}
