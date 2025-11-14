#[derive(Debug, Clone)]
pub enum Literal {
    Int(i32),
    Str(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    DivMod,
    Pow,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Pos,
    Neg,
}

#[derive(Debug, Clone)]
pub enum Expr {
    StringLiteral(String),
    Identifier(String),
    FString(Vec<Expr>),
    MultilineString(String),
    Call {
        func: String,
        args: Vec<Expr>,
    },
    Let {
        name: String,
        typ: Option<String>,
        value: Box<Expr>,
        is_const: bool,
    },
    Assign {
        name: String,
        value: Box<Expr>,
    },
    Return(Box<Expr>),
    Literal(Literal),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    If {
        condition: Box<Expr>,
        then_branch: Box<Block>,
        else_branch: Option<Box<Block>>,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Program {
    // compiler needed
    pub globals: Vec<Expr>,
    pub functions: Vec<FunctionDef>,

    // compiler additional
    pub profile: String,
    pub name: String
}
