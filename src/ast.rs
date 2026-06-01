#[derive(Debug, Clone)]
pub enum Literal {
    Int(i32),
    Float(f64),
    Bool(bool),
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
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Import(String),
    List(Vec<Expr>),
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
    AssignIndex {
        name: String,
        index: Box<Expr>,
        value: Box<Expr>,
    },
    FieldAccess {
        target: Box<Expr>,
        field: String,
    },
    FieldAssign {
        target: Box<Expr>,
        field: String,
        value: Box<Expr>,
    },
    StructLiteral {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    Tuple(Vec<Expr>),
    TupleAccess {
        target: Box<Expr>,
        index: usize,
    },
    EnumLiteral {
        enum_name: String,
        variant_name: String,
        args: Vec<Expr>,
    },
    StringLiteral(String),
    Identifier(String),
    Borrow {
        name: String,
        mutable: bool,
    },
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
    While {
        condition: Box<Expr>,
        body: Box<Block>,
    },
    For {
        variable: String,
        iterable: Box<Expr>,
        body: Box<Block>,
    },
    ForRange {
        variable: String,
        start: Box<Expr>,
        end: Box<Expr>,
        body: Box<Block>,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum MatchPattern {
    Int(i32),
    Variant {
        name: String,
        bindings: Vec<String>,
    },
    Wildcard,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<String>,
    pub param_types: Vec<Option<String>>,
    pub return_type: Option<String>,
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub typ: String,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub name: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct ImplDef {
    pub struct_name: String,
    pub methods: Vec<FunctionDef>,
}

#[derive(Debug, Clone)]
pub struct Program {
    // compiler needed
    pub globals: Vec<Expr>,
    pub functions: Vec<FunctionDef>,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
    pub type_aliases: Vec<TypeAlias>,
    pub impls: Vec<ImplDef>,

    // compiler additional
    pub profile: String,
    pub name: String,
}
