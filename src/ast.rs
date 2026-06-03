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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceSpan {
    pub line: usize,
    pub column: usize,
}

impl SourceSpan {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }

    pub fn unknown() -> Self {
        Self { line: 0, column: 0 }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Import(String, SourceSpan),
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
    Tuple(Vec<Expr>, SourceSpan),
    TupleAccess {
        target: Box<Expr>,
        index: usize,
    },
    EnumLiteral {
        enum_name: String,
        variant_name: String,
        args: Vec<Expr>,
    },
    StringLiteral(String, SourceSpan),
    Identifier(String, SourceSpan),
    Borrow {
        name: String,
        mutable: bool,
        span: SourceSpan,
    },
    FString(Vec<Expr>, SourceSpan),
    MultilineString(String, SourceSpan),
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
    Return(Box<Expr>, SourceSpan),
    Literal(Literal, SourceSpan),
    Binary(Box<Expr>, BinOp, Box<Expr>, SourceSpan),
    Unary(UnaryOp, Box<Expr>, SourceSpan),
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

impl Expr {
    pub fn span(&self) -> SourceSpan {
        match self {
            Expr::Import(_, span)
            | Expr::Tuple(_, span)
            | Expr::StringLiteral(_, span)
            | Expr::Identifier(_, span)
            | Expr::Borrow { span, .. }
            | Expr::FString(_, span)
            | Expr::MultilineString(_, span)
            | Expr::Return(_, span)
            | Expr::Literal(_, span)
            | Expr::Binary(_, _, _, span)
            | Expr::Unary(_, _, span) => *span,
            Expr::List(items) => items.first().map_or(SourceSpan::unknown(), |e| e.span()),
            Expr::Index { target, .. } => target.span(),
            Expr::AssignIndex { index, .. } => index.span(),
            Expr::FieldAccess { target, .. } => target.span(),
            Expr::FieldAssign { target, .. } => target.span(),
            Expr::StructLiteral { fields, .. } => fields.first().map_or(SourceSpan::unknown(), |(_, e)| e.span()),
            Expr::TupleAccess { target, .. } => target.span(),
            Expr::EnumLiteral { args, .. } => args.first().map_or(SourceSpan::unknown(), |e| e.span()),
            Expr::Call { args, .. } => args.first().map_or(SourceSpan::unknown(), |e| e.span()),
            Expr::Let { value, .. } => value.span(),
            Expr::Assign { value, .. } => value.span(),
            Expr::If { condition, .. } => condition.span(),
            Expr::While { condition, .. } => condition.span(),
            Expr::For { iterable, .. } => iterable.span(),
            Expr::ForRange { start, .. } => start.span(),
            Expr::Match { expr, .. } => expr.span(),
            Expr::Break | Expr::Continue => SourceSpan::unknown(),
        }
    }
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
