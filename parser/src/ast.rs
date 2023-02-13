use lexer::token::Token;

/// A expression that can be evaluated.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Assign(Assign<'a>),
    Binary(Binary<'a>),
    Call(Call<'a>),
    FunDeclaration(FunDeclaration<'a>),
    Literal(Literal<'a>),
    Grouping(Grouping<'a>),
    Substitution(Substitution<'a>),
    VarReference(VarReference<'a>),
    VarDeclaration(VarDeclaration<'a>),
}

/// A variable assignation.
#[derive(Debug, Clone, PartialEq)]
pub struct Assign<'a> {
    /// The identifier of the variable.
    pub name: Token<'a>,
    /// The value of the variable to be evaluated.
    pub value: Box<Expr<'a>>,
}

/// A binary operation between two expressions.
#[derive(Debug, Clone, PartialEq)]
pub struct Binary<'a> {
    /// The left-hand side of the operation.
    pub left: Box<Expr<'a>>,
    /// The operator of the operation.
    pub op: BinaryOperator,
    /// The right-hand side of the operation.
    pub right: Box<Expr<'a>>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinaryOperator {
    /// The `==` operator.
    EqualEqual,
    /// The `!=` operator.
    NotEqual,
    /// The `<` operator.
    Less,
    /// The `<=` operator.
    LessEqual,
    /// The `>` operator.
    Greater,
    /// The `>=` operator.
    GreaterEqual,
    /// The `+` operator.
    Plus,
    /// The `-` operator.
    Minus,
    /// The `*` operator.
    Star,
    /// The `/` operator.
    Slash,
    /// The `%` operator.
    Percent,
}

/// A call to a function or a command.
#[derive(Debug, Clone, PartialEq)]
pub struct Call<'a> {
    /// The name of the function or command.
    pub name: Token<'a>,
    /// The arguments of the function or command.
    pub arguments: Vec<Expr<'a>>,
}

/// A function declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct FunDeclaration<'a> {
    pub name: Token<'a>,
    pub parameters: Vec<TypedVariable<'a>>,
    pub body: Vec<Expr<'a>>,
}

/// A boxed expression that helps with precedence.
#[derive(Debug, Clone, PartialEq)]
pub struct Grouping<'a> {
    pub expr: Box<Expr<'a>>,
}

/// A literal value that can be used directly.
#[derive(Debug, Clone, PartialEq)]
pub struct Literal<'a> {
    pub token: Token<'a>,
    pub parsed: LiteralValue,
}

/// A literal value that can be used directly.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    String(String),
    Int(i64),
    Float(f64),
}

/// A special type of grouping expression that should be substituted
/// based on its expression and kind.
#[derive(Debug, Clone, PartialEq)]
pub struct Substitution<'a> {
    pub expr: Box<Expr<'a>>,
    pub kind: SubstitutionKind,
}

/// The kind of substitution that should be performed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SubstitutionKind {
    /// An arithmetic evaluation with `$((...))`.
    Arithmetic,
    /// A command standard output substitution with `$(...)`.
    Capture,
    /// A return value substitution with `@(...)`.
    Return,
}

/// A typed variable.
#[derive(Debug, Clone, PartialEq)]
pub struct TypedVariable<'a> {
    /// The name of the variable.
    pub name: Token<'a>,
    /// The type of the variable.
    pub ty: Option<Token<'a>>,
}

/// A variable declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclaration<'a> {
    /// The kind of the variable.
    pub kind: VarKind,
    /// The variable.
    pub var: TypedVariable<'a>,
    /// The value of the variable to be evaluated.
    pub initializer: Option<Box<Expr<'a>>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarKind {
    Var,
    Val,
}

/// A variable reference, prefixed with `$`.
#[derive(Debug, Clone, PartialEq)]
pub struct VarReference<'a> {
    /// The name of the variable.
    pub name: Token<'a>,
}
