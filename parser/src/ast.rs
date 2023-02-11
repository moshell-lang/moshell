use lexer::token::Token;

/// A expression that can be evaluated.
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct Assign<'a> {
    /// The identifier of the variable.
    pub name: Token<'a>,
    /// The value of the variable to be evaluated.
    pub value: Box<Expr<'a>>,
}

/// A binary operation between two expressions.
#[derive(Debug, Clone)]
pub struct Binary<'a> {
    /// The left-hand side of the operation.
    pub left: Box<Expr<'a>>,
    /// The operator of the operation.
    pub op: BinaryOperator,
    /// The right-hand side of the operation.
    pub right: Box<Expr<'a>>,
}

#[derive(Debug, Copy, Clone)]
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
#[derive(Debug, Clone)]
pub struct Call<'a> {
    /// The name of the function or command.
    pub name: Token<'a>,
    /// The arguments of the function or command.
    pub arguments: Vec<Expr<'a>>,
}

/// A function declaration.
#[derive(Debug, Clone)]
pub struct FunDeclaration<'a> {
    pub name: Token<'a>,
    pub parameters: Vec<TypedVariable<'a>>,
    pub body: Vec<Expr<'a>>,
}

/// A boxed expression that helps with precedence.
#[derive(Debug, Clone)]
pub struct Grouping<'a> {
    pub expr: Box<Expr<'a>>,
}

/// A literal value that can be used directly.
#[derive(Debug, Clone)]
pub struct Literal<'a> {
    pub value: Token<'a>,
}

/// A special type of grouping expression that should be substituted
/// based on its expression and kind.
#[derive(Debug, Clone)]
pub struct Substitution<'a> {
    pub expr: Box<Expr<'a>>,
    pub kind: SubstitutionKind,
}

/// The kind of substitution that should be performed.
#[derive(Debug, Clone, Copy)]
pub enum SubstitutionKind {
    /// An arithmetic evaluation with `$((...))`.
    Arithmetic,
    /// A command standard output substitution with `$(...)`.
    Capture,
    /// A return value substitution with `@(...)`.
    Return,
}

/// A typed variable.
#[derive(Debug, Clone)]
pub struct TypedVariable<'a> {
    /// The name of the variable.
    pub name: Token<'a>,
    /// The type of the variable.
    pub ty: Option<Token<'a>>,
}

/// A variable declaration.
#[derive(Debug, Clone)]
pub struct VarDeclaration<'a> {
    /// The kind of the variable.
    pub kind: VarKind,
    /// The variable.
    pub var: TypedVariable<'a>,
    /// The value of the variable to be evaluated.
    pub initializer: Option<Box<Expr<'a>>>,
}

#[derive(Debug, Clone, Copy)]
pub enum VarKind {
    Var,
    Val,
}

/// A variable reference, prefixed with `$`.
#[derive(Debug, Clone)]
pub struct VarReference<'a> {
    /// The name of the variable.
    pub name: Token<'a>,
}
