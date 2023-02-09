use lexer::Token;

/// A expression that can be evaluated.
#[derive(Debug, Clone)]
pub enum Expr {
    Assign(Assign),
    Binary(Binary),
    Call(Call),
    FunDeclaration(FunDeclaration),
    Literal(Literal),
    Grouping(Grouping),
    Substitution(Substitution),
    VarReference(VarReference),
    VarDeclaration(VarDeclaration),
}

/// A variable assignation.
#[derive(Debug, Clone)]
pub struct Assign {
    /// The identifier of the variable.
    pub name: Token,
    /// The value of the variable to be evaluated.
    pub value: Box<Expr>,
}

/// A binary operation between two expressions.
#[derive(Debug, Clone)]
pub struct Binary {
    /// The left-hand side of the operation.
    pub left: Box<Expr>,
    /// The operator of the operation.
    pub op: BinaryOperator,
    /// The right-hand side of the operation.
    pub right: Box<Expr>,
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
pub struct Call {
    /// The name of the function or command.
    pub name: Token,
    /// The arguments of the function or command.
    pub arguments: Vec<Expr>,
}

/// A function declaration.
#[derive(Debug, Clone)]
pub struct FunDeclaration {
    pub name: Token,
    pub parameters: Vec<TypedVariable>,
    pub body: Vec<Expr>,
}

/// A boxed expression that helps with precedence.
#[derive(Debug, Clone)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

/// A literal value that can be used directly.
#[derive(Debug, Clone)]
pub struct Literal {
    pub value: Token,
}

/// A special type of grouping expression that should be substituted
/// based on its expression and kind.
#[derive(Debug, Clone)]
pub struct Substitution {
    pub expr: Box<Expr>,
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
pub struct TypedVariable {
    /// The name of the variable.
    pub name: String,
    /// The type of the variable.
    pub ty: Option<Token>,
}

/// A variable declaration.
#[derive(Debug, Clone)]
pub struct VarDeclaration {
    /// The variable.
    pub var: TypedVariable,
    /// The value of the variable to be evaluated.
    pub initializer: Option<Box<Expr>>,
}

/// A variable reference, prefixed with `$`.
#[derive(Debug, Clone)]
pub struct VarReference {
    /// The name of the variable.
    pub name: Token,
}
