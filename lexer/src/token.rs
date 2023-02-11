use logos::Logos;

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub value: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, value: &'a str) -> Self {
        Self { token_type, value }
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum TokenType {
    #[token("var")]
    Var,
    #[token("val")]
    Val,

    #[regex("\"([^\"\n]|\\.)*\"|'([^\"\n]|\\.)*'|[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    #[regex("[+-]?[0-9]+", priority = 2)]
    IntLiteral,
    #[regex("[+-]?[0-9]+\\.[0-9]+")]
    FloatLiteral,

    #[token("\n")]
    NewLine,

    #[token("fun")]
    Fun,
    #[token("use")]
    Use,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("while")]
    While,
    #[token("match")]
    Match,

    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,

    #[token("int")]
    Int,
    #[token("float")]
    Float,
    #[token("exitcode")]
    Exitcode,
    #[token("bool")]
    Bool,
    #[token("any")]
    Any,

    #[token(":")]
    Colon,
    #[token("=")]
    Equal,
    #[token("'")]
    Quote,
    #[token("$")]
    Dollar,
    #[token("&")]
    Ampersand,
    #[token("@")]
    At,

    #[token("|")]
    Pipe,
    #[regex("[0-2&]>>")]
    AppendRedirect,
    #[regex("[0-2&]>")]
    Redirect,
    #[regex(">&2")]
    ErrorRedirect,

    #[token("<<<")]
    Here,

    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,

    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,

    #[token("+=")]
    PlusEqual,
    #[token("-=")]
    MinusEqual,
    #[token("*=")]
    TimesEqual,
    #[token("/=")]
    DivideEqual,
    #[token("%=")]
    ModuloEqual,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("/")]
    Divide,
    #[token("%")]
    Modulo,

    #[token("[")]
    SquareLeftBracket,
    #[token("]")]
    SquareRightBracket,
    #[token("(")]
    RoundedLeftBracket,
    #[token(")")]
    RoundedRightBracket,
    #[token("{")]
    CurlyLeftBracket,
    #[token("}")]
    CurlyRightBracket,

    #[regex(r"([ \t\f]+)|(//.*)", logos::skip)]
    #[error]
    Error,
}
