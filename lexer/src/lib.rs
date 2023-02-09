use logos::{Lexer, Logos};


mod tests;

#[derive(Debug, PartialEq)]
pub struct LexError {
    pub message: String,
    pub offset: usize,
}


#[derive(Logos, Debug, PartialEq)]
pub enum Token<'a> {
    #[token("var")]
    Var,
    #[token("val")]
    Val,

    #[regex("\".*\"|'.*'|[^0-9\\s][a-zA-Z0-9_]*")]
    Identifier(&'a str),

    #[regex("[+-]?[0-9]+", priority = 2)]
    IntLiteral(&'a str),
    #[regex("[+-]?[0-9]+\\.[0-9]+")]
    FloatLiteral(&'a str),

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
    #[regex("[0-2&]>>", |lex| lex.slice().chars().next())]
    AppendRedirect(char),
    #[regex("[0-2&]>", |lex| lex.slice().chars().next())]
    Redirect(char),
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

    #[regex(r"[ \t\f]+", logos::skip)]
    #[error]
    Error,
}




