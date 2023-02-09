use logos::Logos;

pub trait Token<'a> {
    fn get_type(self) -> TokenType<'a>;

    fn value(self) -> Option<&'a str>;
    fn line(self) -> usize;
    fn column(self) -> usize;


    fn new(&self, token_type: TokenType<'a>,
           value: Option<&'a str>,
           line: usize,
           column: usize) -> Self;
}

pub struct TokenStruct<'a> {
    token_type: TokenType<'a>,
    value: Option<&'a str>,
    line: usize,
    column: usize,
}

impl<'a> Token<'a> for TokenStruct<'a> {
    fn get_type(self) -> TokenType<'a> {
        self.token_type
    }

    fn value(self) -> Option<&'a str> {
        self.value
    }

    fn line(self) -> usize {
        self.line
    }

    fn column(self) -> usize {
        self.column
    }

    fn new(&self, token_type: TokenType<'a>,
           value: Option<&'a str>,
           line: usize,
           column: usize) -> Self {
        Self {
            token_type,
            value,
            line,
            column,
        }
    }
}

#[derive(Logos, Debug, PartialEq)]
pub enum TokenType<'a> {
    #[token("var")]
    Var,
    #[token("val")]
    Val,

    #[regex("\"([^\"\n]|\\.)*\"|'([^\"\n]|\\.)*'|[a-zA-Z_][a-zA-Z0-9_]*")]
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
    #[regex("[0-2&]>>", | lex | lex.slice().chars().next())]
    AppendRedirect(char),
    #[regex("[0-2&]>", | lex | lex.slice().chars().next())]
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

    #[regex(r"([ \t\f]+)|(//.*)", logos::skip)]
    #[error]
    Error,
}
