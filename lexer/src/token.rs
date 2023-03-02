use crate::token::TokenType::*;
use logos::Logos;

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub value: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, value: &'a str) -> Self {
        Self { token_type, value }
    }
}

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    #[token("var")]
    Var,
    #[token("val")]
    Val,

    #[regex("[^;:<>|&\\s'\"$\\\\)(*+-/=;:}{,@}]+")]
    Identifier,

    #[regex("-?[0-9]+", priority = 2)]
    IntLiteral,
    #[regex("-?[0-9]+\\.[0-9]+")]
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

    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token("=")]
    Equal,
    #[token("'")]
    Quote,
    #[token("\"")]
    DoubleQuote,
    #[token("$")]
    Dollar,
    #[token("&")]
    Ampersand,
    #[token("@")]
    At,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,

    #[token("|")]
    Pipe,

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
    Star,
    #[token("/")]
    Slash,
    #[token("\\")]
    BackSlash,
    #[token("%")]
    Percent,

    #[token("[")]
    SquareLeftBracket,
    #[token("]")]
    SquaredRightBracket,
    #[token("(")]
    RoundedLeftBracket,
    #[token(")")]
    RoundedRightBracket,
    #[token("{")]
    CurlyLeftBracket,
    #[token("}")]
    CurlyRightBracket,

    #[regex(r"[ \t\f]+")]
    Space,

    #[regex("//.*", logos::skip)]
    #[error]
    Error,

    EndOfFile,
}

impl TokenType {
    pub fn is_bin_operator(self) -> bool {
        match self {
            And | Or | Plus | Minus | Star | EqualEqual | NotEqual | Less | LessEqual | Greater
            | GreaterEqual => true,
            _ => false,
        }
    }

    pub fn is_identifier_bound(self) -> bool {
        match self {
            NewLine | SemiColon | Less | Pipe | Greater | And | Or => true,
            _ => false,
        }
    }

    pub fn is_ponctuation(self) -> bool {
        matches!(
            self,
            TokenType::Ampersand
                | TokenType::Less
                | TokenType::Greater
                | TokenType::And
                | TokenType::Or
                | TokenType::Pipe
                | TokenType::SquareLeftBracket
                | TokenType::SquaredRightBracket
                | TokenType::RoundedLeftBracket
                | TokenType::RoundedRightBracket
                | TokenType::CurlyLeftBracket
                | TokenType::CurlyRightBracket
                | TokenType::Space
                | TokenType::SemiColon
                | TokenType::NewLine
                | TokenType::Error
        )
    }

    pub fn is_closing_ponctuation(self) -> bool {
        matches!(
            self,
            |TokenType::SquaredRightBracket| TokenType::RoundedRightBracket
                | TokenType::CurlyRightBracket
        )
    }

    /// Get a exact string representation of the token type
    ///
    /// Returns `None` if the token type is not a keyword.
    pub fn represent(self) -> Option<&'static str> {
        match self {
            Var => Some("var"),
            Val => Some("val"),
            Fun => Some("fun"),
            Use => Some("use"),
            If => Some("if"),
            Then => Some("then"),
            Else => Some("else"),
            For => Some("for"),
            In => Some("in"),
            While => Some("while"),
            Match => Some("match"),
            Arrow => Some("->"),
            FatArrow => Some("=>"),
            Colon => Some(":"),
            SemiColon => Some(";"),
            Equal => Some("="),
            Quote => Some("'"),
            DoubleQuote => Some("\""),
            Dollar => Some("$"),
            Ampersand => Some("&"),
            At => Some("@"),
            Comma => Some(","),
            Dot => Some("."),
            Pipe => Some("|"),
            And => Some("&&"),
            Or => Some("||"),
            Not => Some("!"),
            EqualEqual => Some("=="),
            NotEqual => Some("!="),
            Less => Some("<"),
            LessEqual => Some("<="),
            Greater => Some(">"),
            GreaterEqual => Some(">="),
            PlusEqual => Some("+="),
            MinusEqual => Some("-="),
            TimesEqual => Some("*="),
            DivideEqual => Some("/="),
            ModuloEqual => Some("%="),
            Plus => Some("+"),
            Minus => Some("-"),
            Star => Some("*"),
            Slash => Some("/"),
            BackSlash => Some("\\"),
            Percent => Some("%"),
            SquareLeftBracket => Some("["),
            SquaredRightBracket => Some("]"),
            RoundedLeftBracket => Some("("),
            RoundedRightBracket => Some(")"),
            CurlyLeftBracket => Some("{"),
            CurlyRightBracket => Some("}"),
            _ => None,
        }
    }
}
