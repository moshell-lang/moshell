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

    #[regex("[^;:<>|&\\s'\\[\\]\"$\\\\)(*+-/=!;:}{,@}]+")]
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
    Bar,

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
    SquaredLeftBracket,
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
    ///is this lexeme a keyword of the language ?
    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            Fun | Use | If | Else | For | In | While | Match | Val | Var
        )
    }

    ///is this lexeme a valid reference name for a variable ?
    pub fn is_valid_var_ref_name(self) -> bool {
        matches!(
            self,
            Identifier | Dollar | Ampersand | At | Not | IntLiteral
        )
    }

    ///is this lexeme a binary operator ?
    pub fn is_bin_operator(self) -> bool {
        match self {
            And | Or | Plus | Minus | Star | EqualEqual | NotEqual | Less | LessEqual | Greater
            | GreaterEqual => true,
            _ => false,
        }
    }

    ///is this lexeme a lexeme that cannot fusion with other glued tokens
    pub fn is_identifier_bound(self) -> bool {
        match self {
            NewLine | SemiColon | Less | Bar | Greater | And | Or | FatArrow => true,
            _ => false,
        }
    }

    ///is this a lexeme that stops a call line
    pub fn is_call_bound(self) -> bool {
        matches!(
            self,
            TokenType::Ampersand
                | TokenType::And
                | TokenType::Or
                | TokenType::SquaredLeftBracket
                | TokenType::SquaredRightBracket
                | TokenType::RoundedLeftBracket
                | TokenType::RoundedRightBracket
                | TokenType::CurlyLeftBracket
                | TokenType::CurlyRightBracket
                | TokenType::SemiColon
                | TokenType::Error
                | TokenType::EndOfFile
        )
    }

    ///is this lexeme a punctuation
    pub fn is_ponctuation(self) -> bool {
        matches!(
            self,
            TokenType::Ampersand
                | TokenType::Less
                | TokenType::Greater
                | TokenType::And
                | TokenType::Or
                | TokenType::Bar
                | TokenType::Arrow
                | TokenType::FatArrow
                | TokenType::SquaredLeftBracket
                | TokenType::SquaredRightBracket
                | TokenType::RoundedLeftBracket
                | TokenType::RoundedRightBracket
                | TokenType::CurlyLeftBracket
                | TokenType::CurlyRightBracket
                | TokenType::SemiColon
                | TokenType::Error
        )
    }

    ///is this lexeme a closing punctuation
    pub fn is_closing_ponctuation(self) -> bool {
        matches!(
            self,
            |TokenType::SquaredRightBracket| TokenType::RoundedRightBracket
                | TokenType::CurlyRightBracket
        )
    }
}
