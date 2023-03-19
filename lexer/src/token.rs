use crate::token::TokenType::*;
use dbg_pls::DebugPls;
use enum_assoc::Assoc;
use logos::Logos;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub value: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, value: &'a str) -> Self {
        Self { token_type, value }
    }
}

impl<'a> From<Token<'a>> for &'a str {
    fn from(token: Token<'a>) -> Self {
        token.value
    }
}

impl<'a> From<&Token<'a>> for &'a str {
    fn from(token: &Token<'a>) -> Self {
        token.value
    }
}

#[derive(Assoc, Logos, Debug, PartialEq, Clone, Copy, DebugPls)]
#[func(pub fn str(&self) -> Option<&'static str>)]
pub enum TokenType {
    #[token("var")]
    #[assoc(str = "var")]
    Var,
    #[token("val")]
    #[assoc(str = "val")]
    Val,

    #[regex("[^;:<>|&\\s'\\[\\]\"$\\\\)(*+-/=!;:}{,@}]+")]
    Identifier,

    #[regex("-?[0-9]+", priority = 2)]
    IntLiteral,
    #[regex("-?[0-9]+\\.[0-9]+")]
    FloatLiteral,

    #[token("\n")]
    #[assoc(str = "\\n")]
    NewLine,

    #[token("fun")]
    #[assoc(str = "fun")]
    Fun,
    #[token("use")]
    #[assoc(str = "use")]
    Use,
    #[token("if")]
    #[assoc(str = "if")]
    If,
    #[token("else")]
    #[assoc(str = "else")]
    Else,
    #[token("for")]
    #[assoc(str = "for")]
    For,
    #[token("in")]
    #[assoc(str = "in")]
    In,
    #[token("while")]
    #[assoc(str = "while")]
    While,
    #[token("loop")]
    #[assoc(str = "loop")]
    Loop,
    #[token("match")]
    #[assoc(str = "match")]
    Match,

    #[token("continue")]
    #[assoc(str = "continue")]
    Continue,
    #[token("break")]
    #[assoc(str = "break")]
    Break,
    #[token("return")]
    #[assoc(str = "return")]
    Return,

    #[token("->")]
    #[assoc(str = "->")]
    Arrow,
    #[token("=>")]
    #[assoc(str = "=>")]
    FatArrow,

    #[token(":")]
    #[assoc(str = ":")]
    Colon,
    #[token(";")]
    #[assoc(str = ";")]
    SemiColon,
    #[token("=")]
    #[assoc(str = "=")]
    Equal,
    #[token("'")]
    #[assoc(str = "'")]
    Quote,
    #[token("\"")]
    #[assoc(str = "\"")]
    DoubleQuote,
    #[token("$")]
    #[assoc(str = "$")]
    Dollar,
    #[token("&")]
    #[assoc(str = "&")]
    Ampersand,
    #[token("@")]
    #[assoc(str = "@")]
    At,
    #[token(",")]
    #[assoc(str = ",")]
    Comma,
    #[token(".")]
    #[assoc(str = ".")]
    Dot,
    #[token("...")]
    #[assoc(str = "...")]
    Vararg,
    #[token("..")]
    #[assoc(str = "..")]
    DotDot,

    #[token("|")]
    #[assoc(str = "|")]
    Bar,

    #[token("&&")]
    #[assoc(str = "&&")]
    And,
    #[token("||")]
    #[assoc(str = "||")]
    Or,
    #[token("!")]
    #[assoc(str = "!")]
    Not,

    #[token("==")]
    #[assoc(str = "==")]
    EqualEqual,
    #[token("!=")]
    #[assoc(str = "!=")]
    NotEqual,
    #[token("<")]
    #[assoc(str = "<")]
    Less,
    #[token("<=")]
    #[assoc(str = "<=")]
    LessEqual,
    #[token(">")]
    #[assoc(str = ">")]
    Greater,
    #[token(">=")]
    #[assoc(str = ">=")]
    GreaterEqual,

    #[token("+=")]
    #[assoc(str = "+=")]
    PlusEqual,
    #[token("-=")]
    #[assoc(str = "-=")]
    MinusEqual,
    #[token("*=")]
    #[assoc(str = "*=")]
    StarEqual,
    #[token("/=")]
    #[assoc(str = "/=")]
    SlashEqual,
    #[token("%=")]
    #[assoc(str = "%=")]
    ModuloEqual,

    #[token("+")]
    #[assoc(str = "+")]
    Plus,
    #[token("-")]
    #[assoc(str = "-")]
    Minus,
    #[token("*")]
    #[assoc(str = "*")]
    Star,
    #[token("/")]
    #[assoc(str = "/")]
    Slash,
    #[token("\\")]
    #[assoc(str = "\\")]
    BackSlash,
    #[token("%")]
    #[assoc(str = "%")]
    Percent,

    #[token("[")]
    #[assoc(str = "[")]
    SquaredLeftBracket,
    #[token("]")]
    #[assoc(str = "]")]
    SquaredRightBracket,
    #[token("(")]
    #[assoc(str = "(")]
    RoundedLeftBracket,
    #[token(")")]
    #[assoc(str = ")")]
    RoundedRightBracket,
    #[token("{")]
    #[assoc(str = "{")]
    CurlyLeftBracket,
    #[token("}")]
    #[assoc(str = "}")]
    CurlyRightBracket,

    #[regex(r"[ \t\f]+")]
    Space,

    #[regex("//.*", logos::skip)]
    #[error]
    Error,

    #[assoc(str = "<end of input>")]
    EndOfFile,
}

impl TokenType {
    pub fn is_valid_function_name(self) -> bool {
        self == Identifier
    }

    ///is this lexeme a keyword of the language ?
    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            Fun | Use | If | Else | For | In | While | Match | Val | Var
        )
    }

    ///is this lexeme a valid reference name for a variable ?
    pub fn is_valid_var_ref_name(self) -> bool {
        matches!(self, Identifier | Ampersand | At | Not | IntLiteral)
    }

    ///is this lexeme a binary operator ?
    pub fn is_bin_operator(self) -> bool {
        match self {
            And | Or | Plus | Minus | Star | Slash | Percent | EqualEqual | NotEqual | Less
            | LessEqual | Greater | GreaterEqual => true,
            _ => false,
        }
    }

    ///is this lexeme a lexeme that cannot fusion with other glued tokens
    pub fn is_identifier_bound(self) -> bool {
        match self {
            NewLine | SemiColon | Less | Bar | Greater | And | Or => true,
            _ => false,
        }
    }

    /// Tests if this token marks the end of a call statement's arguments.
    pub fn is_call_bound(self) -> bool {
        matches!(
            self,
            TokenType::Ampersand
                | TokenType::Less
                | TokenType::Greater
                | TokenType::Bar
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

    /// Tests if the token is a punctuation in a programming context.
    ///
    /// See [`TokenType::is_ponctuation()`] for a more general definition.
    pub fn is_extended_ponctuation(self) -> bool {
        matches!(
            self,
            TokenType::Comma | TokenType::DotDot | TokenType::Arrow | TokenType::FatArrow
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

    pub fn closing_pair(self) -> Option<TokenType> {
        match self {
            SquaredLeftBracket => Some(SquaredRightBracket),
            RoundedLeftBracket => Some(RoundedRightBracket),
            CurlyLeftBracket => Some(CurlyRightBracket),
            _ => None,
        }
    }
}
