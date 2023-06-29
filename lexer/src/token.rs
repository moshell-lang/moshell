use crate::token::TokenType::*;
use dbg_pls::DebugPls;
use enum_assoc::Assoc;

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

#[derive(Assoc, Debug, PartialEq, Clone, Copy, DebugPls)]
#[func(pub fn str(&self) -> Option<&'static str>)]
pub enum TokenType {
    #[assoc(str = "var")]
    Var,
    #[assoc(str = "val")]
    Val,

    Identifier,

    IntLiteral,
    FloatLiteral,

    #[assoc(str = "\\n")]
    NewLine,

    #[assoc(str = "false")]
    False,
    #[assoc(str = "true")]
    True,
    #[assoc(str = "fun")]
    Fun,
    #[assoc(str = "use")]
    Use,
    #[assoc(str = "if")]
    If,
    #[assoc(str = "else")]
    Else,
    #[assoc(str = "for")]
    For,
    #[assoc(str = "in")]
    In,
    #[assoc(str = "while")]
    While,
    #[assoc(str = "loop")]
    Loop,
    #[assoc(str = "match")]
    Match,
    #[assoc(str = "as")]
    As,
    #[assoc(str = "shell")]
    Shell,

    #[assoc(str = "continue")]
    Continue,
    #[assoc(str = "break")]
    Break,
    #[assoc(str = "return")]
    Return,

    #[assoc(str = "->")]
    Arrow,
    #[assoc(str = "=>")]
    FatArrow,

    #[assoc(str = ":")]
    Colon,
    #[assoc(str = "::")]
    ColonColon,
    #[assoc(str = ";")]
    SemiColon,
    #[assoc(str = "=")]
    Equal,
    #[assoc(str = "'")]
    Quote,
    #[assoc(str = "\"")]
    DoubleQuote,
    #[assoc(str = "$")]
    Dollar,
    #[assoc(str = "&")]
    Ampersand,
    #[assoc(str = "@")]
    At,
    #[assoc(str = ",")]
    Comma,
    #[assoc(str = ".")]
    Dot,
    #[assoc(str = "..")]
    DotDot,
    #[assoc(str = "...")]
    Vararg,

    #[assoc(str = "|")]
    Bar,

    #[assoc(str = "&&")]
    And,
    #[assoc(str = "||")]
    Or,
    #[assoc(str = "!")]
    Not,

    #[assoc(str = "==")]
    EqualEqual,
    #[assoc(str = "!=")]
    NotEqual,
    #[assoc(str = "<")]
    Less,
    #[assoc(str = "<=")]
    LessEqual,
    #[assoc(str = ">")]
    Greater,
    #[assoc(str = ">=")]
    GreaterEqual,

    #[assoc(str = "+=")]
    PlusEqual,
    #[assoc(str = "-=")]
    MinusEqual,
    #[assoc(str = "*=")]
    StarEqual,
    #[assoc(str = "/=")]
    SlashEqual,
    #[assoc(str = "%=")]
    ModuloEqual,

    #[assoc(str = "+")]
    Plus,
    #[assoc(str = "-")]
    Minus,
    #[assoc(str = "*")]
    Star,
    #[assoc(str = "/")]
    Slash,
    #[assoc(str = "\\")]
    BackSlash,
    #[assoc(str = "%")]
    Percent,

    #[assoc(str = "[")]
    SquaredLeftBracket,
    #[assoc(str = "]")]
    SquaredRightBracket,
    #[assoc(str = "(")]
    RoundedLeftBracket,
    #[assoc(str = ")")]
    RoundedRightBracket,
    #[assoc(str = "{")]
    CurlyLeftBracket,
    #[assoc(str = "}")]
    CurlyRightBracket,

    Space,

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
        matches!(
            self,
            Identifier | Ampersand | At | Not | IntLiteral | Dollar
        )
    }

    ///is this lexeme a binary operator ?
    pub fn is_bin_operator(self) -> bool {
        matches!(
            self,
            And | Or
                | Plus
                | Minus
                | Star
                | Slash
                | Percent
                | EqualEqual
                | NotEqual
                | Less
                | LessEqual
                | Greater
                | GreaterEqual
        )
    }

    ///is this lexeme a lexeme that cannot fusion with other glued tokens
    pub fn is_identifier_bound(self) -> bool {
        matches!(self, NewLine | SemiColon | Less | Bar | Greater | And | Or)
    }

    /// Tests if this token marks the end of a call statement's arguments.
    pub fn is_call_bound(self) -> bool {
        matches!(
            self,
            Ampersand
                | Less
                | Greater
                | Bar
                | Or
                | And
                | SquaredLeftBracket
                | SquaredRightBracket
                | RoundedLeftBracket
                | RoundedRightBracket
                | CurlyLeftBracket
                | CurlyRightBracket
                | SemiColon
                | NewLine
                | Error
                | EndOfFile
        )
    }

    ///is this lexeme a punctuation
    pub fn is_ponctuation(self) -> bool {
        matches!(
            self,
            Ampersand
                | Less
                | Greater
                | Bar
                | Or
                | And
                | Quote
                | DoubleQuote
                | SquaredLeftBracket
                | SquaredRightBracket
                | RoundedLeftBracket
                | RoundedRightBracket
                | CurlyLeftBracket
                | CurlyRightBracket
                | SemiColon
                | Error
        )
    }

    /// Tests if the token is a punctuation in a programming context.
    ///
    /// See [`TokenType::is_ponctuation()`] for a more general definition.
    pub fn is_extended_ponctuation(self) -> bool {
        matches!(self, Comma | DotDot | Arrow | FatArrow | Not)
    }

    ///is this lexeme a opening punctuation
    pub fn is_opening_ponctuation(self) -> bool {
        matches!(self, |SquaredLeftBracket| RoundedLeftBracket
            | CurlyLeftBracket
            | Quote
            | DoubleQuote)
    }

    ///is this lexeme a closing punctuation
    pub fn is_closing_ponctuation(self) -> bool {
        matches!(self, |SquaredRightBracket| RoundedRightBracket
            | CurlyRightBracket
            | Quote
            | DoubleQuote)
    }

    /// Tests if this token ends an expression, where newlines are not allowed.
    pub fn ends_expression(self) -> bool {
        matches!(
            self,
            SemiColon
                | NewLine
                | EndOfFile
                | SquaredRightBracket
                | RoundedRightBracket
                | CurlyRightBracket
        )
    }

    /// Tests if this token ends a group, where newlines are allowed.
    pub fn ends_group(self) -> bool {
        matches!(
            self,
            EndOfFile | SquaredRightBracket | RoundedRightBracket | CurlyRightBracket
        )
    }

    pub fn closing_pair(self) -> Option<TokenType> {
        match self {
            SquaredLeftBracket => Some(SquaredRightBracket),
            RoundedLeftBracket => Some(RoundedRightBracket),
            CurlyLeftBracket => Some(CurlyRightBracket),
            Quote => Some(Quote),
            DoubleQuote => Some(DoubleQuote),
            _ => None,
        }
    }
}
