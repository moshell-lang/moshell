use context::source::SourceSegment;
use enum_assoc::Assoc;

use crate::token::TokenType::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: SourceSegment,
}

impl Token {
    pub fn new(token_type: TokenType, span: SourceSegment) -> Self {
        Self { token_type, span }
    }

    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.span.clone()]
    }
}

#[derive(Assoc, Debug, PartialEq, Eq, Clone, Copy)]
#[func(pub fn str(&self) -> Option<&'static str>)]
pub enum TokenType {
    #[assoc(str = "var")]
    Var,
    #[assoc(str = "val")]
    Val,

    #[assoc(str = "`")]
    Backtick,
    StringStart,
    StringEnd,
    StringContent,
    StringLiteral,
    Identifier,

    IntLiteral,
    FloatLiteral,

    #[assoc(str = "struct")]
    Struct,
    #[assoc(str = "impl")]
    Impl,

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
    #[assoc(str = "reef")]
    Reef,
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

    #[assoc(str = "self")]
    Slf,

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
    #[assoc(str = "~")]
    Tilde,
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

    #[assoc(str = "+")]
    Plus,
    #[assoc(str = "-")]
    Minus,
    #[assoc(str = "*")]
    Star,
    #[assoc(str = "/")]
    Slash,
    #[assoc(str = "%")]
    Percent,
    #[assoc(str = "^")]
    Caret,
    #[assoc(str = "?")]
    QuestionMark,

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
            As | Break
                | Continue
                | Else
                | False
                | For
                | Fun
                | If
                | Impl
                | In
                | Loop
                | Match
                | Reef
                | Return
                | Slf
                | Struct
                | True
                | Use
                | Val
                | Var
                | While
        )
    }

    ///is this lexeme a valid reference name for a variable ?
    pub fn is_valid_var_ref_name(self) -> bool {
        matches!(
            self,
            Identifier | Ampersand | At | Not | Caret | IntLiteral | Dollar | Star | Slf
        )
    }

    /// Tests if this token is a valid identifier.
    pub fn is_infix_operator(self) -> bool {
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
                | DotDot
                | As
        )
    }

    /// Tests if this token is a prefix operator.
    pub fn is_prefix_operator(self) -> bool {
        matches!(self, Not | Minus)
    }

    /// Tests if this token marks the end of a call statement's arguments.
    pub fn is_call_bound(self) -> bool {
        matches!(
            self,
            Bar | Or
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
    pub fn is_punctuation(self) -> bool {
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
                | Error
        )
    }

    /// Tests if the token is a punctuation in a programming context.
    ///
    /// See [`TokenType::is_punctuation()`] for a more general definition.
    pub fn is_extended_punctuation(self) -> bool {
        matches!(self, Comma | DotDot | Arrow | FatArrow | Not)
    }

    ///is this lexeme a opening punctuation
    pub fn is_opening_punctuation(self) -> bool {
        matches!(self, |SquaredLeftBracket| RoundedLeftBracket
            | CurlyLeftBracket)
    }

    ///is this lexeme a closing punctuation
    pub fn is_closing_punctuation(self) -> bool {
        matches!(self, |SquaredRightBracket| RoundedRightBracket
            | CurlyRightBracket)
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

    /// Determines if this token creates a shell context when at the start of a statement.
    pub fn belongs_to_shell(self) -> bool {
        matches!(
            self,
            StringStart
                | StringLiteral
                | StringContent
                | StringEnd
                | Identifier
                | NewLine
                | SemiColon
                | Dollar
                | Ampersand
                | At
                | Comma
                | Dot
                | Bar
                | And
                | Or
                | Not
                | Less
                | Greater
                | Plus
                | Minus
                | Star
                | Slash
                | Percent
                | SquaredRightBracket
                | RoundedRightBracket
                | CurlyRightBracket
                | Space
                | EndOfFile
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
