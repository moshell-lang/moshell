mod tests;

use regex::{Match, Regex};
use logos::{Lexer, Logos};
use crate::IdentifierType::*;
use lazy_static::lazy_static;

#[derive(Debug, PartialEq)]
pub struct LexError {
    pub message: String,
    pub offset: usize,
}

lazy_static! {
    pub static ref VAR_REF_REGEX: Regex = Regex::new(r"\\$([a-zA-Z0-9]+)").unwrap();
    pub static ref COMP_REF_REGEX: Regex = Regex::new(r"([a-zA-Z0-9]+)").unwrap();
    pub static ref LIST_ANY_REF_REGEX: Regex = Regex::new("\"(.*)\"").unwrap();
    pub static ref ANY_REF_REGEX: Regex = Regex::new("(\\S+)").unwrap();
}

#[derive(Debug, PartialEq)]
pub enum IdentifierType {
    VarRef,
    ComponentRef,
    AnyRef,
}

fn determine_type<'a>(lex: &'a mut Lexer<'a, Token<'a>>) -> Option<IdentifierValue<'a>> {
    let slice = lex.slice();
    None.or_else(|| (VAR_REF_REGEX.captures(slice).zip(Some(VarRef))))
        .or_else(|| (COMP_REF_REGEX.captures(slice).zip(Some(ComponentRef))))
        .or_else(|| (LIST_ANY_REF_REGEX.captures(slice).zip(Some(AnyRef))))
        .or_else(|| (ANY_REF_REGEX.captures(slice).zip(Some(AnyRef))))
        .and_then(|(caps, id)| (caps.get(1).zip(Some(id))))
        .map(|(m, id)| IdentifierValue::new(m.as_str(), id))
}

#[derive(Debug, PartialEq)]
pub struct IdentifierValue<'a> {
    pub value: &'a str,
    pub identifier_type: IdentifierType,
}

impl<'a> IdentifierValue<'a> {
    fn new(value: &'a str, id_type: IdentifierType) -> Self {
        Self {
            value, identifier_type: id_type,
        }
    }
}


#[derive(Logos, Debug, PartialEq)]
pub enum Token<'a> {
    #[token("var")]
    Var,
    #[token("val")]
    Val,

    #[regex(".*", determine_type)]
    Identifier(IdentifierValue<'a>),

    #[regex("[+-]?[0-9]+", | lex | lex.slice().parse(), priority = 2)]
    IntLiteral(i64),
    #[regex("[+-]?[0-9]+\\.[0-9]+", | lex | lex.slice().parse())]
    FloatLiteral(f64),

    #[token("\n")]
    NewLine,

    #[token("fun")]
    Fun,
    #[token("->")]
    Arrow,

    #[token("use")]
    Use,

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

    #[token("==")]
    EqualEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,

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




