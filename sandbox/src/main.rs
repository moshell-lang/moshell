use std::fs;
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[token("(")]
    LeftRoundBracket,
    #[token(")")]
    RightRoundBracket,
    #[token("[")]
    LeftSquareBracket,
    #[token("]")]
    RightSquareBracket,
    #[token("fun")]
    Fun,
    #[token("\\S+")]
    Any(str),

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error
}

fn main() {
    let contents = fs::read_to_string("sample.msh")
        .expect("Cannot open file sample.msh");

    let mut lexer = Token::lexer(&contents);

    let mut token = lexer.next();
    while token != None {
        token = lexer.next();
    }

}

