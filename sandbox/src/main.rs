use logos::Logos;
use std::fs;

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
    Any,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

fn main() {
    let contents = fs::read_to_string("sample.msh").expect("Cannot open file sample.msh");

    let mut lexer = Token::lexer(&contents);

    while let Some(token) = lexer.next() {
        println!("{:?} {:?}", token, lexer.slice());
    }
}
