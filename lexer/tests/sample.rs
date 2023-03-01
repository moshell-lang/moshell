use lexer::lexer::lex;
use lexer::token::{Token, TokenType};
use pretty_assertions::assert_eq;

#[test]
fn variable_and_initializer() {
    let tokens = lex("var x = 1+2");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Var, "var"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Identifier, "x"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Equal, "="),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::IntLiteral, "1"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::IntLiteral, "2"),
        ]
    );
}

#[test]
fn relative_path() {
    let tokens = lex("cd ./some/path");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Identifier, "cd"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Dot, "."),
            Token::new(TokenType::Slash, "/"),
            Token::new(TokenType::Identifier, "some"),
            Token::new(TokenType::Slash, "/"),
            Token::new(TokenType::Identifier, "path"),
        ]
    );
}

#[test]
fn quoted_arithmetic() {
    let tokens = lex("echo \"Result $((1+2*3))\"");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Identifier, "echo"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::Identifier, "Result"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::IntLiteral, "1"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::IntLiteral, "2"),
            Token::new(TokenType::Star, "*"),
            Token::new(TokenType::IntLiteral, "3"),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::DoubleQuote, "\""),
        ]
    );
}

#[test]
fn run_sample() {
    let content = include_str!("sample.msh");

    let tokens = lex(content);
    println!("{:?}", tokens);
}
