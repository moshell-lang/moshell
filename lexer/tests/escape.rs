use lexer::lexer::lex;
use lexer::token::{Token, TokenType};

#[test]
fn escaped_filename() {
    let tokens = lex("cat filename\\ with\\ spaces\\ and\\@.txt");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Identifier, "cat"),
            Token::new(TokenType::Space, " "),
            Token::new(
                TokenType::Identifier,
                "filename\\ with\\ spaces\\ and\\@.txt"
            ),
        ]
    );
}

#[test]
fn escaped_string() {
    let tokens = lex("'It\\'s incredible'");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Quote, "'"),
            Token::new(TokenType::Identifier, "It\\'s"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Identifier, "incredible"),
            Token::new(TokenType::Quote, "'"),
        ]
    );
}
