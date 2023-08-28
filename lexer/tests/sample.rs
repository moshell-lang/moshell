use lexer::delimiter::UnmatchedDelimiter;
use lexer::token::{Token, TokenType};
use pretty_assertions::assert_eq;

fn lex(input: &str) -> Vec<Token> {
    let (tokens, unmatched) = lexer::lex(input);
    assert_eq!(unmatched, &[]);
    tokens
}

#[test]
fn string_literal() {
    let tokens = lex("'It\\'s http://localhost  //true' // comment\nyes");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Quote, "'"),
            Token::new(TokenType::Identifier, "It"),
            Token::new(TokenType::Identifier, "'"),
            Token::new(TokenType::Identifier, "s"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Identifier, "http"),
            Token::new(TokenType::Colon, ":"),
            Token::new(TokenType::Identifier, "/"),
            Token::new(TokenType::Identifier, "/localhost"),
            Token::new(TokenType::Space, "  "),
            Token::new(TokenType::Identifier, "/"),
            Token::new(TokenType::Identifier, "/true"),
            Token::new(TokenType::Quote, "'"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::NewLine, "\n"),
            Token::new(TokenType::Identifier, "yes"),
        ]
    );
}

#[test]
fn glue_tokens() {
    let tokens = lex("echo 729zip1 cut2");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Identifier, "echo"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Identifier, "729zip1"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Identifier, "cut2"),
        ]
    );
}

#[test]
fn string_literal_unterminated_due_to_comment() {
    let input = "echo \"$(//)\"";
    let (tokens, unmatched) = lexer::lex(input);
    assert_eq!(
        unmatched,
        vec![UnmatchedDelimiter {
            opening: Some(input.find("(").unwrap()),
            candidate: None,
            closing: None
        }]
    );
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Identifier, "echo"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
        ]
    );
}

#[test]
fn string_literal_comment_after_arithmetic() {
    let tokens = lex("echo \"$(\"$((1+1))//\")//\"// comment");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Identifier, "echo"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::IntLiteral, "1"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::IntLiteral, "1"),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::Identifier, "/"),
            Token::new(TokenType::Identifier, "/"),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::Identifier, "/"),
            Token::new(TokenType::Identifier, "/"),
            Token::new(TokenType::DoubleQuote, "\""),
        ]
    );
}

#[test]
fn utf8_compatible() {
    let tokens = lex("val 🦀 = \"épique\"");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Val, "val"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Identifier, "🦀"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Equal, "="),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::Identifier, "épique"),
            Token::new(TokenType::DoubleQuote, "\""),
        ]
    );
}

#[test]
fn variable_and_initializer() {
    let tokens = lex("var x = 1.0+2");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Var, "var"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Identifier, "x"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Equal, "="),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::FloatLiteral, "1.0"),
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
fn literal_in_literal() {
    let tokens = lex("echo \"$(ls \"tes$(echo \"t\")\")\"");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Identifier, "echo"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::Identifier, "ls"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::Identifier, "tes"),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::Identifier, "echo"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::Identifier, "t"),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::DoubleQuote, "\""),
        ]
    );
}

#[test]
fn var_in_literal() {
    let tokens = lex("\"$a = ${b}\"");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::DoubleQuote, "\""),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::Identifier, "a"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Equal, "="),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::CurlyLeftBracket, "{"),
            Token::new(TokenType::Identifier, "b"),
            Token::new(TokenType::CurlyRightBracket, "}"),
            Token::new(TokenType::DoubleQuote, "\""),
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
fn numbers() {
    let tokens = lex("42 42.17 10..15");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::IntLiteral, "42"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::FloatLiteral, "42.17"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::IntLiteral, "10"),
            Token::new(TokenType::DotDot, ".."),
            Token::new(TokenType::IntLiteral, "15"),
        ]
    );
}

#[test]
fn multiline_comments() {
    let tokens = lex("ls/*\n * This is a comment\n*/exit");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Identifier, "ls"),
            Token::new(TokenType::Identifier, "exit"),
        ]
    );
}

#[test]
fn short_divide() {
    let tokens = lex("$(($a/$b))");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::Identifier, "a"),
            Token::new(TokenType::Slash, "/"),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::Identifier, "b"),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::RoundedRightBracket, ")"),
        ]
    );
}

#[test]
fn unmatched_delimiter() {
    let input = "( [ )]) = {";
    let unmatched = lexer::lex(input).1;
    assert_eq!(
        unmatched,
        vec![
            UnmatchedDelimiter {
                opening: Some(input.find('[').unwrap()),
                candidate: Some(input.find(')').unwrap()),
                closing: Some(input.find(']').unwrap()),
            },
            UnmatchedDelimiter {
                opening: Some(input.find('{').unwrap()),
                candidate: None,
                closing: None,
            }
        ]
    );
}

#[test]
fn unfool() {
    let input = "{) {} }";
    let unmatched = lexer::lex(input).1;
    assert_eq!(
        unmatched,
        vec![UnmatchedDelimiter {
            opening: Some(input.find('{').unwrap()),
            candidate: Some(input.find(')').unwrap()),
            closing: Some(input.rfind('}').unwrap()),
        }],
    );
}

#[test]
fn too_many_delimiter() {
    let input = "())";
    let unmatched = lexer::lex(input).1;
    assert_eq!(
        unmatched,
        vec![UnmatchedDelimiter {
            opening: None,
            candidate: Some(input.rfind(')').unwrap()),
            closing: None,
        },]
    );
}
