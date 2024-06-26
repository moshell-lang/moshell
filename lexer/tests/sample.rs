use lexer::delimiter::UnmatchedDelimiter;
use lexer::token::TokenType;
use lexer::unescape;
use pretty_assertions::assert_eq;

#[derive(Debug, PartialEq, Eq)]
struct Token<'a> {
    token_type: TokenType,
    text: &'a str,
}

impl Token<'_> {
    fn new(token_type: TokenType, text: &str) -> Token {
        Token { token_type, text }
    }
}

fn lex(input: &str) -> Vec<Token> {
    let (tokens, unmatched) = lexer::lex(input);
    assert_eq!(unmatched, &[]);
    tokens
        .iter()
        .map(|t| Token {
            token_type: t.token_type,
            text: t.text(input),
        })
        .collect::<Vec<_>>()
}

#[test]
fn string_literal() {
    let tokens = lex("'It\\'s http://localhost  //true' // comment\nyes");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::StringLiteral, "It\\'s http://localhost  //true"),
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
            opening: Some(input.find('(').unwrap()),
            candidate: None,
            closing: None
        }]
    );
    assert_eq!(
        tokens
            .iter()
            .map(|t| Token {
                token_type: t.token_type,
                text: t.text(input),
            })
            .collect::<Vec<_>>(),
        vec![
            Token::new(TokenType::Identifier, "echo"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::StringStart, "\""),
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
            Token::new(TokenType::StringStart, "\""),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::StringStart, "\""),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::IntLiteral, "1"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::IntLiteral, "1"),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::StringContent, "//"),
            Token::new(TokenType::StringEnd, "\""),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::StringContent, "//"),
            Token::new(TokenType::StringEnd, "\""),
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
            Token::new(TokenType::Error, "🦀"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Equal, "="),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::StringStart, "\""),
            Token::new(TokenType::StringContent, "épique"),
            Token::new(TokenType::StringEnd, "\""),
        ]
    );
}

#[test]
fn accept_underscore_indent_start() {
    let tokens = lex("_x");
    assert_eq!(tokens, [Token::new(TokenType::Identifier, "_x")]);
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
            Token::new(TokenType::StringStart, "\""),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::Identifier, "ls"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::StringStart, "\""),
            Token::new(TokenType::StringContent, "tes"),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::RoundedLeftBracket, "("),
            Token::new(TokenType::Identifier, "echo"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::StringStart, "\""),
            Token::new(TokenType::StringContent, "t"),
            Token::new(TokenType::StringEnd, "\""),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::StringEnd, "\""),
            Token::new(TokenType::RoundedRightBracket, ")"),
            Token::new(TokenType::StringEnd, "\""),
        ]
    );
}

#[test]
fn var_in_literal() {
    let tokens = lex("\"$a = ${b}\"");
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::StringStart, "\""),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::Identifier, "a"),
            Token::new(TokenType::StringContent, " = "),
            Token::new(TokenType::Dollar, "$"),
            Token::new(TokenType::CurlyLeftBracket, "{"),
            Token::new(TokenType::Identifier, "b"),
            Token::new(TokenType::CurlyRightBracket, "}"),
            Token::new(TokenType::StringEnd, "\""),
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
            Token::new(TokenType::StringStart, "\""),
            Token::new(TokenType::StringContent, "Result "),
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
            Token::new(TokenType::StringEnd, "\""),
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
    let tokens = lex("ls//*\n * This is a comment\n*/exit");
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

#[test]
fn null_byte() {
    let input = "\0";
    let tokens = lex(input);
    assert_eq!(tokens, [Token::new(TokenType::Error, "\0")]);
}

#[test]
fn too_many_escapes() {
    let input = r"\\\";
    assert_eq!(unescape(input), Err(lexer::EscapeError::UnexpectedEnd));
}

#[test]
fn unknown_escape() {
    let input = r"\z";
    assert_eq!(
        unescape(input),
        Err(lexer::EscapeError::InvalidEscape { idx: 1 })
    );
}

#[test]
fn unescape_normal() {
    let input = r#"\n \r \t \\ \""#;
    assert_eq!(unescape(input), Ok("\n \r \t \\ \"".to_string()));
}
