use crate::delimiter::{TokenStream, UnmatchedDelimiter};
use crate::token::Token;

pub mod delimiter;
mod lexer;
mod literal;
pub mod token;

/// Scans the input string and tokenizes it.
pub fn lex(input: &str) -> (Vec<Token>, Vec<UnmatchedDelimiter>) {
    let mut stream = TokenStream::new(input);
    let tokens = Vec::from_iter(&mut stream);
    let mismatches = stream.lexer.mismatches;
    (tokens, mismatches)
}

/// Tests if the delimiters in the input string are balanced, but not terminated.
pub fn is_unterminated(input: &str) -> bool {
    let mut stream = TokenStream::new(input);
    for _ in stream.by_ref() {}
    !stream.lexer.mismatches.is_empty()
        && stream
            .lexer
            .mismatches
            .into_iter()
            .all(|unmatched| unmatched.candidate.is_none())
}

pub fn unescape(input: &str) -> String {
    let mut output = String::with_capacity(input.len());
    let mut chars = input.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next().expect("Unterminated escape sequence") {
                'n' => output.push('\n'),
                'r' => output.push('\r'),
                't' => output.push('\t'),
                '\\' => output.push('\\'),
                '"' => output.push('"'),
                '\'' => output.push('\''),
                c => output.push(c),
            }
        } else {
            output.push(c);
        }
    }
    output
}
