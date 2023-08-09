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
    let mismatches = stream.mismatches;
    (tokens, mismatches)
}

/// Tests if the delimiters in the input string are balanced, but not terminated.
pub fn is_unterminated(input: &str) -> bool {
    let mut stream = TokenStream::new(input);
    for _ in stream.by_ref() {}
    !stream.mismatches.is_empty()
        && stream
            .mismatches
            .into_iter()
            .all(|unmatched| unmatched.candidate.is_none())
}
