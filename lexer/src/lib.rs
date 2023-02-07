#![allow(dead_code)]

use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, PartialEq)]
pub struct LexError {
    pub message: String,
    pub index: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub pos: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
    Integer(usize),
}

pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(input);
    lexer.lex()
}

struct Lexer<'a> {
    input: &'a str,
    it: Peekable<CharIndices<'a>>,
    current_token_start: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            it: input.char_indices().peekable(),
            current_token_start: 0,
        }
    }

    fn lex(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens: Vec<Token> = Vec::new();
        while let Some(ch) = self.next() {
            match ch {
                '0'..='9' => {
                    tokens.push(self.read_number());
                }
                _ => {
                    return Err(LexError {
                        message: format!("Unexpected character: {ch}"),
                        index: self.current_token_start,
                    });
                }
            }
        }
        Ok(tokens)
    }

    fn read_number(&mut self) -> Token {
        let end = self.next_while(|ch| ch.is_ascii_digit());
        Token {
            value: TokenValue::Integer(
                self.input[self.current_token_start..end]
                    .parse::<usize>()
                    .expect("Failed to parse integer, invalid numbers should be detected before"),
            ),
            pos: self.current_token_start,
        }
    }

    fn next(&mut self) -> Option<char> {
        self.it.next().map(|(pos, ch)| {
            self.current_token_start = pos;
            ch
        })
    }

    fn next_while(&mut self, predicate: impl Fn(char) -> bool) -> usize {
        let mut pos = self.current_token_start;
        while let Some((p, ch)) = self.it.peek() {
            if predicate(*ch) {
                pos = *p;
                self.it.next();
            } else {
                break;
            }
        }
        pos + 1
    }

    fn peek(&mut self) -> Option<char> {
        self.it.peek().map(|(_, ch)| *ch)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_int() {
        assert_eq!(
            lex("1"),
            Ok(vec![Token {
                value: TokenValue::Integer(1),
                pos: 0
            }])
        );
        assert_eq!(
            lex("123"),
            Ok(vec![Token {
                value: TokenValue::Integer(123),
                pos: 0
            }])
        );
        assert_eq!(
            lex("4b"),
            Err(LexError {
                message: "Unexpected character: b".to_string(),
                index: 1
            })
        );
    }
}
