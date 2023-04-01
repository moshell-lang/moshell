#![allow(dead_code)]

use crate::token::*;
use std::iter::Peekable;
use std::str::CharIndices;

pub fn lex(input: &str) -> Vec<Token> {
    Lexer::new(&input).collect()
}

pub(crate) struct Lexer<'a> {
    pub(crate) iter: Peekable<CharIndices<'a>>,
    pub(crate) input: &'a str,
    pub(crate) string_depth: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.token_type == TokenType::EndOfFile {
            None
        } else {
            Some(token)
        }
    }
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            iter: input.char_indices().peekable(),
            input,
            string_depth: 0,
        }
    }

    fn next_token(&mut self) -> Token<'a> {
        if let Some((pos, c)) = self.iter.next() {
            self.next_token_char(pos, c)
        } else {
            Token::new(TokenType::EndOfFile, "")
        }
    }

    fn next_token_char(&mut self, pos: usize, ch: char) -> Token<'a> {
        let mut size = ch.len_utf8();
        let token_type = match ch {
            '\'' => {
                self.string_depth += 1;
                TokenType::Quote
            }
            '"' => {
                self.string_depth += 1;
                TokenType::DoubleQuote
            }
            '/' if self.is_in_string() => {
                if self.matches_next('/', &mut size) {
                    self.skip_line();
                    return self.next_token();
                } else {
                    TokenType::Slash
                }
            }
            '\\' => {
                return self.next_escape(pos);
            }
            '+' => TokenType::Plus,
            '*' => TokenType::Star,
            '%' => TokenType::Percent,
            '[' => TokenType::SquaredLeftBracket,
            ']' => TokenType::SquaredRightBracket,
            '(' => TokenType::RoundedLeftBracket,
            ')' => TokenType::RoundedRightBracket,
            '{' => TokenType::CurlyLeftBracket,
            '}' => TokenType::CurlyRightBracket,
            '$' => TokenType::Dollar,
            ',' => TokenType::Comma,
            '.' => {
                if self.matches_next('.', &mut size) {
                    if self.matches_next('.', &mut size) {
                        TokenType::Vararg
                    } else {
                        TokenType::DotDot
                    }
                } else {
                    TokenType::Dot
                }
            }
            ';' => TokenType::SemiColon,
            ':' => TokenType::Colon,
            '\r' | '\n' => TokenType::NewLine,
            '!' => {
                if self.matches_next('=', &mut size) {
                    TokenType::NotEqual
                } else {
                    TokenType::Not
                }
            }
            '<' => {
                if self.matches_next('=', &mut size) {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                }
            }
            '>' => {
                if self.matches_next('=', &mut size) {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                }
            }
            '=' => {
                if self.matches_next('=', &mut size) {
                    TokenType::EqualEqual
                } else if self.matches_next('>', &mut size) {
                    TokenType::FatArrow
                } else {
                    TokenType::Equal
                }
            }
            '&' => {
                if self.matches_next('&', &mut size) {
                    TokenType::And
                } else {
                    TokenType::Ampersand
                }
            }
            '|' => {
                if self.matches_next('|', &mut size) {
                    TokenType::Or
                } else {
                    TokenType::Bar
                }
            }
            '-' => {
                if self.matches_next('>', &mut size) {
                    TokenType::Arrow
                } else {
                    TokenType::Minus
                }
            }
            c if c.is_whitespace() => {
                return self.next_space(pos, c);
            }
            c if c.is_ascii_digit() => {
                return self.next_number(pos);
            }
            c => {
                return if let Some(keyword) = self.read_keyword(c) {
                    keyword
                } else {
                    self.next_identifier(pos, c)
                }
            }
        };
        Token::new(token_type, &self.input[pos..pos + size])
    }

    fn next_escape(&mut self, mut start_pos: usize) -> Token<'a> {
        if let Some((_, c)) = self.iter.next() {
            start_pos += 1;
            match c {
                c if c.is_whitespace() => Token::new(
                    TokenType::Space,
                    &self.input[start_pos..start_pos + c.len_utf8()],
                ),
                _ => Token::new(
                    TokenType::Identifier,
                    &self.input[start_pos..start_pos + c.len_utf8()],
                ),
            }
        } else {
            Token::new(TokenType::BackSlash, &self.input[start_pos..start_pos + 1])
        }
    }

    fn matches_next(&mut self, expected: char, size: &mut usize) -> bool {
        if let Some((_, c)) = self.iter.peek() {
            if *c == expected {
                self.iter.next();
                *size += expected.len_utf8();
                return true;
            }
        }
        false
    }

    fn skip_line(&mut self) {
        while let Some((_, c)) = self.iter.next() {
            if c == '\n' {
                break;
            }
        }
    }
}
