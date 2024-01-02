use context::source::SourceSegment;
use std::iter::Peekable;
use std::str::CharIndices;

use crate::delimiter::UnmatchedDelimiter;
use crate::token::{Token, TokenType};

/// A lexer that iterates over the input string and produces tokens.
pub(crate) struct Lexer<'a> {
    /// The iterator over the input string.
    pub(crate) iter: Peekable<CharIndices<'a>>,

    /// The input string.
    pub(crate) input: &'a str,

    /// The stack to keep track of string depths.
    pub(crate) open_delimiters: Vec<Token>,

    /// The vector of unmatched delimiter errors.
    pub(crate) mismatches: Vec<UnmatchedDelimiter>,

    state: LexerState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexerState {
    Normal,
    Variable,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
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
    /// Creates a new lexer.
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            iter: input.char_indices().peekable(),
            input,
            open_delimiters: Vec::new(),
            mismatches: Vec::new(),
            state: LexerState::Normal,
        }
    }

    fn next_token(&mut self) -> Token {
        if let Some((pos, c)) = self.iter.next() {
            if self.is_in_string() {
                if c == '$' {
                    self.state = LexerState::Variable;
                } else if c == '"' {
                    self.open_delimiters.pop();
                    return Token::new(TokenType::StringEnd, pos..pos + 1);
                }
            }
            if self.is_in_string() && self.state == LexerState::Normal {
                self.next_content(pos)
            } else {
                self.next_token_char(pos, c)
            }
        } else {
            Token::new(TokenType::EndOfFile, SourceSegment::default())
        }
    }

    /// Creates the next token, given that the first character is already known.
    fn next_token_char(&mut self, pos: usize, ch: char) -> Token {
        self.state = LexerState::Normal;
        let mut size = ch.len_utf8();
        let token_type = match ch {
            '\'' => {
                return self.next_string(pos);
            }
            '"' => {
                self.open_delimiters
                    .push(Token::new(TokenType::StringStart, pos..pos + 1));
                TokenType::StringStart
            }
            '`' => {
                if let Some(Token {
                    token_type: TokenType::Backtick,
                    ..
                }) = self.open_delimiters.last()
                {
                    self.open_delimiters.pop();
                } else {
                    self.open_delimiters
                        .push(Token::new(TokenType::Backtick, pos..pos + 1));
                }
                TokenType::Backtick
            }
            '/' => {
                if self.matches_next('/', &mut size) {
                    return if self.matches_next('*', &mut size) {
                        self.skip_multiline_comment();
                        self.next_token()
                    } else {
                        self.skip_line()
                    };
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
            '(' => {
                self.open_delimiters
                    .push(Token::new(TokenType::RoundedLeftBracket, pos..pos + 1));
                TokenType::RoundedLeftBracket
            }
            ')' => {
                self.pop_delimiter_if(TokenType::RoundedLeftBracket);
                TokenType::RoundedRightBracket
            }
            '{' => {
                self.open_delimiters
                    .push(Token::new(TokenType::CurlyLeftBracket, pos..pos + 1));
                TokenType::CurlyLeftBracket
            }
            '}' => {
                self.pop_delimiter_if(TokenType::CurlyLeftBracket);
                TokenType::CurlyRightBracket
            }
            '@' => TokenType::At,
            '~' => TokenType::Tilde,
            '$' => {
                self.state = LexerState::Variable;
                TokenType::Dollar
            }
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
            ':' => {
                if self.matches_next(':', &mut size) {
                    TokenType::ColonColon
                } else {
                    TokenType::Colon
                }
            }
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
                return self.next_identifier(pos, c);
            }
        };
        Token::new(token_type, pos..pos + size)
    }

    /// Yields a token for a escape character.
    fn next_escape(&mut self, mut start_pos: usize) -> Token {
        if let Some((_, c)) = self.iter.next() {
            start_pos += 1;
            match c {
                '\r' | '\n' => self.next_space(start_pos, c),
                _ => Token::new(TokenType::Identifier, start_pos..start_pos + c.len_utf8()),
            }
        } else {
            Token::new(TokenType::EndOfFile, start_pos..start_pos + 1)
        }
    }

    /// Tests the next character.
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

    /// Skip the remaining characters of the current line.
    fn skip_line(&mut self) -> Token {
        for (pos, c) in self.iter.by_ref() {
            if c == '\n' {
                return Token::new(TokenType::NewLine, pos..pos + 1);
            }
        }
        Token::new(TokenType::EndOfFile, SourceSegment::default())
    }

    /// Skip the remaining characters of the current multiline comment.
    fn skip_multiline_comment(&mut self) {
        while let Some((_, c)) = self.iter.next() {
            if c == '*' && self.iter.next_if(|(_, c)| *c == '/').is_some() {
                break;
            }
        }
    }

    fn next_string(&mut self, start: usize) -> Token {
        let mut end = start + 1;
        let mut escape = false;
        for (pos, c) in self.iter.by_ref() {
            end = pos;
            if escape {
                escape = false;
                continue;
            }
            if c == '\\' {
                escape = true;
                continue;
            }
            if c == '\'' {
                return Token::new(TokenType::StringLiteral, start + 1..end);
            }
        }
        self.mismatches.push(UnmatchedDelimiter {
            opening: Some(start),
            candidate: None,
            closing: None,
        });
        Token::new(TokenType::StringLiteral, start + 1..end)
    }

    fn next_content(&mut self, start: usize) -> Token {
        let mut end = start;
        let mut escape = false;
        while let Some(&(pos, c)) = self.iter.peek() {
            end = pos;
            if escape {
                escape = false;
                self.iter.next();
                continue;
            }
            if c == '\\' {
                escape = true;
                self.iter.next();
                continue;
            }
            if c == '$' {
                self.state = LexerState::Variable;
                break;
            }
            if c == '\"' {
                break;
            }
            self.iter.next();
        }
        Token::new(TokenType::StringContent, start..end)
    }

    fn is_in_string(&self) -> bool {
        self.open_delimiters
            .last()
            .map_or(false, |token| token.token_type == TokenType::StringStart)
    }

    fn pop_delimiter_if(&mut self, token_type: TokenType) {
        if let Some(open_delimiter) = self.open_delimiters.pop() {
            if open_delimiter.token_type != token_type {
                self.open_delimiters.push(open_delimiter);
            }
        }
    }
}
