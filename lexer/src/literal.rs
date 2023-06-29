use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub(crate) fn is_not_identifier_part(c: char) -> bool {
    matches!(
        c,
        ';' | ':'
            | '<'
            | '>'
            | '|'
            | '&'
            | '@'
            | '\''
            | '"'
            | '/'
            | '\\'
            | '+'
            | '-'
            | '*'
            | '%'
            | '['
            | ']'
            | '('
            | ')'
            | '{'
            | '}'
            | '$'
            | ','
            | '.'
            | '='
    ) || c.is_whitespace()
}

impl<'a> Lexer<'a> {
    pub(crate) fn next_identifier(&mut self, start_pos: usize, start_char: char) -> Token<'a> {
        let mut pos = start_pos + start_char.len_utf8();
        while let Some((p, c)) = self.iter.peek() {
            if is_not_identifier_part(*c) {
                break;
            }
            pos = p + c.len_utf8();
            self.iter.next();
        }
        let value = &self.input[start_pos..pos];
        let token_type = match value {
            "as" => TokenType::As,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "in" => TokenType::In,
            "loop" => TokenType::Loop,
            "match" => TokenType::Match,
            "return" => TokenType::Return,
            "shell" => TokenType::Shell,
            "true" => TokenType::True,
            "use" => TokenType::Use,
            "var" => TokenType::Var,
            "val" => TokenType::Val,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };
        Token::new(token_type, value)
    }

    pub(crate) fn next_number(&mut self, start_pos: usize) -> Token<'a> {
        let mut pos = start_pos + 1;
        let mut is_float = false;
        let mut it = self.iter.clone();
        while let Some((p, c)) = it.peek().copied() {
            if c.is_ascii_digit() {
                pos = p + 1;
                it.next();
                self.iter = it.clone();
            } else if c == '.'
                && !is_float
                && it.next().is_some()
                && it.peek().map(|(_, c)| c.is_ascii_digit()).unwrap_or(false)
            {
                pos = p + 1;
                is_float = true;
            } else {
                break;
            }
        }
        Token::new(
            if is_float {
                TokenType::FloatLiteral
            } else {
                TokenType::IntLiteral
            },
            &self.input[start_pos..pos],
        )
    }

    pub(crate) fn next_space(&mut self, start_pos: usize, start_char: char) -> Token<'a> {
        let mut pos = start_pos + start_char.len_utf8();
        while let Some((p, c)) = self.iter.peek().copied() {
            if c == '\\' {
                let mut it = self.iter.clone();
                it.next();
                if let Some((p, c)) = it.next() {
                    if c.is_whitespace() {
                        pos = p + c.len_utf8();
                        self.iter = it;
                        continue;
                    }
                }
                break;
            } else if c == ' ' || c == '\t' {
                pos = p + c.len_utf8();
                self.iter.next();
            } else {
                break;
            }
        }
        Token::new(TokenType::Space, &self.input[start_pos..pos])
    }

    /// Returns `true` if the lexer is currently in a string.
    pub(crate) fn is_in_string(&self) -> bool {
        self.string_depth & 1 == 1
    }

    /// Toggles the string state.
    pub(crate) fn toggle_in_string_state(&mut self) {
        if self.is_in_string() {
            self.string_depth -= 1;
        } else {
            self.string_depth += 1;
        }
    }
}
