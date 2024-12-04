use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use unicode_ident::{is_xid_continue, is_xid_start};

impl Lexer<'_> {
    pub(crate) fn next_identifier(&mut self, start_pos: usize, start_char: char) -> Token {
        if !is_xid_start(start_char) && !start_char.is_ascii_digit() && start_char != '_' {
            return Token::new(
                TokenType::Error,
                start_pos..start_pos + start_char.len_utf8(),
            );
        }
        let mut pos = start_pos + start_char.len_utf8();
        while let Some((p, c)) = self.iter.peek() {
            if !is_xid_continue(*c) {
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
            "impl" => TokenType::Impl,
            "in" => TokenType::In,
            "loop" => TokenType::Loop,
            "match" => TokenType::Match,
            "reef" => TokenType::Reef,
            "return" => TokenType::Return,
            "self" => TokenType::Slf,
            "struct" => TokenType::Struct,
            "true" => TokenType::True,
            "use" => TokenType::Use,
            "var" => TokenType::Var,
            "val" => TokenType::Val,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };
        Token::new(token_type, start_pos..pos)
    }

    pub(crate) fn next_number(&mut self, start_pos: usize) -> Token {
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
            } else if !is_xid_continue(c) {
                break;
            } else {
                return self.next_identifier(
                    start_pos,
                    self.input[start_pos..]
                        .chars()
                        .next()
                        .expect("Invalid starting position"),
                );
            }
        }
        Token::new(
            if is_float {
                TokenType::FloatLiteral
            } else {
                TokenType::IntLiteral
            },
            start_pos..pos,
        )
    }

    pub(crate) fn next_space(&mut self, start_pos: usize, start_char: char) -> Token {
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
        Token::new(TokenType::Space, start_pos..pos)
    }
}
