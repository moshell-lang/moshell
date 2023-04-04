use crate::lexer::Lexer;
use crate::literal::is_not_identifier_part;
use crate::token::{Token, TokenType};

impl<'a> Lexer<'a> {
    /// Reads a keyword token if the next characters match the keyword.
    ///
    /// The first letter of the keyword must have already been consumed.
    pub(crate) fn read_keyword(&mut self, ch: char) -> Option<Token<'a>> {
        match ch {
            'b' => self.emit_keyword("reak", TokenType::Break),
            'c' => self.emit_keyword("ontinue", TokenType::Continue),
            'e' => self.emit_keyword("lse", TokenType::Else),
            'f' => self
                .emit_keyword("un", TokenType::Fun)
                .or_else(|| self.emit_keyword("or", TokenType::For)),
            'i' => self
                .emit_keyword("f", TokenType::If)
                .or_else(|| self.emit_keyword("n", TokenType::In)),
            'l' => self.emit_keyword("oop", TokenType::Loop),
            'm' => self.emit_keyword("atch", TokenType::Match),
            'r' => self.emit_keyword("eturn", TokenType::Return),
            'u' => self.emit_keyword("se", TokenType::Use),
            'U' => self.emit_keyword("nit", TokenType::Unit),
            'N' => self.emit_keyword("othing", TokenType::Nothing),
            'v' => self
                .emit_keyword("ar", TokenType::Var)
                .or_else(|| self.emit_keyword("al", TokenType::Val)),
            'w' => self.emit_keyword("hile", TokenType::While),
            _ => None,
        }
    }

    /// Emits a keyword token if the next characters match the expected keyword.
    fn emit_keyword(&mut self, expected: &str, token_type: TokenType) -> Option<Token<'a>> {
        if let Some(end_pos) = self.matches_keyword(expected) {
            Some(Token::new(
                token_type,
                &self.input[end_pos - expected.len()..end_pos + 1],
            ))
        } else {
            None
        }
    }

    /// Tests if the next characters match the expected keyword, and returns the end position if so.
    fn matches_keyword(&mut self, expected: &str) -> Option<usize> {
        let mut it = self.iter.clone();
        let mut pos = 0;
        for c in expected.chars() {
            if let Some((p, ch)) = it.peek() {
                pos = *p;
                if *ch != c {
                    return None;
                }
                it.next();
            } else {
                return None;
            }
        }
        if let Some((_, ch)) = it.peek() {
            if is_not_identifier_part(*ch) {
                self.iter = it;
                Some(pos)
            } else {
                None
            }
        } else {
            self.iter = it;
            Some(pos)
        }
    }
}
