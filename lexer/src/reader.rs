use std::io;
use logos::Logos;
use crate::token::{Token, TokenType};

pub struct BufferedTokenReader<'a, S> {
    input: S,
    buff: Vec<Token<'a>>,
    pos: usize,
    end_of_input: bool,
}

impl<'a, S> BufferedTokenReader<'a, S> {
    fn is_at_end(&self) -> bool {
        self.end_of_input
    }
}

pub trait LineSupplier<'a, E> {
    fn next_line(&'a mut self) -> Result<Option<&'a str>, E>;
}


///implementation for a line supplier input
impl<'a, S: LineSupplier<'a, io::Error>> BufferedTokenReader<'a, S>
{
    pub fn from_line_supplier(supplier: S) -> Self {
        Self {
            input: supplier,
            buff: Vec::new(),
            pos: 0,
            end_of_input: false
        }
    }

    pub fn next(&'a mut self) -> Result<Option<Token>, io::Error> {

        if self.pos == self.buff.len() {
            return self.refill();
        }

        if self.end_of_input {
            return Ok(None);
        }

        let token = self.buff[self.pos].clone();
        self.pos += 1;
        Ok(Some(token.clone()))
    }

    fn refill(&'a mut self) -> Result<Option<Token>, io::Error> {
        if let Some(line) = self.input.next_line()? {

            let mut lexer = TokenType::lexer(line);

            self.buff.clear();
            while let Some(token_type) = lexer.next() {
                self.buff.push(Token::new(token_type, lexer.slice()))
            }
            self.pos = 0; //reset buffer pos
            return self.next();
        }

        self.end_of_input = true;
        return self.next();
    }
}
