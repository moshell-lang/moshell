use std::io;
use logos::Logos;
use crate::token::{Token, TokenType};

pub struct BufferedTokenReader<'a, I> {
    input: I,
    buff:Vec<Token<'a>>,
    pos: usize,
    end_of_input: bool,
}

impl<'a, S> BufferedTokenReader<'a, S> {
    fn is_at_end(&self) -> bool {
        self.end_of_input
    }
}

///implementation for a line supplier input
impl<'a, I> BufferedTokenReader<'a, I>
    where I: FnMut() -> Result<Option<&'a str>, io::Error>
{
    pub fn from_line_supplier(supplier: I) -> Self {
        Self {
            input: supplier,
            buff: Vec::new(),
            pos: 0,
            end_of_input: false,
        }
    }


    pub fn next(&mut self) -> Result<Option<Token>, io::Error> {

        if self.pos == self.buff.len() {
            self.refill()?;
            return self.next();
        }

        if self.end_of_input {
            return Ok(None);
        }

        let token = self.buff[self.pos].clone();
        self.pos += 1;
        Ok(Some(token.clone()))
    }

    fn refill(&mut self) -> Result<(), io::Error> {
        if let Some(line) = (self.input)()? {

            let mut lexer = TokenType::lexer(line);

            self.buff.clear();
            while let Some(token_type) = lexer.next() {
                let slice = lexer.slice();
                self.buff.push(Token::new(token_type, slice).clone())
            }
            self.pos = 0; //reset buffer pos
            return Ok(());
        }

        self.end_of_input = true;
        return Ok(());
    }
}