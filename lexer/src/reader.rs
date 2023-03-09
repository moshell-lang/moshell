
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

///implementation for a line supplier input
impl<'a, S, E> BufferedTokenReader<'a, S>
    where
        S: FnMut() -> Result<Option<&'a str>, E>
{

    pub fn from_line_supplier(supplier: S) -> Self {
        Self {
            input: supplier,
            buff: Vec::new(),
            pos: 0,
            end_of_input: false
        }
    }

    pub fn next(&mut self) -> Result<Option<Token>, E> {
        if self.end_of_input {
            return Ok(None);
        }

        if self.pos == self.buff.len() {
            self.refill()?;
            return self.next();
        }

        let token = self.buff[self.pos].clone();
        self.pos += 1;
        Ok(Some(token.clone()))
    }

    fn refill(&mut self) -> Result<(), E> {
        if let Some(line) = (self.input)()? {

            let mut lexer = TokenType::lexer(line);

            self.buff.clear();
            while let Some(token_type) = lexer.next() {
                self.buff.push(Token::new(token_type, lexer.slice()))
            }
            self.pos = 0; //reset buffer pos
            return Ok(());
        }

        self.end_of_input = true;
        return Ok(());
    }
}
