use logos::Logos;
use crate::token::{Token, TokenType};

use context::poller::Poller;


pub struct BufferedTokenReader<'a, I> {
    input: I,
    buff: Vec<Token<'a>>,
    pos: usize,
    end_of_input: bool,
}

impl<'a, I> BufferedTokenReader<'a, I> {
    fn is_at_end(&self) -> bool {
        self.end_of_input
    }
}

///implementation for a line supplier input
impl<'a, I> BufferedTokenReader<'a, I>
    where I: Poller<'a, &'a str>
{

    pub fn new(input: I) -> Self {
        Self {
            input,
            buff: Vec::new(),
            pos: 0,
            end_of_input: false
        }
    }

    fn next(&mut self) -> Result<Option<Token<'a>>, I::Error> {
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

    fn refill(&mut self) -> Result<(), I::Error> {
        if let Some(line) = self.input.next()? {
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