use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

/// An incorrectly placed delimiter.
///
/// It contains the byte offset of each delimiter found.
#[derive(Debug, Clone, PartialEq)]
pub struct UnmatchedDelimiter {
    /// The opening delimiter that was recorded.
    ///
    /// Will be `None` if only the closing delimiter was found.
    pub opening: Option<usize>,

    /// The delimiter that is not correct.
    ///
    /// Can be `None` if the closing delimiter is missing.
    pub candidate: Option<usize>,

    /// The correct closing delimiter that was eventually found afterwards.
    pub closing: Option<usize>,
}

/// An iterator over the tokens of a string.
pub(crate) struct TokenStream<'a> {
    /// The lexer that produces the tokens.
    lexer: Lexer<'a>,

    /// The stack of keep track of delimiter pairs.
    pub(crate) open_delimiters: Vec<Token<'a>>,

    /// The vector of unmatched delimiter errors.
    pub(crate) mismatches: Vec<UnmatchedDelimiter>,
}

impl<'a> TokenStream<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input),
            open_delimiters: Vec::new(),
            mismatches: Vec::new(),
        }
    }

    fn verify_pair(&mut self, token: Token<'a>) -> Token<'a> {
        match token.token_type {
            TokenType::SquaredLeftBracket
            | TokenType::RoundedLeftBracket
            | TokenType::CurlyLeftBracket => {
                self.open_delimiters.push(token.clone());
            }
            TokenType::SquaredRightBracket
            | TokenType::RoundedRightBracket
            | TokenType::CurlyRightBracket => {
                let offset = token.value.as_ptr() as usize - self.lexer.input.as_ptr() as usize;
                if let Some(open_delimiter) = self.open_delimiters.pop() {
                    let closing_pair = open_delimiter
                        .token_type
                        .closing_pair()
                        .expect("Invalid opening delimiter passed to the stack");
                    let open_offset =
                        open_delimiter.value.as_ptr() as usize - self.lexer.input.as_ptr() as usize;
                    if token.token_type == closing_pair {
                        if let Some(last) = self.mismatches.last_mut() {
                            if last.opening == Some(open_offset) && last.closing.is_none() {
                                last.closing = Some(offset);
                            }
                        }
                    } else {
                        self.mismatches.push(UnmatchedDelimiter {
                            opening: Some(open_offset),
                            candidate: Some(offset),
                            closing: None,
                        });
                        self.open_delimiters.push(open_delimiter);
                    }
                } else {
                    self.mismatches.push(UnmatchedDelimiter {
                        opening: None,
                        candidate: Some(offset),
                        closing: None,
                    });
                }
            }
            _ => {}
        }
        token
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            Some(token) => Some(self.verify_pair(token)),
            None => {
                while let Some(open_delimiter) = self.open_delimiters.pop() {
                    let offset =
                        open_delimiter.value.as_ptr() as usize - self.lexer.input.as_ptr() as usize;
                    self.mismatches.push(UnmatchedDelimiter {
                        opening: Some(offset),
                        candidate: None,
                        closing: None,
                    });
                }
                None
            }
        }
    }
}
