use std::fmt::Debug;
use context::poller::Poller;
use context::source::{Location, Source};
use lexer::token::Token;
use crate::err::{ErrorContext, ErrorReporter, ParseError, ParseErrorKind};

#[derive(Debug)]
pub struct ParserContext<'a, P: Poller<'a, Token<'a>> + Debug> {
    pub source: &'a Source,
    pub poller: P
}

impl<'a, P: Poller<'a, Token<'a>> + Debug> ParserContext<'a, P> {

    /// Get the relative byte offset of the given string in the source code.
    ///
    /// # Panics
    /// This method panics if the given string is not contained in the source code.
    pub fn relative_pos(&self, str: &str) -> Location {
        let start = (str.as_ptr() as usize)
            .checked_sub(self.source.source.as_ptr() as usize)
            .expect("String is not contained in the source code.");
        let end = start + str.len();
        start..end
    }

    /// Get the relative byte offset of the given context in the source code.
    ///
    /// # Panics
    /// This method panics if the given context is not contained in the source code.
    pub fn relative_pos_ctx(&self, context: impl Into<ErrorContext<'a>>) -> Location {
        let context = context.into();
        let start = (context.from.as_ptr() as usize)
            .checked_sub(self.source.source.as_ptr() as usize)
            .expect("Context start is not contained in the source code.");
        let end = context.to.as_ptr() as usize + context.to.len() as usize
            - self.source.source.as_ptr() as usize;
        start..end
    }
}


impl<'a, P: Poller<'a, Token<'a>> + Debug> ErrorReporter for ParserContext<'a, P> {
    fn mk_parse_error<'b>(
        &self,
        message: impl Into<String>,
        context: impl Into<ErrorContext<'b>>,
        kind: ParseErrorKind,
    ) -> ParseError {
        ParseError {
            message: message.into(),
            position: self.relative_pos_ctx(context),
            kind,
        }
    }
}
