use crate::err::ParseErrorKind;
use crate::moves::{eod, eox, of_type, word_seps, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::structure::Construct;
use ast::Expr;
use lexer::token::TokenType;

pub trait StructureAspect<'a> {
    /// Parses a structure constructor call.
    fn constructor(&mut self) -> ParseResult<Expr<'a>>;

    /// Checks if the cursor is at the start of a constructor.
    fn is_at_constructor_start(&self) -> bool;
}

impl<'a> StructureAspect<'a> for Parser<'a> {
    fn constructor(&mut self) -> ParseResult<Expr<'a>> {
        let name = self
            .cursor
            .force(of_type(TokenType::Identifier), "Excepted structure name.")?;
        let open_parenthesis = self.cursor.force(
            of_type(TokenType::RoundedLeftBracket),
            "Expected opening parenthesis.",
        )?;
        self.delimiter_stack.push_back(open_parenthesis.clone());

        // Read the args until a closing delimiter or a new non-escaped line is found.
        let mut args = vec![];
        loop {
            if self
                .cursor
                .advance(word_seps().then(of_type(TokenType::RoundedRightBracket)))
                .is_some()
            {
                self.delimiter_stack.pop_back();
                return Ok(Expr::Construct(Construct {
                    name: name.value,
                    args,
                }));
            }
            args.push(self.next_value()?);
            self.cursor.advance(word_seps());

            // Check if the constructor is abnormally terminated.
            if self.cursor.lookahead(eox()).is_some() {
                self.expected(
                    "Expected closing parenthesis.",
                    ParseErrorKind::Unpaired(self.cursor.relative_pos(open_parenthesis.clone())),
                )?;
            }
            if self.cursor.lookahead(eod()).is_some() {
                self.expect_delimiter(TokenType::RoundedRightBracket)?;
                break;
            }
        }
        Ok(Expr::Construct(Construct {
            name: name.value,
            args,
        }))
    }

    fn is_at_constructor_start(&self) -> bool {
        self.cursor
            .lookahead(
                of_type(TokenType::Identifier).and_then(of_type(TokenType::RoundedLeftBracket)),
            )
            .is_some()
    }
}
