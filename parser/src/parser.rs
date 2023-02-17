use lexer::token::{Token, TokenType};

use crate::aspects::call_parser::CallParser;
use crate::aspects::literal_parser::LiteralParser;
use crate::aspects::var_declaration_parser::VarDeclarationParser;
use crate::ast::variable::VarKind;
use crate::cursor::ParserCursor;
use crate::ast::Expr;
use crate::moves::next;

pub type ParseResult<T> = Result<T, ParseError>;

/// An error that occurs during parsing.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    //pub actual: Token<'a>,
}

/// A parser for the Moshell scripting language.
pub(crate) struct Parser<'a> {
    cursor: ParserCursor<'a>,
}

impl<'a> Parser<'a> {
    pub(crate) fn cursor(&mut self) -> &mut ParserCursor<'a> {
        &mut self.cursor
    }

    /// Creates a new parser.
    pub(crate) fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            cursor: ParserCursor::new(tokens)
        }
    }

    /// Parses an expression.
    pub(crate) fn expression(&mut self) -> ParseResult<Expr<'a>> {
        match self.cursor().lookahead(next()).unwrap().token_type {
            TokenType::IntLiteral | TokenType::FloatLiteral => self.literal(),
            TokenType::Quote => self.string_literal(),
            TokenType::DoubleQuote => self.templated_string_literal(),
            _ => self.argument(),
        }
    }

    /// Parse a statement.
    ///
    /// Statements are usually on their own line.
    pub(crate) fn statement(&mut self) -> ParseResult<Expr<'a>> {
        match self.cursor().lookahead(next()).unwrap().token_type {
            TokenType::Identifier => self.call(),
            TokenType::Quote => self.call(),
            TokenType::DoubleQuote => self.call(),
            TokenType::Var => self.var_declaration(),
            TokenType::Val => self.var_declaration(),
            _ => self.expression(),
        }
    }

    /// Parses the tokens into an abstract syntax tree.
    pub(crate) fn parse(&mut self) -> ParseResult<Vec<Expr<'a>>> {
        let mut statements = Vec::new();

        while !self.cursor().is_at_end() {
            statements.push(self.statement()?);
        }

        Ok(statements)
    }


    pub fn expected<T>(&self, message: &str) -> ParseResult<T> {
        Err(self.mk_parse_error(message))
    }

    fn mk_parse_error(&self, message: impl Into<String>) -> ParseError {
        ParseError {
            message: message.into(),
            //actual: self.peek_token().clone(),
        }
    }

}
