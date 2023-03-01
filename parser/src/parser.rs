use crate::aspects::block_parser::BlockParser;
use lexer::lexer::lex;
use lexer::token::{Token, TokenType};

use crate::aspects::call_parser::CallParser;
use crate::aspects::literal_parser::LiteralParser;
use crate::aspects::var_declaration_parser::VarDeclarationParser;
use crate::ast::Expr;
use crate::cursor::ParserCursor;
use crate::moves::{eox, space, spaces, MoveOperations};
use crate::source::{Location, Source};

pub type ParseResult<T> = Result<T, ParseError>;

/// An error that occurs during parsing.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub position: Location,
}

/// A parser for the Moshell scripting language.
pub(crate) struct Parser<'a> {
    pub(crate) cursor: ParserCursor<'a>,
    pub(crate) source: Source<'a>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from a defined source.
    pub(crate) fn new(source: Source<'a>) -> Self {
        Self {
            cursor: ParserCursor::new_with_source(lex(source.source), source.source),
            source,
        }
    }

    /// Parses an expression.
    pub(crate) fn expression(&mut self) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            TokenType::IntLiteral | TokenType::FloatLiteral => self.literal(),
            TokenType::Quote => self.string_literal(),
            TokenType::CurlyLeftBracket => self.block(),
            TokenType::DoubleQuote => self.templated_string_literal(),
            _ if pivot.is_closing_ponctuation() => self.expected("Unexpected closing bracket."),
            _ => self.argument(),
        }
    }

    /// Parse a statement.
    /// a statement is usually on a single line
    pub(crate) fn statement(&mut self) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            TokenType::Identifier => self.call(),
            TokenType::Quote => self.call(),
            TokenType::DoubleQuote => self.call(),
            TokenType::Var => self.var_declaration(),
            TokenType::Val => self.var_declaration(),
            _ => self.expression(),
        }
    }

    ///Skips spaces and verify that this parser is not parsing the end of an expression
    /// (unescaped newline or semicolon)
    fn repos(&mut self) -> ParseResult<()> {
        self.cursor.advance(spaces()); //skip spaces
        if self.cursor.lookahead(eox()).is_some() {
            return self.expected("Unexpected end of expression");
        }
        Ok(())
    }

    /// Parses the tokens into an abstract syntax tree.
    pub(crate) fn parse(&mut self) -> ParseResult<Vec<Expr<'a>>> {
        let mut statements = Vec::new();

        while !self.cursor.is_at_end() {
            statements.push(self.statement()?);
            self.cursor.advance(space().then(eox()));
        }

        Ok(statements)
    }

    pub(crate) fn expected<T>(&self, message: &str) -> ParseResult<T> {
        Err(self.mk_parse_error(message, self.cursor.peek()))
    }

    pub(crate) fn mk_parse_error(
        &self,
        message: impl Into<String>,
        erroneous_token: Token,
    ) -> ParseError {
        self.cursor.mk_parse_error(message, erroneous_token)
    }
}
