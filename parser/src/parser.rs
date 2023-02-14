use crate::aspects::base_parser::BaseParser;
use lexer::token::{Token, TokenType};

use crate::aspects::call_parser::CallParser;
use crate::aspects::literal_parser::LiteralParser;
use crate::aspects::var_declaration_parser::VarDeclarationParser;
use crate::ast::Expr;
use crate::ast::variable::VarKind;

pub type ParseResult<T> = Result<T, ParseError>;

/// An error that occurs during parsing.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    //pub actual: Token<'a>,
}

/// A parser for the Moshell scripting language.
pub(crate) struct Parser<'a> {
    /// The tokens to be parsed.
    pub(crate) tokens: Vec<Token<'a>>,
    /// The current position in the tokens.
    pub(crate) current: usize,
}

impl<'a> Parser<'a> {
    /// Creates a new parser.
    pub(crate) fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, current: 0 }
    }

    /// Parses an expression.
    pub(crate) fn expression(&mut self) -> ParseResult<Expr<'a>> {
        match self.peek_token().token_type {
            TokenType::Var => self.var_declaration(VarKind::Var),
            TokenType::Val => self.var_declaration(VarKind::Val),
            TokenType::IntLiteral | TokenType::FloatLiteral => self.literal(),
            TokenType::Quote => self.string_literal(),
            //TODO add other expression parsers
            _ => self.call(),
        }
    }

    /// Parses the tokens into an abstract syntax tree.
    pub(crate) fn parse(&mut self) -> ParseResult<Vec<Expr<'a>>> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.expression()?);
        }

        Ok(statements)
    }
}
