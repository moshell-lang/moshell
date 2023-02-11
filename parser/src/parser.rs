

use lexer::token::{Token, TokenType};

use crate::aspects::base_parser::BaseParser;
use crate::aspects::var_declaration_parser::VarDeclarationParser;
use crate::aspects::call_parser::CallParser;
use crate::ast::{Expr, Literal, VarKind};

/// An error that occurs during parsing.
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    //pub actual: Token<'a>,
}

/// A parser for the Moshell scripting language.
pub struct Parser<'a> {
    /// The tokens to be parsed.
    pub(crate) tokens: Vec<Token<'a>>,
    /// The current position in the tokens.
    pub(crate) current: usize,
}


impl<'a> Parser<'a> {
    /// Parses an expression.
    pub fn expression(&mut self) -> Result<Expr<'a>, ParseError> {
        let token = self.next_token()?;
        match token.token_type {
            TokenType::Var => self.var_declaration(VarKind::Var),
            TokenType::Val => self.var_declaration(VarKind::Val),
            TokenType::IntLiteral => self.literal(token),
            //TODO add other expression parsers
            _ => self.call(),
        }
    }

    /// Creates a new parser.
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn literal(&mut self, token: Token<'a>) -> Result<Expr<'a>, ParseError> {
        Ok(Expr::Literal(Literal { value: token }))
    }

    /// Parses the tokens into an abstract syntax tree.
    pub fn parse(&mut self) -> Result<Vec<Expr<'a>>, ParseError> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.expression()?);
        }

        Ok(statements)
    }
}
