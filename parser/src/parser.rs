use lexer::token::{Token, TokenType};
use lexer::token::TokenType::EndOfFile;

use crate::aspects::binary_operations_parser::BinaryOps;
use crate::aspects::call_parser::CallParser;
use crate::aspects::group_parser::GroupParser;
use crate::aspects::literal_parser::LiteralParser;
use crate::aspects::var_declaration_parser::VarDeclarationParser;
use crate::ast::Expr;
use crate::cursor::ParserCursor;
use crate::moves::{bin_op, eox, Move, MoveOperations, of_type, spaces};

pub type ParseResult<T> = Result<T, ParseError>;

/// An error that occurs during parsing.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    //pub actual: Token<'a>,
}

/// A parser for the Moshell scripting language.
pub(crate) struct Parser<'a> {
    pub(crate) cursor: ParserCursor<'a>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser.
    pub(crate) fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            cursor: ParserCursor::new(tokens),
        }
    }

    /// Parses an expression.
    pub(crate) fn expression(&mut self) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            TokenType::IntLiteral | TokenType::FloatLiteral => self.literal(),
            TokenType::Quote => self.string_literal(),
            TokenType::CurlyLeftBracket | TokenType::RoundedLeftBracket => self.group(),
            TokenType::DoubleQuote => self.templated_string_literal(),
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

    //simple error report system, should be enhanced in later PRs
    pub(crate) fn report_error(&self, err: &ParseError) {
        eprintln!("ERROR !");
        eprintln!("message: {}", err.message)
    }

    /// Parses input tokens into an abstract syntax tree representation.
    pub(crate) fn parse(&mut self) -> ParseResult<Vec<Expr<'a>>> {
        let mut statements = Vec::new();

        while !self.cursor.is_at_end() {
            let statement = self.parse_next(eox().or(of_type(EndOfFile)));
            if let Err(error) = &statement {
                self.report_error(error);
            }
            //consume end of expression
            self.cursor.force(eox().or(of_type(EndOfFile)), "expected end of expression or file")?;
            statements.push(statement?);
        }

        Ok(statements)
    }

    pub(crate) fn parse_next(&mut self, eox: impl Move + Copy) -> ParseResult<Expr<'a>> {
        let statement = self.statement()?;

        self.cursor.advance(spaces()); //consume spaces

        //expect end of expression (then the statement is directly pushed) OR if not present,
        // expect a binary operator to start a binary operation representation.
        let is_eox = self.cursor.lookahead(eox).is_some();

        if is_eox {
            return Ok(statement);
        }

        if self.cursor.lookahead(bin_op()).is_none() {
            return self.expected("wrong binary operator for left expression");
        }
        //consider the statement as left of the binary operator then parse the right branch.
        self.binary_operator_right(statement, eox)
    }

    pub(crate) fn expected<T>(&self, message: &str) -> ParseResult<T> {
        Err(self.mk_parse_error(message))
    }

    pub(crate) fn mk_parse_error(&self, message: impl Into<String>) -> ParseError {
        ParseError {
            message: message.into(),
            //actual: self.peek_token().clone(),
        }
    }
}
