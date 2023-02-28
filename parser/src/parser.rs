use lexer::token::Token;
use lexer::token::TokenType::*;

use crate::aspects::binary_operation_parser::BinaryOperationsParser;
use crate::aspects::call_parser::CallParser;
use crate::aspects::group_parser::GroupParser;
use crate::aspects::literal_parser::LiteralParser;
use crate::aspects::redirection_parser::RedirectionParser;
use crate::aspects::var_declaration_parser::VarDeclarationParser;
use crate::ast::Expr;
use crate::cursor::ParserCursor;
use crate::moves::{bin_op, eod, eox, MoveOperations, next, of_types, spaces};

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

    /// Parses an expression or binary expression.
    pub(crate) fn expression(&mut self) -> ParseResult<Expr<'a>> {
        let expr = self.next_expression()?;
        self.parse_binary_expr(expr)
    }


    /// Parses a statement or binary expression.
    /// a statement is usually on a single line
    pub(crate) fn statement(&mut self) -> ParseResult<Expr<'a>> {
        let result = self.next_statement()?;
        self.parse_binary_expr(result)
    }

    /// Parses a value or binary expression
    pub(crate) fn value(&mut self) -> ParseResult<Expr<'a>> {
        let value = self.next_value()?;
        self.parse_binary_value_expr(value)
    }

    ///Parse the next statement
    pub(crate) fn next_statement(&mut self) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            Identifier | Quote | DoubleQuote => self.call(),
            Var | Val => self.var_declaration(),

            _ => self.next_expression(),
        }
    }

    ///Parse the next expression
    pub(crate) fn next_expression(&mut self) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            //if we are parsing an expression, then we want to see a parenthesised expr as a subshell expression
            RoundedLeftBracket => self.subshell(),
            _ => self.next_value(),
        }
    }

    ///Parse the next value
    pub(crate) fn next_value(&mut self) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            RoundedLeftBracket => self.parenthesis(),
            CurlyLeftBracket => self.block(),

            IntLiteral | FloatLiteral => self.literal(),
            Quote => self.string_literal(),
            DoubleQuote => self.templated_string_literal(),

            RoundedRightBracket | CurlyRightBracket => self.expected("unexpected closing brackets")?,

            _ => self.argument(),
        }
    }

    //parses any binary expression, considering given input expression
    //as the left arm of the expression.
    //if given expression is directly followed by an eox delimiter, then return it as is
    fn parse_binary_expr(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        self.cursor.advance(spaces()); //consume spaces

        //if there is an end of expression, it means that the expr is terminated so we return it here
        if self.cursor.lookahead(eox().or(eod())).is_some() {
            return Ok(expr);
        }

        if self.cursor.lookahead(of_types(&[Or, And])).is_some() {
            return self.binary_operation_right(expr, Parser::next_statement);
        }

        //now, we know that there is something right after the expression.
        //test if this 'something' is a redirection.
        if self.is_at_redirection_sign() {
            return self.redirectable(expr)
        }

        //else, we hit an invalid binary expression.
        self.expected(&format!("invalid infix operator, found '{}'", self.cursor.peek().value))
    }

    //parses any binary value expression, considering given input expression
    //as the left arm of the expression.
    //if given expression is directly followed by an eox delimiter, then return it as is
    fn parse_binary_value_expr(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        self.cursor.advance(spaces()); //consume spaces
        //if there is an end of expression, it means that the expr is terminated so we return it here
        if self.cursor.lookahead(eox().or(eod())).is_some() {
            return Ok(expr);
        }

        //now, we know that there is something right after the expression.
        //test if this 'something' is a redirection.
        if self.cursor
            .lookahead(bin_op()).is_some() {
            return self.binary_operation_right(expr, Parser::next_value);
        }

        //else, we hit an invalid binary expression.
        self.expected(&format!("invalid infix operator, found '{}'", self.cursor.peek().value))
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

    //traverse current expression and go to next expression
    fn repos_to_next_expr(&mut self) {
        while self.cursor.lookahead(eox()).is_none() {
            self.cursor.advance(next());
        }
    }

    /// Parses input tokens into an abstract syntax tree representation.
    pub(crate) fn parse(&mut self) -> ParseResult<Vec<Expr<'a>>> {
        let mut statements = Vec::new();

        while !self.cursor.is_at_end() {
            let statement = self.statement();

            if let Err(error) = &statement {
                self.report_error(error);
                self.repos_to_next_expr();
            }

            //consume end of expression
            self.cursor.force(eox(),
                              &format!("expected end of expression or file, found '{}'", self.cursor.peek().value))?;

            statements.push(statement?);
        }

        Ok(statements)
    }

    pub(crate) fn expected<T>(&self, message: &str) -> ParseResult<T> {
        Err(self.mk_parse_error(message))
    }

    pub(crate) fn mk_parse_error(&self, message: impl Into<String>) -> ParseError {
        ParseError {
            message: message.into(),
        }
    }
}
