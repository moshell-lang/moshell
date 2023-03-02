use context::source::Source;
use lexer::lexer::lex;
use lexer::token::Token;
use lexer::token::TokenType::*;

use crate::aspects::binary_operation::BinaryOperationsAspect;
use crate::aspects::call::CallAspect;
use crate::aspects::group::GroupAspect;
use crate::aspects::literal::LiteralAspect;
use crate::aspects::redirection::RedirectionAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use crate::ast::Expr;
use crate::cursor::ParserCursor;
use crate::err::{ParseError, ParseErrorKind};
use crate::moves::{bin_op, eod, eox, next, of_types, spaces, MoveOperations};

pub(crate) type ParseResult<T> = Result<T, ParseError>;

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

    /// Parses input tokens into an abstract syntax tree representation.
    pub fn parse(&mut self) -> ParseResult<Vec<Expr<'a>>> {
        let mut statements = Vec::new();
        let mut last_error = None;

        while !self.cursor.is_at_end() {
            match self.parse_next() {
                Err(error) => {
                    self.repos_to_next_expr();
                    last_error = Some(error);
                }
                Ok(statement) => statements.push(statement),
            }
        }

        if let Some(error) = last_error {
            Err(error)
        } else {
            Ok(statements)
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
            RoundedLeftBracket => Ok(Expr::Subshell(self.subshell()?)),
            _ => self.next_value(),
        }
    }

    ///Parse the next value
    pub(crate) fn next_value(&mut self) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            RoundedLeftBracket => Ok(Expr::Parenthesis(self.parenthesis()?)),
            CurlyLeftBracket => Ok(Expr::Block(self.block()?)),

            IntLiteral | FloatLiteral => self.literal(),
            Quote => self.string_literal(),
            DoubleQuote => self.templated_string_literal(),

            _ if pivot.is_closing_ponctuation() => {
                self.expected("Unexpected closing bracket.", ParseErrorKind::Unexpected)
            }
            _ => self.argument(),
        }
    }

    pub(crate) fn parse_next(&mut self) -> ParseResult<Expr<'a>> {
        let statement = self.statement();
        if statement.is_ok() {
            //consume end of expression
            self.cursor
                .force(eox(), "expected end of expression or file")?;
        };
        statement
    }

    pub(crate) fn expected<T>(&self, message: &str, kind: ParseErrorKind) -> ParseResult<T> {
        Err(self.mk_parse_error(message, self.cursor.peek(), kind))
    }

    pub(crate) fn mk_parse_error(
        &self,
        message: impl Into<String>,
        erroneous_token: Token,
        kind: ParseErrorKind,
    ) -> ParseError {
        self.cursor.mk_parse_error(message, erroneous_token, kind)
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
            return self.redirectable(expr);
        }

        //else, we hit an invalid binary expression.
        self.expected("invalid infix operator", ParseErrorKind::Unexpected)
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
        if self.cursor.lookahead(bin_op()).is_some() {
            return self.binary_operation_right(expr, Parser::next_value);
        }

        //else, we hit an invalid binary expression.
        self.expected("invalid infix operator", ParseErrorKind::Unexpected)
    }

    //Skips spaces and verify that this parser is not parsing the end of an expression
    // (unescaped newline or semicolon)
    fn repos(&mut self) -> ParseResult<()> {
        self.cursor.advance(spaces()); //skip spaces
        if self.cursor.lookahead(eox()).is_some() {
            return self.expected("Unexpected end of expression", ParseErrorKind::Unexpected);
        }
        Ok(())
    }

    //traverse current expression and go to next expression
    fn repos_to_next_expr(&mut self) {
        while self.cursor.lookahead(eox()).is_none() {
            self.cursor.advance(next());
        }
    }
}
