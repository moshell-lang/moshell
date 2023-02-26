use lexer::token::{Token};
use lexer::token::TokenType::*;
use crate::aspects::binary_operation_parser::{BinaryOperationsParser};

use crate::aspects::call_parser::CallParser;
use crate::aspects::group_parser::GroupParser;
use crate::aspects::literal_parser::LiteralParser;
use crate::aspects::redirection_parser::RedirectionParser;
use crate::aspects::var_declaration_parser::VarDeclarationParser;
use crate::ast::Expr;
use crate::ast::operation::BinaryOperator;
use crate::context::{ParserContext};
use crate::cursor::ParserCursor;
use crate::moves::{custom_eox, eox, next, of_type, predicate, spaces};

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
    pub(crate) fn expression(&mut self, ctx: ParserContext) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            IntLiteral | FloatLiteral => self.literal(),
            Quote => self.string_literal(),
            CurlyLeftBracket | RoundedLeftBracket => self.group(ctx),
            DoubleQuote => self.templated_string_literal(),
            _ => self.argument(),
        }
    }

    /// Parse a statement.
    /// a statement is usually on a single line
    pub(crate) fn statement(&mut self, ctx: ParserContext) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            Identifier | Quote | DoubleQuote if !ctx.parsing_value => self.call(),
            Var | Val => self.var_declaration(ctx),

            _ => self.expression(ctx),
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
            let statement = self.parse_next(ParserContext::default());

            if let Err(error) = &statement {
                self.report_error(error);
                self.repos_to_next_expr();
            }

            //consume end of expression
            self.cursor.force(custom_eox(of_type(EndOfFile)), "expected end of expression or file")?;

            statements.push(statement?);
        }

        Ok(statements)
    }

    pub(crate) fn parse_next(&mut self,
                             ctx: ParserContext) -> ParseResult<Expr<'a>> {
        let statement = self.statement(ctx.clone())?;

        self.cursor.advance(spaces()); //consume spaces

        //test for end of expression
        let is_eox = match ctx.enclosing_end {
            Some(end_token) => self.cursor.lookahead(custom_eox(of_type(end_token))).is_some(),
            None => self.cursor.lookahead(eox()).is_some()
        };

        //we hit end of expression so the parsing ends here
        if is_eox {
            return Ok(statement);
        }

        //there's a token at cursors' current pos
        //let's try if we can continue to parse the expression as a left-handed expression.
        self.parse_next_right(statement, ctx)
    }

    fn parse_next_right(&mut self, left: Expr<'a>, ctx: ParserContext) -> ParseResult<Expr<'a>> {
        //can be a boolean operation expression
        if self.cursor
            .lookahead(predicate(|t| BinaryOperator::convert_bin_operator(t.token_type)
                .and_then(|t| Ok(ctx.allowed_operators.contains(&t)))
                .unwrap_or(false))).is_some() {
            return self.binary_operation_right(left, ctx);
        }

        //can be a std or pipe redirection
        if self.is_at_redirection_sign() {
            return self.redirectable(left);
        }


        self.expected("invalid infix operator")
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
