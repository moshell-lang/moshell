use lexer::token::{Token, TokenType};
use lexer::token::TokenType::*;

use crate::aspects::binary_operation::BinaryOperationsAspect;
use crate::aspects::call::CallAspect;
use crate::aspects::group::GroupAspect;
use crate::aspects::if_else::IfElseAspect;
use crate::aspects::literal::LiteralAspect;
use crate::aspects::redirection::RedirectionAspect;
use crate::aspects::test::TestAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use crate::ast::Expr;
use crate::cursor::ParserCursor;
use crate::moves::{bin_op, eod, eox, next, of_types, spaces, MoveOperations, repeat, space, like};

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
    /// Parses input tokens into an abstract syntax tree representation.
    pub fn parse(&mut self) -> ParseResult<Vec<Expr<'a>>> {
        let mut statements = Vec::new();
        let mut last_error = None;

        while self.look_for_input() {
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

    fn look_for_input(&mut self) -> bool {
        self.cursor.advance(repeat(space().or(eox())));

        !self.cursor.is_at_end()
    }

    /// Creates a new parser.
    pub(crate) fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            cursor: ParserCursor::new(tokens),
        }
    }

    /// Parses a statement or binary expression.
    /// a statement is usually on a single line
    pub(crate) fn statement(&mut self) -> ParseResult<Expr<'a>> {
        let result = self.next_statement()?;
        self.parse_binary_expr(result)
    }

    /// Parses an expression-statement or binary expression.
    pub(crate) fn expression_statement(&mut self) -> ParseResult<Expr<'a>> {
        let expr = self.next_expression_statement()?;
        self.parse_binary_expr(expr)
    }

    /// Parses an expression or binary expression.
    pub(crate) fn expression(&mut self) -> ParseResult<Expr<'a>> {
        let expr = self.next_expression()?;
        self.parse_binary_expr(expr)
    }

    /// Parses a value or binary expression
    pub(crate) fn value(&mut self) -> ParseResult<Expr<'a>> {
        let value = self.next_value()?;
        //values needs a different handling of right-handed binary expressions
        self.parse_binary_value_expr(value)
    }

    ///Parse the next statement
    pub(crate) fn next_statement(&mut self) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            Var | Val => self.var_declaration(),

            _ => self.next_expression_statement(),
        }
    }


    ///Parse the next expression
    pub(crate) fn next_expression_statement(&mut self) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            If => self.parse_if(Parser::statement),
            Identifier | Quote | DoubleQuote => self.call(),

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
            SquaredLeftBracket => self.parse_test(),

            Not => self.not(Parser::next_expression_statement),


            _ => self.next_value(),
        }
    }

    ///Parse the next value expression
    pub(crate) fn next_value(&mut self) -> ParseResult<Expr<'a>> {
        self.repos()?;

        let token = self.cursor.peek();
        let pivot = token.token_type;
        match pivot {
            RoundedLeftBracket => Ok(Expr::Parenthesis(self.parenthesis()?)),
            CurlyLeftBracket => Ok(Expr::Block(self.block()?)),
            //test expressions has nothing to do in a value expression.
            SquaredLeftBracket => self.expected("Unexpected start of test expression"),

            Not => self.not(Parser::next_value),

            IntLiteral | FloatLiteral => self.literal(),
            Quote => self.string_literal(),
            DoubleQuote => self.templated_string_literal(),

            If => self.parse_if(Parser::value),

            _ if pivot.is_keyword() => self.expected(&format!("Unexpected keyword '{}'", token.value)),
            _ if pivot.is_ponctuation() => self.expected(&format!("Unexpected token '{}'.", token.value)),
            _ => self.argument(),
        }
    }

    pub(crate) fn parse_next(&mut self) -> ParseResult<Expr<'a>> {
        let statement = self.statement();
        if statement.is_ok() {
            //consume end of expression
            self.cursor.force(
                eox(),
                &format!(
                    "expected end of expression or file, found '{}'",
                    self.cursor.peek().value
                ),
            )?;
        };
        statement
    }

    pub(crate) fn expected<T>(&self, message: &str) -> ParseResult<T> {
        Err(self.mk_parse_error(message))
    }

    pub(crate) fn mk_parse_error(&self, message: impl Into<String>) -> ParseError {
        ParseError {
            message: message.into(),
        }
    }

    //parses any binary expression, considering given input expression
    //as the left arm of the expression.
    //if given expression is directly followed by an eox delimiter, then return it as is
    fn parse_binary_expr(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        self.cursor.advance(spaces()); //consume spaces

        //if there is an end of expression, it means that the expr is terminated so we return it here
        //any keyword would also stop this expression.
        if self.cursor.lookahead(eox().or(eod()).or(like(TokenType::is_keyword))).is_some() {
            return Ok(expr);
        }

        if self.cursor.lookahead(of_types(&[Or, And])).is_some() {
            return self.binary_operation_right(expr, Parser::next_expression_statement);
        }

        //now, we know that there is something right after the expression.
        //test if this 'something' is a redirection.
        if self.is_at_redirection_sign() {
            return self.redirectable(expr);
        }


        //else, we hit an invalid binary expression.
        self.expected(&format!(
            "invalid infix operator, found '{}'",
            self.cursor.peek().value
        ))
    }

    //parses any binary value expression, considering given input expression
    //as the left arm of the expression.
    //if given expression is directly followed by an eox delimiter, then return it as is
    fn parse_binary_value_expr(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        self.cursor.advance(spaces()); //consume spaces

        //if there is an end of expression, it means that the expr is terminated so we return it here
        //any keyword would also stop this expression.
        if self.cursor.lookahead(eox().or(eod()).or(like(TokenType::is_keyword))).is_some() {
            return Ok(expr);
        }

        //now, we know that there is something right after the expression.
        //test if this 'something' is a redirection.
        if self.cursor.lookahead(bin_op()).is_some() {
            return self.binary_operation_right(expr, Parser::next_value);
        }

        //else, we hit an invalid binary expression.
        self.expected(&format!(
            "invalid infix operator, found '{}'",
            self.cursor.peek().value
        ))
    }

    //Skips spaces and verify that this parser is not parsing the end of an expression
    // (unescaped newline or semicolon)
    fn repos(&mut self) -> ParseResult<()> {
        self.cursor.advance(spaces()); //skip spaces
        if self.cursor.lookahead(eox()).is_some() {
            return self.expected("Unexpected end of expression");
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
