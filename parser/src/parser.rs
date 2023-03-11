use context::source::Source;
use lexer::lexer::lex;
use lexer::token::TokenType::*;
use lexer::token::{Token, TokenType};
use std::collections::vec_deque::VecDeque;

use crate::aspects::assign::AssignAspect;
use crate::aspects::binary_operation::BinaryOperationsAspect;
use crate::aspects::call::CallAspect;
use crate::aspects::detached::DetachedAspect;
use crate::aspects::group::GroupAspect;
use crate::aspects::if_else::IfElseAspect;
use crate::aspects::literal::LiteralAspect;
use crate::aspects::r#loop::LoopAspect;
use crate::aspects::r#match::MatchAspect;
use crate::aspects::r#use::UseAspect;
use crate::aspects::redirection::RedirectionAspect;
use crate::aspects::structure::StructureAspect;
use crate::aspects::test::TestAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use ast::Expr;
use crate::cursor::ParserCursor;
use crate::err::ParseErrorKind::Unexpected;
use crate::err::{ErrorContext, ParseError, ParseErrorKind, ParseReport};
use crate::moves::{
    bin_op, eod, eox, like, next, of_type, of_types, repeat, spaces, word_seps, MoveOperations,
};

pub(crate) type ParseResult<T> = Result<T, ParseError>;

/// A parser for the Moshell scripting language.
pub(crate) struct Parser<'a> {
    pub(crate) cursor: ParserCursor<'a>,
    pub(crate) source: Source<'a>,
    pub(crate) delimiter_stack: VecDeque<Token<'a>>,
}

//all tokens that can't be an infix operator
macro_rules! non_infix {
    () => {
        eox()
            .or(eod())
            .or(like(TokenType::is_keyword))
            .or(of_types(&[
                RoundedLeftBracket,
                CurlyLeftBracket,
                SquaredLeftBracket,
                Bar,
                FatArrow,
            ]))
    };
}

impl<'a> Parser<'a> {
    /// Creates a new parser from a defined source.
    pub(crate) fn new(source: Source<'a>) -> Self {
        Self {
            cursor: ParserCursor::new_with_source(lex(source.source), source.source),
            source,
            delimiter_stack: VecDeque::new(),
        }
    }

    /// Parses input tokens into an abstract syntax tree representation.
    pub fn parse(&mut self) -> ParseReport<'a> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while self.look_for_input() {
            match self.parse_next() {
                Err(error) => {
                    self.repos_to_next_expr();
                    errors.push(error);
                }
                Ok(statement) => statements.push(statement),
            }
        }

        ParseReport {
            expr: statements,
            errors,
            stack_ended: self.delimiter_stack.is_empty(),
        }
    }

    fn look_for_input(&mut self) -> bool {
        self.cursor.advance(repeat(spaces().or(eox())));

        !self.cursor.is_at_end()
    }

    #[inline]
    pub(crate) fn parse_full_expr<P>(&mut self, mut next: P) -> ParseResult<Expr<'a>>
    where
        P: FnMut(&mut Self) -> ParseResult<Expr<'a>>,
    {
        let expr = next(self)?;
        let expr = self.parse_binary_expr(expr)?;
        self.parse_detached(expr)
    }

    /// Parses a statement or binary expression.
    /// a statement is usually on a single line
    pub(crate) fn statement(&mut self) -> ParseResult<Expr<'a>> {
        self.parse_full_expr(Parser::next_statement)
    }

    /// Parses an expression-statement or binary expression.
    pub(crate) fn expression_statement(&mut self) -> ParseResult<Expr<'a>> {
        self.parse_full_expr(Parser::next_expression_statement)
    }

    /// Parses an expression or binary expression.
    pub(crate) fn expression(&mut self) -> ParseResult<Expr<'a>> {
        self.parse_full_expr(Parser::next_expression)
    }

    /// Parses a value or binary expression
    pub(crate) fn value(&mut self) -> ParseResult<Expr<'a>> {
        let value = self.next_value()?;
        //values needs a different handling of right-handed binary expressions
        let value = self.parse_binary_value_expr(value)?;
        self.parse_detached(value)
    }

    ///Parse the next statement
    pub(crate) fn next_statement(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected statement")?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            Var | Val => self.var_declaration(),
            Use => self.parse_use(),

            While => self.parse_while().map(Expr::While),
            Loop => self.parse_loop().map(Expr::Loop),
            Identifier
                if self
                    .cursor
                    .lookahead(next().then(word_seps().then(of_type(Equal))))
                    .is_some() =>
            {
                self.parse_assign().map(Expr::Assign)
            }

            _ => self.next_expression_statement(),
        }
    }

    ///Parse the next expression
    pub(crate) fn next_expression_statement(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected expression statement")?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            If => self.parse_if(Parser::statement).map(Expr::If),
            Match => self.parse_match(Parser::statement).map(Expr::Match),
            Identifier if self.is_at_constructor_start() => self.constructor(),
            Identifier | Quote | DoubleQuote => self.call(),

            _ if pivot.is_bin_operator() => self.call(),

            _ => self.next_expression(),
        }
    }

    ///Parse the next expression
    pub(crate) fn next_expression(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected expression")?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            //if we are parsing an expression, then we want to see a parenthesised expr as a subshell expression
            RoundedLeftBracket => self.subshell().map(Expr::Subshell),
            SquaredLeftBracket => self.parse_test(),

            Continue => {
                self.cursor.next()?;
                Ok(Expr::Continue)
            }
            Break => {
                self.cursor.next()?;
                Ok(Expr::Break)
            }

            Not => self.not(Parser::next_expression_statement),

            _ => self.next_value(),
        }
    }

    ///Parse the next value expression
    pub(crate) fn next_value(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected value")?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            RoundedLeftBracket => self.parenthesis().map(Expr::Parenthesis),
            CurlyLeftBracket => self.block().map(Expr::Block),
            Not => self.not(Parser::next_value),

            //expression that can also be used as values
            If => self.parse_if(Parser::value).map(Expr::If),
            Match => self.parse_match(Parser::value).map(Expr::Match),
            Identifier if self.is_at_constructor_start() => self.constructor(),

            //test expressions has nothing to do in a value expression.
            SquaredLeftBracket => self.expected("Unexpected start of test expression", Unexpected),
            _ => self.literal(),
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

    /// Raise an error on the current token.
    ///
    /// Use [Parser::expected_with] if the error is not on the current token.
    pub(crate) fn expected<T>(&self, message: &str, kind: ParseErrorKind) -> ParseResult<T> {
        Err(self.mk_parse_error(message, self.cursor.peek(), kind))
    }

    /// Raise an error with a specific context.
    ///
    /// The context is used to better display where the error is.
    /// A context can be a single token or a range of tokens.
    pub(crate) fn expected_with<T>(
        &self,
        message: &str,
        context: impl Into<ErrorContext<'a>>,
        kind: ParseErrorKind,
    ) -> ParseResult<T> {
        Err(self.mk_parse_error(message, context, kind))
    }

    /// Expect a specific delimiter token type and pop it from the delimiter stack.
    pub(crate) fn expect_delimiter(&mut self, eog: TokenType) -> ParseResult<()> {
        if self.cursor.advance(of_type(eog)).is_some() {
            self.delimiter_stack.pop_back();
            Ok(())
        } else {
            self.mismatched_delimiter(eog)
        }
    }

    /// Raise a mismatched delimiter error on the current token.
    pub(crate) fn mismatched_delimiter(&mut self, eog: TokenType) -> ParseResult<()> {
        if let Some(last) = self.delimiter_stack.back() {
            self.expected(
                "Mismatched closing delimiter.",
                ParseErrorKind::Unpaired(self.cursor.relative_pos(last)),
            )
        } else {
            self.expected(
                "Unexpected closing delimiter.",
                ParseErrorKind::Excepted(eog.str().unwrap_or("specific token")),
            )
        }
    }

    pub(crate) fn mk_parse_error(
        &self,
        message: impl Into<String>,
        context: impl Into<ErrorContext<'a>>,
        kind: ParseErrorKind,
    ) -> ParseError {
        self.cursor.mk_parse_error(message, context, kind)
    }

    //parses any binary expression, considering given input expression
    //as the left arm of the expression.
    //if given expression is directly followed by an eox delimiter, then return it as is
    fn parse_binary_expr(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        self.cursor.advance(spaces()); //consume spaces

        //if there is an end of expression, it means that the expr is terminated so we return it here
        //any keyword would also stop this expression.
        if self.cursor.lookahead(non_infix!()).is_some() {
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

        if let Expr::Literal(literal) = &expr {
            if self.cursor.lookahead(bin_op()).is_some() {
                let start_pos = self.cursor.relative_pos(literal.lexeme).start;
                if self
                    .binary_operation_right(expr, Parser::next_value)
                    .is_ok()
                {
                    let end_pos = self.cursor.relative_pos(&self.cursor.peek()).end;
                    let slice = &self.source.source[start_pos..end_pos];
                    return self.expected_with(
                        "Binary operations must be enclosed in a value expression.",
                        slice,
                        ParseErrorKind::UnexpectedInContext(format!("$(( {} ))", slice)),
                    );
                }
            }
        }

        //else, we hit an invalid binary expression.
        self.expected("invalid expression operator", Unexpected)
    }

    //parses any binary value expression, considering given input expression
    //as the left arm of the expression.
    //if given expression is directly followed by an eox delimiter, then return it as is
    fn parse_binary_value_expr(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        self.cursor.advance(word_seps()); //consume word separators

        //if there is an end of expression, it means that the expr is terminated so we return it here
        //any keyword would also stop this expression.
        if self.cursor.lookahead(non_infix!()).is_some() {
            return Ok(expr);
        }

        //now, we know that there is something right after the expression.
        //test if this 'something' is a redirection.
        if self.cursor.lookahead(bin_op()).is_some() {
            return self.binary_operation_right(expr, Parser::next_value);
        }

        //else, we hit an invalid binary expression.
        self.expected("invalid infix operator", Unexpected)
    }

    ///Skips spaces and verify that this parser is not parsing the end of an expression
    /// (unescaped newline or semicolon)
    pub(crate) fn repos(&mut self, message: &str) -> ParseResult<()> {
        self.cursor.advance(word_seps()); //skip word separators
        if self.cursor.lookahead(eox()).is_some() {
            return self.expected(message, Unexpected);
        }
        Ok(())
    }

    //traverse current expression and go to next expression
    fn repos_to_next_expr(&mut self) {
        while !self.cursor.is_at_end() {
            if let Some(last) = self.delimiter_stack.back() {
                if let Some(token) = self.cursor.advance(next()) {
                    if last
                        .token_type
                        .closing_pair()
                        .expect("invalid delimiter passed to stack")
                        == token.token_type
                    {
                        self.delimiter_stack.pop_back();
                    }
                }
            } else if self.cursor.lookahead(eox()).is_none() {
                self.cursor.advance(next());
            } else {
                break;
            }
        }
    }
}
