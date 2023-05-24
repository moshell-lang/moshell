use context::source::Source;
use lexer::lexer::lex;
use lexer::token::TokenType::*;
use lexer::token::{Token, TokenType};
use std::collections::vec_deque::VecDeque;

use crate::aspects::assign::AssignAspect;
use crate::aspects::binary_operation::BinaryOperationsAspect;
use crate::aspects::call::CallAspect;
use crate::aspects::detached::DetachedAspect;
use crate::aspects::function_declaration::FunctionDeclarationAspect;
use crate::aspects::group::GroupAspect;
use crate::aspects::if_else::IfElseAspect;
use crate::aspects::lambda_def::LambdaDefinitionAspect;
use crate::aspects::literal::{LiteralAspect, LiteralLeniency};
use crate::aspects::modules::ModulesAspect;
use crate::aspects::r#loop::LoopAspect;
use crate::aspects::r#match::MatchAspect;
use crate::aspects::r#type::TypeAspect;
use crate::aspects::range::RangeAspect;
use crate::aspects::redirection::RedirectionAspect;
use crate::aspects::test::TestAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use crate::cursor::ParserCursor;
use crate::err::ParseErrorKind::Unexpected;
use crate::err::{ErrorContext, ParseError, ParseErrorKind, ParseReport};
use crate::moves::{
    any, bin_op, blanks, eod, eox, like, next, of_type, of_types, repeat, spaces, Move,
    MoveOperations,
};
use ast::range::Iterable;
use ast::Expr;

pub(crate) type ParseResult<T> = Result<T, ParseError>;

/// A parser for the Moshell scripting language.
pub(crate) struct Parser<'a> {
    pub(crate) cursor: ParserCursor<'a>,
    pub(crate) source: Source<'a>,
    pub(crate) delimiter_stack: VecDeque<Token<'a>>,
    errors: Vec<ParseError>,
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
                Comma,
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
            errors: Vec::new(),
        }
    }

    /// Parses input tokens into an abstract syntax tree representation.
    pub fn parse(mut self) -> ParseReport<'a> {
        let mut statements = Vec::new();

        while self.look_for_input() {
            match self.parse_next() {
                Err(error) => {
                    self.recover_from(error, eox());
                }
                Ok(statement) => statements.push(statement),
            }
        }

        ParseReport {
            expr: statements,
            errors: self.errors,
            delimiter_stack: self
                .delimiter_stack
                .into_iter()
                .map(|t| t.token_type)
                .collect(),
        }
    }

    /// Parses a statement with a given parser function.
    ///
    /// This function is intended to be only used in tests.
    pub(crate) fn parse_specific<E>(
        mut self,
        mut next: impl FnMut(&mut Self) -> ParseResult<E>,
    ) -> ParseResult<E> {
        match next(&mut self) {
            Ok(statement) => {
                if let Some(err) = self.errors.pop() {
                    if !self.errors.is_empty() {
                        panic!("There is more than one error in the parser's error stack.");
                    }
                    Err(err)
                } else {
                    Ok(statement)
                }
            }
            Err(err) => {
                if !self.errors.is_empty() {
                    panic!("There is more more than one error in the parser's error stack.");
                }
                Err(err)
            }
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
            Fun => self
                .parse_function_declaration()
                .map(Expr::FunctionDeclaration),

            While => self.parse_while().map(Expr::While),
            Loop => self.parse_loop().map(Expr::Loop),
            For => self.parse_for().map(Expr::For),
            Identifier
                if self
                    .cursor
                    .lookahead(next().then(spaces().then(of_type(Equal))))
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
        let expr = match pivot {
            If => self.parse_if(Parser::statement).map(Expr::If),
            Match => self.parse_match(Parser::statement).map(Expr::Match),
            Identifier | Quote | DoubleQuote => self.any_call(),

            _ if pivot.is_bin_operator() => self.call(),

            _ => self.next_expression(),
        }?;
        self.expand_call_chain(expr)
    }

    ///Parse the next expression
    pub(crate) fn next_expression(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected expression")?;

        let pivot = self.cursor.peek().token_type;
        match pivot {
            //if we are parsing an expression, then we want to see a parenthesised expr as a subshell expression
            RoundedLeftBracket => self.subshell_or_parentheses(),
            SquaredLeftBracket => self.parse_test(),

            Continue => {
                let current = self.cursor.next()?;
                Ok(Expr::Continue(self.cursor.relative_pos(current.value)))
            }
            Break => {
                let current = self.cursor.next()?;
                Ok(Expr::Break(self.cursor.relative_pos(current.value)))
            }
            Return => self.parse_return().map(Expr::Return),

            Not => self.not(Parser::next_expression_statement),

            _ => self.next_value(),
        }
    }

    ///Parse the next value expression
    pub(crate) fn next_value(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected value")?;

        let pivot = self.cursor.peek().token_type;
        let value = match pivot {
            RoundedLeftBracket => self.lambda_or_parentheses(),
            CurlyLeftBracket => self.block().map(Expr::Block),
            Not => self.not(Parser::next_value),

            //expression that can also be used as values
            If => self.parse_if(Parser::value).map(Expr::If),
            Match => self.parse_match(Parser::value).map(Expr::Match),
            Identifier if self.may_be_at_programmatic_call_start() => self.programmatic_call(),
            Identifier
                if self
                    .cursor
                    .lookahead(any().then(blanks().then(of_type(FatArrow))))
                    .is_some() =>
            {
                self.parse_lambda_definition().map(Expr::LambdaDef)
            }

            //test expressions has nothing to do in a value expression.
            SquaredLeftBracket => self.expected("Unexpected start of test expression", Unexpected),
            _ => self.literal(LiteralLeniency::Strict),
        }?;
        let value = self.expand_call_chain(value)?;
        self.handle_cast(value)
    }

    pub(crate) fn parse_next(&mut self) -> ParseResult<Expr<'a>> {
        let statement = self.statement();
        if statement.is_ok() {
            //consume end of expression
            self.cursor
                .force(spaces().then(eox()), "expected end of expression or file")?;
        };
        statement
    }

    ///handle specific case of casted expressions (<expr> as <type>)
    fn handle_cast(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        if self.cursor.lookahead(blanks().then(of_type(As))).is_some() {
            return self.parse_cast(expr).map(Expr::Casted);
        }
        Ok(expr)
    }

    ///handle tricky case of lambda `(e) => x` and parentheses `(e)`
    fn lambda_or_parentheses(&mut self) -> ParseResult<Expr<'a>> {
        let initial = self.cursor.get_pos();
        self.advance_to_parenthesis_end();
        if self
            .cursor
            .lookahead(blanks().then(of_type(FatArrow)))
            .is_some()
        {
            self.cursor.repos(initial);
            self.parse_lambda_definition().map(Expr::LambdaDef)
        } else {
            self.cursor.repos(initial);
            self.parenthesis().map(Expr::Parenthesis)
        }
    }

    ///handle tricky case of lambda `(e) => x` and subshell `(e)`
    fn subshell_or_parentheses(&mut self) -> ParseResult<Expr<'a>> {
        let initial = self.cursor.get_pos();
        self.advance_to_parenthesis_end();
        if self
            .cursor
            .lookahead(blanks().then(of_type(FatArrow)))
            .is_some()
        {
            self.cursor.repos(initial);
            self.parse_lambda_definition().map(Expr::LambdaDef)
        } else {
            self.cursor.repos(initial);
            self.subshell().map(Expr::Subshell)
        }
    }

    /// Advances the parser to the end of the current parenthesis pair, given that the cursor is
    /// currently on the opening parenthesis.
    ///
    /// This is currently used to handle the case of `(e) => x` and `(e)`.
    fn advance_to_parenthesis_end(&mut self) {
        let mut parenthesis_count = -1;
        while let Some(token) = self.cursor.next_opt() {
            match token.token_type {
                RoundedLeftBracket => parenthesis_count += 1,
                RoundedRightBracket => {
                    if parenthesis_count == 0 {
                        break;
                    }
                    parenthesis_count -= 1;
                }
                _ => {}
            }
        }
    }

    /// Raise an error on the current token.
    ///
    /// Use [Parser::expected_with] if the error is not on the current token.
    pub(crate) fn expected<T>(
        &self,
        message: impl Into<String>,
        kind: ParseErrorKind,
    ) -> ParseResult<T> {
        Err(self.mk_parse_error(message, self.cursor.peek(), kind))
    }

    /// Raise an error with a specific context.
    ///
    /// The context is used to better display where the error is.
    /// A context can be a single token or a range of tokens.
    pub(crate) fn expected_with<T>(
        &self,
        message: impl Into<String>,
        context: impl Into<ErrorContext<'a>>,
        kind: ParseErrorKind,
    ) -> ParseResult<T> {
        Err(self.mk_parse_error(message, context, kind))
    }

    /// Expect a specific delimiter token type and pop it from the delimiter stack.
    pub(crate) fn expect_delimiter(&mut self, eog: TokenType) -> ParseResult<Token<'a>> {
        if let Some(token) = self.cursor.advance(of_type(eog)) {
            self.delimiter_stack.pop_back();
            Ok(token)
        } else if self.cursor.peek().token_type.is_closing_ponctuation() {
            self.mismatched_delimiter(eog)
        } else {
            self.expected(
                format!(
                    "Expected '{}' delimiter.",
                    eog.str().unwrap_or("specific token")
                ),
                self.delimiter_stack
                    .back()
                    .map(|last| ParseErrorKind::Unpaired(self.cursor.relative_pos(last)))
                    .unwrap_or(Unexpected),
            )
        }
    }

    /// Raise a mismatched delimiter error on the current token.
    pub(crate) fn mismatched_delimiter<T>(&mut self, eog: TokenType) -> ParseResult<T> {
        if let Some(last) = self.delimiter_stack.back() {
            self.expected(
                "Mismatched closing delimiter.",
                ParseErrorKind::Unpaired(self.cursor.relative_pos(last)),
            )
        } else {
            self.expected(
                "Unexpected closing delimiter.",
                ParseErrorKind::Expected(eog.str().unwrap_or("specific token").to_string()),
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

    /// Executes an operation on the parser, and returns the result and whether there were no errors that
    /// were reported and already handled.
    pub(crate) fn observe_error_reports<E>(
        &mut self,
        transaction: impl FnOnce(&mut Self) -> ParseResult<E>,
    ) -> ParseResult<(E, bool)> {
        let old_len = self.errors.len();
        let result = transaction(self);
        Ok((result?, self.errors.len() == old_len))
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
                let start_pos = literal.segment.start;
                if self
                    .binary_operation_right(expr.clone(), Parser::next_value)
                    .is_ok()
                {
                    let end_pos = self.cursor.relative_pos(self.cursor.peek()).end;
                    let slice = &self.source.source[start_pos..end_pos];
                    return self.expected_with(
                        "Binary operations must be enclosed in a value expression.",
                        slice,
                        ParseErrorKind::UnexpectedInContext(format!("$(( {slice} ))")),
                    );
                }
            }
        }

        if self.cursor.lookahead(of_type(As)).is_some() {
            let expr = self.parse_cast(expr).map(Expr::Casted)?;
            return self.parse_binary_expr(expr);
        }

        if self.cursor.lookahead(bin_op()).is_none() {
            return Ok(expr);
        }
        //else, we hit an invalid binary expression.
        self.expected("invalid expression operator", Unexpected)
    }

    //parses any binary value expression, considering given input expression
    //as the left arm of the expression.
    //if given expression is directly followed by an eox delimiter, then return it as is
    fn parse_binary_value_expr(&mut self, expr: Expr<'a>) -> ParseResult<Expr<'a>> {
        self.cursor.advance(spaces()); //consume word separators

        if self.cursor.advance(of_type(DotDot)).is_some() {
            return self
                .parse_range(expr)
                .map(|expr| Expr::Range(Iterable::Range(expr)));
        }

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
        let token = self.cursor.next()?;
        let err = self.mk_parse_error("invalid infix operator", token, Unexpected);
        // Avoid recovering here to block the cursor on the closing delimiter token
        if self
            .cursor
            .lookahead(spaces().then(eox().or(eod())))
            .is_some()
        {
            return Err(err);
        }

        // We can try something here...
        self.report_error(err);
        self.value().map(|_| expr)
    }

    ///Skips spaces and verify that this parser is not parsing the end of an expression
    /// (unescaped newline or semicolon)
    pub(crate) fn repos(&mut self, message: &str) -> ParseResult<()> {
        self.cursor.advance(spaces()); //skip word separators
        if self.cursor.lookahead(eox()).is_some() {
            return self.expected(message, Unexpected);
        }
        Ok(())
    }

    /// Advances the cursor after an error, and reports it.
    ///
    /// The base behavior is to go to the end of the file or the next valid closing delimiter,
    /// but this can be further configured by the `break_on` parameter.
    pub(crate) fn recover_from(&mut self, error: ParseError, break_on: impl Move + Copy) {
        match error.kind {
            ParseErrorKind::Unpaired(_) => {
                self.repos_to_top_delimiter();
            }
            _ => {
                self.repos_to_next_expr(break_on);
            }
        }
        self.report_error(error);
    }

    /// Advances the cursor due to an error, but does not report it.
    ///
    /// In most cases, [`Parser::recover_from`] should be used instead.
    ///
    /// This should be used when a delimiter has been pushed to the stack,
    /// but an error occurred before the corresponding closing delimiter was found.
    pub(crate) fn repos_delimiter_due_to(&mut self, error: &ParseError) {
        // Unpaired delimiters already look for the next valid closing delimiter.
        // Only handle other errors that would leave the delimiter stack in an invalid state.
        if !matches!(error.kind, ParseErrorKind::Unpaired(_)) {
            self.repos_to_top_delimiter();
        }
    }

    /// Adds an error to the parser's error vector.
    ///
    /// This assumes that any recovery has already been done.
    /// Prefer using [`Parser::recover_from`] instead if it's not the case.
    pub(crate) fn report_error(&mut self, error: ParseError) {
        self.errors.push(error);
    }

    /// Goes to the next expression, where the move can be specialized.
    fn repos_to_next_expr(&mut self, break_on: impl Move + Copy) {
        // If delimiters are encountered while moving, they must be removed from the stack first,
        // before repositioning.
        let start_len = self.delimiter_stack.len();
        while !self.cursor.is_at_end() {
            // Stop before a break_on token.
            if self.delimiter_stack.len() == start_len && self.cursor.lookahead(break_on).is_some()
            {
                break;
            }

            // See if we're at a closing delimiter.
            let token = self.cursor.peek();
            if token.token_type.is_opening_ponctuation() {
                self.delimiter_stack.push_back(token.clone());
            }
            if let Some(last) = self.delimiter_stack.back() {
                if last
                    .token_type
                    .closing_pair()
                    .expect("invalid delimiter passed to stack")
                    == token.token_type
                {
                    if self.delimiter_stack.len() > start_len {
                        self.delimiter_stack.pop_back();
                    } else {
                        // Do not consume it to avoid breaking the stack.
                        // The caller will consume it.
                        break;
                    }
                }
            }
            // Otherwise, just advance.
            self.cursor.next_opt();
        }
    }

    /// Goes to the next closing delimiter of the top delimiter on the stack.
    ///
    /// If the stack is empty, this does nothing.
    /// Always prefer using [`Parser::recover_from`] instead.
    pub(crate) fn repos_to_top_delimiter(&mut self) {
        let start_len = self.delimiter_stack.len();
        while let Some(token) = self.cursor.next_opt() {
            if token.token_type.is_opening_ponctuation() {
                self.delimiter_stack.push_back(token.clone());
            } else if let Some(last) = self.delimiter_stack.back() {
                if last
                    .token_type
                    .closing_pair()
                    .expect("invalid delimiter passed to stack")
                    == token.token_type
                {
                    self.delimiter_stack.pop_back();
                    if self.delimiter_stack.len() < start_len {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }
}
