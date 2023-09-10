use ast::operation::{BinaryOperation, BinaryOperator, UnaryOperation, UnaryOperator};
use ast::r#type::CastedExpr;
use ast::range::Iterable;
use ast::Expr;
use context::source::{Source, SourceSegment, SourceSegmentHolder};
use lexer::lex;
use lexer::token::TokenType::*;
use lexer::token::{Token, TokenType};
use std::num::NonZeroU8;

use crate::aspects::assign::AssignAspect;
use crate::aspects::binary_operation::{infix_precedence, shell_infix_precedence};
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
use crate::aspects::r#struct::StructAspect;
use crate::aspects::r#type::TypeAspect;
use crate::aspects::range::RangeAspect;
use crate::aspects::redirection::RedirectionAspect;
use crate::aspects::test::TestAspect;
use crate::aspects::var_declaration::VarDeclarationAspect;
use crate::cursor::ParserCursor;
use crate::err::ParseErrorKind::Unexpected;
use crate::err::{
    determine_skip_sections, ErrorContext, ParseError, ParseErrorKind, ParseReport, SkipSections,
};
use crate::moves::{
    any, blanks, line_end, next, of_type, of_types, repeat, spaces, Move, MoveOperations,
};

pub(crate) type ParseResult<T> = Result<T, ParseError>;

/// A parser for the Moshell scripting language.
pub(crate) struct Parser<'a> {
    pub(crate) cursor: ParserCursor<'a>,
    pub(crate) source: Source<'a>,
    pub(crate) skip: SkipSections,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from a defined source.
    pub(crate) fn new(source: Source<'a>) -> Self {
        let (tokens, unmatched) = lex(source.source);
        let cursor = ParserCursor::new_with_source(tokens, source.source);
        let skip = determine_skip_sections(source.source.len(), &unmatched);
        let errors = unmatched
            .into_iter()
            .filter_map(|unmatched| {
                Some(ParseError {
                    message: if unmatched.opening.is_some() {
                        "Mismatched closing delimiter."
                    } else {
                        "Unexpected closing delimiter."
                    }
                    .to_owned(),
                    position: unmatched.candidate?..(unmatched.candidate? + 1),
                    kind: if unmatched.opening.is_some() {
                        ParseErrorKind::Unpaired(unmatched.opening?..unmatched.opening? + 1)
                    } else {
                        Unexpected
                    },
                })
            })
            .collect::<Vec<ParseError>>();
        Self {
            cursor,
            source,
            skip,
            errors,
        }
    }

    /// Parses input tokens into an abstract syntax tree representation.
    pub fn parse(mut self) -> ParseReport<'a> {
        let mut statements = Vec::new();

        while self.look_for_input() {
            match self.parse_next() {
                Err(error) => {
                    let pos = self.cursor.get_pos();
                    self.recover_from(error, line_end());
                    if self.cursor.get_pos() == pos {
                        // If the error was not recovered from, advance anyway.
                        // This prevents infinite loops when delimiters are not
                        // closed without any candidates.
                        self.cursor.advance(next());
                    }
                }
                Ok(statement) => statements.push(statement),
            }
        }

        ParseReport {
            expr: statements,
            errors: self.errors,
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
        self.cursor.advance(repeat(spaces().or(line_end())));

        !self.cursor.is_at_end()
    }

    /// Parses the root of an expression tree.
    ///
    /// It usually spans on at least one entire line.
    pub(crate) fn declaration(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected declaration or statement")?;
        match self.cursor.peek().token_type {
            Use => self.parse_use(),
            Fun => self
                .parse_function_declaration()
                .map(Expr::FunctionDeclaration),
            Loop => self.parse_loop().map(Expr::Loop),
            _ => self.statement(),
        }
    }

    /// Parses a statement.
    pub(crate) fn statement(&mut self) -> ParseResult<Expr<'a>> {
        self.statement_precedence(NonZeroU8::MIN)
    }

    fn statement_precedence(&mut self, min_precedence: NonZeroU8) -> ParseResult<Expr<'a>> {
        let mut lhs = self.next_statement()?;
        lhs = self.parse_detached(lhs)?;
        loop {
            self.cursor.advance(spaces());
            let tok = self.cursor.peek().token_type;
            let precedence = shell_infix_precedence(self);
            if precedence < min_precedence.get() {
                break;
            }
            match tok {
                Bar => {
                    lhs = self.pipeline(lhs)?;
                }
                _ if self.is_at_redirection_sign() => {
                    lhs = self.redirectable(lhs)?;
                }
                tok => {
                    self.cursor.next_opt();
                    let op = BinaryOperator::try_from(tok).expect("Invalid binary operator");
                    let rhs = self.statement_precedence(
                        NonZeroU8::new(precedence.saturating_add(1))
                            .expect("New precedence should be non-zero"),
                    )?; // + 1 for left-associativity
                    lhs = Expr::Binary(BinaryOperation {
                        op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    });
                }
            }
        }
        Ok(lhs)
    }

    /// Parses an expression.
    pub(crate) fn expression(&mut self) -> ParseResult<Expr<'a>> {
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

            _ => self.value(),
        }
    }

    /// Parses a value.
    pub(crate) fn value(&mut self) -> ParseResult<Expr<'a>> {
        self.value_precedence(NonZeroU8::MIN)
    }

    /// Parses the left-hand side of the next statement.
    fn next_statement(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected statement")?;

        let pivot = self.cursor.peek().token_type;
        let expr = match pivot {
            Struct => self.parse_struct().map(Expr::StructDeclaration),
            Impl => self.parse_impl().map(Expr::Impl),
            While => self.parse_while().map(Expr::While),
            For => self.parse_for().map(Expr::For),
            Identifier
                if self
                    .cursor
                    .lookahead(next().then(spaces().then(of_type(Equal))))
                    .is_some() =>
            {
                self.parse_assign().map(Expr::Assign)
            }

            Var | Val => self.var_declaration(),
            Identifier | Reef => self.any_call(),
            Dot => self.call(),
            Shell => {
                self.cursor.next_opt();
                self.cursor.advance(spaces());
                if self.cursor.peek().token_type == CurlyLeftBracket {
                    self.block().map(Expr::Block)
                } else {
                    self.any_call()
                }
            }
            Not if self
                .cursor
                .lookahead(next().then(spaces().then(of_types(&[RoundedLeftBracket, Identifier]))))
                .is_some() =>
            {
                let token = self.cursor.next()?;
                let expr = self.next_statement()?;
                let segment = self.cursor.relative_pos(token).start..expr.segment().end;
                Ok(Expr::Unary(UnaryOperation {
                    op: UnaryOperator::Not,
                    expr: Box::new(expr),
                    segment,
                }))
            }

            _ if pivot.is_infix_operator() && !pivot.is_prefix_operator() => self.call(),

            _ => self.expression(),
        }?;
        self.expand_call_chain(expr)
    }

    /// Parses the left-hand side of the next value.
    fn lhs(&mut self) -> ParseResult<Expr<'a>> {
        self.repos("Expected value")?;
        match self.cursor.peek().token_type {
            RoundedLeftBracket => self.lambda_or_parentheses(),
            CurlyLeftBracket => self.block().map(Expr::Block),
            SquaredLeftBracket => self.expected(
                "Unexpected start of test expression",
                ParseErrorKind::Unexpected,
            )?,
            ty @ (Minus | Not) => {
                let op = self.cursor.next()?;
                let rhs = self.lhs()?;
                let segment = self.cursor.relative_pos(op).start..rhs.segment().end;
                Ok(Expr::Unary(UnaryOperation {
                    op: match ty {
                        Minus => UnaryOperator::Negate,
                        Not => UnaryOperator::Not,
                        _ => unreachable!(),
                    },
                    expr: Box::new(rhs),
                    segment,
                }))
            }
            If => self.parse_if().map(Expr::If),
            Match => self.parse_match().map(Expr::Match),
            Identifier | Reef if self.may_be_at_programmatic_call_start() => {
                self.programmatic_call()
            }
            Identifier
                if self
                    .cursor
                    .lookahead(any().then(blanks().then(of_type(FatArrow))))
                    .is_some() =>
            {
                self.parse_lambda_definition().map(Expr::LambdaDef)
            }
            _ => self.literal(LiteralLeniency::Strict),
        }
    }

    pub(crate) fn value_precedence(&mut self, min_precedence: NonZeroU8) -> ParseResult<Expr<'a>> {
        // Parse prefix operators
        let mut lhs = self.lhs()?;

        // Parse postfix operators
        lhs = self.expand_call_chain(lhs)?;

        // Parse infix operators
        loop {
            self.cursor.advance(spaces());
            let tok = self.cursor.peek().token_type;
            let precedence = infix_precedence(tok);
            if precedence < min_precedence.get() {
                break;
            }
            self.cursor.next_opt();
            match tok {
                As => {
                    let casted_type = self.parse_type()?;
                    let segment = lhs.segment().start..casted_type.segment().end;
                    lhs = Expr::Casted(CastedExpr {
                        expr: Box::new(lhs),
                        casted_type,
                        segment,
                    });
                }
                DotDot => {
                    lhs = self
                        .parse_range(lhs)
                        .map(|expr| Expr::Range(Iterable::Range(expr)))?
                }
                tok => {
                    let op = BinaryOperator::try_from(tok).expect("Invalid binary operator");
                    let rhs = self.value_precedence(
                        NonZeroU8::new(precedence.saturating_add(1))
                            .expect("New precedence should be non-zero"),
                    )?; // + 1 for left-associativity
                    lhs = Expr::Binary(BinaryOperation {
                        op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    });
                }
            }
        }

        Ok(lhs)
    }

    pub(crate) fn parse_next(&mut self) -> ParseResult<Expr<'a>> {
        let declaration = self.declaration();
        if declaration.is_ok() {
            // Consume end of statement
            if let Err(err) = self.cursor.force(
                spaces().then(line_end()),
                "expected end of expression or file",
            ) {
                self.recover_from(err, line_end());
            }
        };
        declaration
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
    pub(crate) fn expect_delimiter(
        &mut self,
        start: Token<'a>,
        eog: TokenType,
    ) -> ParseResult<Token<'a>> {
        if let Some(token) = self.cursor.advance(of_type(eog)) {
            Ok(token)
        } else {
            let err = self.expected(
                format!(
                    "Expected '{}' delimiter.",
                    eog.str().unwrap_or("specific token")
                ),
                ParseErrorKind::Unpaired(self.cursor.relative_pos(start)),
            );
            if self.cursor.peek().token_type.is_closing_ponctuation() {
                self.repos_to_top_delimiter();
            }
            err
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

    ///Skips spaces and verify that this parser is not parsing the end of an expression
    /// (unescaped newline or semicolon)
    pub(crate) fn repos(&mut self, message: &str) -> ParseResult<()> {
        self.cursor.advance(spaces()); //skip word separators
        if self.cursor.lookahead(line_end()).is_some() {
            return self.expected(message, Unexpected);
        }
        Ok(())
    }

    /// Advances the cursor after an error, and reports it.
    ///
    /// The base behavior is to go to the end of the file or the next valid closing delimiter,
    /// but this can be further configured by the `break_on` parameter.
    pub(crate) fn recover_from(&mut self, error: ParseError, break_on: impl Move + Copy) {
        if self.skip.contains(error.position.start) {
            // Mismatched delimiters are already reported by the lexer, so we can skip them
            // in a section marked as skipped. Contrary to repos_to_top_delimiter, this doesn't
            // recover after the delimiter, but just before it.
            while !self.cursor.is_at_end() {
                let token = self.cursor.peek();
                if self
                    .skip
                    .contains(self.cursor.relative_pos(token.value).start)
                {
                    self.cursor.next_opt();
                } else {
                    break;
                }
            }
            return;
        }

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
    /// This should be used when a delimiter was seen by the parser,
    /// but an error occurred before the corresponding closing delimiter was found.
    pub(crate) fn repos_delimiter_due_to(&mut self, error: &ParseError) {
        // Unpaired delimiters are known by the lexer, so we can use that information to
        // skip them. Repositioning is not strictly necessary, but if used appropriately
        // the errors will be more precise.
        if self.skip.contains(error.position.start) {
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
        let mut delimiter_stack = Vec::new();
        while !self.cursor.is_at_end() {
            if self
                .skip
                .contains(self.cursor.relative_pos(self.cursor.peek()).start)
            {
                self.cursor.next_opt();
                continue;
            }
            // Stop before a break_on token.
            if delimiter_stack.is_empty() && self.cursor.lookahead(break_on).is_some() {
                break;
            }

            // See if we're at a closing delimiter.
            let token = self.cursor.peek();
            if token.token_type.is_opening_ponctuation() {
                delimiter_stack.push(token.token_type);
            }
            if let Some(last) = delimiter_stack.last() {
                if last
                    .closing_pair()
                    .expect("invalid delimiter passed to stack")
                    == token.token_type
                {
                    delimiter_stack.pop();
                }
            } else if token.token_type.is_closing_ponctuation() {
                // Do not consume it to avoid breaking the stack.
                // The caller will consume it.
                break;
            }
            // Otherwise, just advance.
            self.cursor.next_opt();
        }
    }

    /// Goes after the next closing delimiter of the top delimiter on the stack.
    ///
    /// The implementation will skip invalid sections and goes to the next valid closing delimiter.
    /// Always prefer using [`Parser::recover_from`] instead.
    pub(crate) fn repos_to_top_delimiter(&mut self) {
        while let Some(token) = self.cursor.next_opt() {
            if !self
                .skip
                .contains(self.cursor.relative_pos(token.value).start)
                && token.token_type.is_closing_ponctuation()
            {
                break;
            }
        }
    }
}

/// Ensures that the given elements are empty, returning Err(ParseError) with given message otherwise.
/// The error's context segment will be the segment between first and last elements of given elements.
pub(crate) fn ensure_empty<T>(
    msg: &str,
    elements: Vec<T>,
    convert: impl Fn(&T) -> SourceSegment,
) -> ParseResult<()> {
    if let Some(first) = elements.first() {
        let position = elements
            .last()
            .map(|last| convert(first).start..convert(last).end)
            .unwrap_or_else(|| convert(first));

        Err(ParseError {
            message: msg.to_string(),
            position,
            kind: Unexpected,
        })
    } else {
        Ok(())
    }
}
