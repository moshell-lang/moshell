use crate::aspects::group::GroupAspect;
use crate::aspects::var_reference::VarReferenceAspect;
use crate::err::ParseErrorKind;
use crate::moves::{eox, not, of_type, repeat_n, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::substitution::{Substitution, SubstitutionKind};
use ast::value::{Literal, LiteralValue};
use ast::Expr;
use lexer::token::TokenType;
use lexer::token::TokenType::{RoundedLeftBracket, RoundedRightBracket};

/// A parser for substitution expressions.
pub(crate) trait SubstitutionAspect<'a> {
    /// Parses a substitution expression, i.e. a variable reference or a command capture.
    fn substitution(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> SubstitutionAspect<'a> for Parser<'a> {
    fn substitution(&mut self) -> ParseResult<Expr<'a>> {
        let dollar = self
            .cursor
            .force(of_type(TokenType::Dollar), "Expected '$' sign.")?;
        let dollar_value = dollar.value;

        //if $ is directly followed by a '(' then it's the start of a Capture substitution.
        if self.cursor.lookahead(of_type(RoundedLeftBracket)).is_some() {
            if let Some(start) = self
                .cursor
                .lookahead(repeat_n(2, of_type(RoundedLeftBracket)))
            {
                self.cursor.advance(of_type(RoundedLeftBracket));
                let parenthesis = self.parenthesis()?;
                self.cursor.force_with(
                    of_type(RoundedRightBracket),
                    "Expected a second closing parenthesis.",
                    ParseErrorKind::Unpaired(self.cursor.relative_pos_ctx(dollar..start)),
                )?;
                return Ok(Expr::Parenthesis(parenthesis));
            }
            return Ok(Expr::Substitution(Substitution {
                // Read the expression inside the parentheses as a new statement
                underlying: self.subshell()?,
                kind: SubstitutionKind::Capture,
            }));
        }

        // Short pass for variable references
        if self.cursor.lookahead(not(spaces().or(eox()))).is_some() {
            return self.var_reference();
        }

        //finally it's a lonely '$' so we return it as a literal
        return Ok(Expr::Literal(Literal {
            lexeme: dollar_value,
            parsed: LiteralValue::String(dollar_value.to_string()),
        }));
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::substitution::SubstitutionAspect;
    use ast::callable::Call;
    use ast::substitution::{Substitution, SubstitutionKind};
    use ast::Expr;

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use ast::group::{Block, Parenthesis, Subshell};
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::value::Literal;
    use ast::variable::VarReference;
    use context::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn unterminated_substitution() {
        let content = "$(echo";
        let source = Source::unknown(content);
        let ast = Parser::new(source).substitution();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Expected closing bracket.".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unpaired(1..2)
            })
        );
    }

    #[test]
    fn unpaired_parenthesis() {
        let content = "$(a $(b) $(c d\\))";
        let source = Source::unknown(content);
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Expected closing bracket.".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unpaired(1..2)
            })
        );
    }

    #[test]
    fn mix_blocks() {
        let source = Source::unknown("$({ls $(pwd)})");
        let ast = Parser::new(source).substitution().expect("Failed to parse");
        assert_eq!(
            ast,
            Expr::Substitution(Substitution {
                underlying: Subshell {
                    expressions: vec![Expr::Block(Block {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![
                                Expr::Literal("ls".into()),
                                Expr::Substitution(Substitution {
                                    underlying: Subshell {
                                        expressions: vec![Expr::Call(Call {
                                            arguments: vec![Expr::Literal("pwd".into())],
                                            tparams: vec![],
                                        })],
                                    },
                                    kind: SubstitutionKind::Capture,
                                }),
                            ],
                            tparams: vec![],
                        })]
                    })],
                },
                kind: SubstitutionKind::Capture,
            })
        );
    }

    #[test]
    fn unexpected_closing_parenthesis() {
        let content = "some stuff)";
        let source = Source::unknown(content);
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "expected end of expression or file".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Unexpected
            })
        );
    }

    #[test]
    fn parenthesis_mismatch() {
        let content = "$(test 9})";
        let source = Source::unknown(content);
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Mismatched closing delimiter.".to_string(),
                position: content.find('}').map(|p| (p..p + 1)).unwrap(),
                kind: ParseErrorKind::Unpaired(content.find('(').map(|p| (p..p + 1)).unwrap())
            })
        );
    }

    #[test]
    fn arithmetic() {
        let source = Source::unknown("$(($a + 1))");
        let ast = Parser::new(source).substitution().expect("Failed to parse");
        assert_eq!(
            ast,
            Expr::Parenthesis(Parenthesis {
                expression: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference { name: "a".into() })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::Literal(Literal {
                        lexeme: "1",
                        parsed: 1.into(),
                    })),
                })),
            })
        );
    }
}
