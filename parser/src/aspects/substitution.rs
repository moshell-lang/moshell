use ast::substitution::{Substitution, SubstitutionKind};
use ast::value::{Literal, LiteralValue};
use ast::Expr;
use lexer::token::TokenType;
use lexer::token::TokenType::{RoundedLeftBracket, RoundedRightBracket};

use crate::err::ParseErrorKind;
use crate::moves::{line_end, not, of_type, repeat_n, spaces, Move};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    pub(crate) fn substitution(&mut self) -> ParseResult<Expr> {
        let dollar = self
            .cursor
            .force(of_type(TokenType::Dollar), "Expected '$' sign.")?;

        //if $ is directly followed by a '(' then it's the start of a Capture substitution.
        if self.cursor.lookahead(of_type(RoundedLeftBracket)).is_some() {
            if let Some(start) = self
                .cursor
                .lookahead(repeat_n(2, of_type(RoundedLeftBracket)))
            {
                self.cursor.advance(of_type(RoundedLeftBracket));
                let mut parenthesis = self.parenthesis()?;
                parenthesis.segment.start -= 2; // Include the '$('
                parenthesis.segment.end += 1;
                self.cursor.force_with(
                    of_type(RoundedRightBracket),
                    "Expected a second closing parenthesis.",
                    ParseErrorKind::Unpaired(dollar.span.start..start.span.end),
                )?;
                return Ok(Expr::Parenthesis(parenthesis));
            }
            return Ok(Expr::Substitution(Substitution {
                // Read the expression inside the parentheses as a new statement
                underlying: self.subshell().map(|mut subshell| {
                    subshell.segment.start -= 1; // Include the '$('
                    subshell
                })?,
                kind: SubstitutionKind::Capture,
            }));
        }

        // Short pass for variable references
        if self
            .cursor
            .lookahead(not(spaces().or(line_end())))
            .is_some()
        {
            return self.var_reference();
        }

        //finally it's a lonely '$' so we return it as a literal
        Ok(Expr::Literal(Literal {
            parsed: LiteralValue::String(dollar.text(self.source).to_owned()),
            segment: dollar.span,
        }))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use ast::call::Call;
    use ast::group::{Block, Parenthesis, Subshell};
    use ast::operation::{BinaryOperation, BinaryOperator};
    use ast::substitution::{Substitution, SubstitutionKind};
    use ast::value::Literal;
    use ast::variable::{VarName, VarReference};
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::find_in;

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use crate::source::literal;

    #[test]
    fn unterminated_substitution() {
        let source = "$(echo";
        let ast = Parser::new(source).substitution();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Expected closing bracket.".to_string(),
                position: source.len()..source.len(),
                kind: ParseErrorKind::Unpaired(1..2)
            })
        );
    }

    #[test]
    fn unpaired_parenthesis() {
        let source = "$(a $(b) $(c d\\))";
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Expected closing bracket.".to_string(),
                position: source.len()..source.len(),
                kind: ParseErrorKind::Unpaired(1..2)
            })
        );
    }

    #[test]
    fn mix_blocks() {
        let source = "$({ls $(pwd)})";
        let ast = Parser::new(source).substitution().expect("Failed to parse");
        assert_eq!(
            ast,
            Expr::Substitution(Substitution {
                underlying: Subshell {
                    expressions: vec![Expr::Block(Block {
                        expressions: vec![Expr::Call(Call {
                            arguments: vec![
                                literal(source, "ls"),
                                Expr::Substitution(Substitution {
                                    underlying: Subshell {
                                        expressions: vec![Expr::Call(Call {
                                            arguments: vec![literal(source, "pwd")],
                                        })],
                                        segment: find_in(source, "$(pwd)")
                                    },
                                    kind: SubstitutionKind::Capture,
                                }),
                            ],
                        })],
                        segment: find_in(source, "{ls $(pwd)}")
                    })],
                    segment: source.segment()
                },
                kind: SubstitutionKind::Capture,
            })
        );
    }

    #[test]
    fn unexpected_closing_parenthesis() {
        let source = "some stuff)";
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected closing delimiter.".to_string(),
                position: source.find(')').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected
            })
        );
    }

    #[test]
    fn parenthesis_mismatch() {
        let source = "$(test 9})";
        let ast: ParseResult<_> = parse(source).into();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Mismatched closing delimiter.".to_string(),
                position: source.find('}').map(|p| (p..p + 1)).unwrap(),
                kind: ParseErrorKind::Unpaired(source.find('(').map(|p| (p..p + 1)).unwrap())
            })
        );
    }

    #[test]
    fn arithmetic() {
        let source = "$(($a + 1))";
        let ast = Parser::new(source).substitution().expect("Failed to parse");
        assert_eq!(
            ast,
            Expr::Parenthesis(Parenthesis {
                expression: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::VarReference(VarReference {
                        name: VarName::User("a".into()),
                        segment: find_in(source, "$a")
                    })),
                    op: BinaryOperator::Plus,
                    right: Box::new(Expr::Literal(Literal {
                        parsed: 1.into(),
                        segment: find_in(source, "1")
                    })),
                })),
                segment: source.segment(),
            })
        );
    }
}
