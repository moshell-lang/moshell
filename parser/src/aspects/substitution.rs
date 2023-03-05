use crate::aspects::group::GroupAspect;
use crate::aspects::var_reference::VarReferenceAspect;
use crate::ast::substitution::{Substitution, SubstitutionKind};
use crate::ast::Expr;
use crate::moves::{eox, MoveOperations, not, of_type, space};
use crate::parser::{ParseResult, Parser};
use lexer::token::TokenType;
use lexer::token::TokenType::{RoundedLeftBracket};
use crate::ast::value::{Literal, LiteralValue};

/// A parser for substitution expressions.
pub(crate) trait SubstitutionAspect<'a> {
    /// Parses a substitution expression, i.e. a variable reference or a command capture.
    fn substitution(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> SubstitutionAspect<'a> for Parser<'a> {
    fn substitution(&mut self) -> ParseResult<Expr<'a>> {
        let dollar_value = self.cursor.force(
            of_type(TokenType::Dollar),
            "Expected '$' sign.",
        )?.value;

        //if $ is directly followed by a '(' then it's the start of a Capture substitution.
        if self.cursor
            .lookahead(of_type(RoundedLeftBracket))
            .is_some() {
            return Ok(Expr::Substitution(Substitution {
                // Read the expression inside the parentheses as a new statement
                underlying: self.subshell()?,
                kind: SubstitutionKind::Capture,
            }))
        }

        // Short pass for variable references
        if self.cursor
            .lookahead(not(space().or(eox())))
            .is_some()
        {
            return self.var_reference();
        }

        //finaly it's a lonely '$' so we return it as a literal
        return Ok(Expr::Literal(Literal {
            lexeme: dollar_value,
            parsed: LiteralValue::String(dollar_value.to_string())
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::substitution::SubstitutionAspect;
    use crate::ast::callable::Call;
    use crate::ast::substitution::{Substitution, SubstitutionKind};
    use crate::ast::Expr;

    use crate::ast::group::{Block, Subshell};
    use crate::parser::{ParseError, Parser};
    use lexer::lexer::lex;
    use pretty_assertions::assert_eq;

    #[test]
    fn unterminated_substitution() {
        let tokens = lex("$(echo");
        let ast = Parser::new(tokens).substitution();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected end of expression".to_string()
            })
        );
    }

    #[test]
    fn unpaired_parenthesis() {
        let tokens = lex("$(a @(b) $(c d\\))");
        let ast = Parser::new(tokens).statement();
        assert_eq!(
            ast,
            Err(ParseError {
                message: "Unexpected end of expression".to_string()
            })
        );
    }

    #[test]
    fn mix_blocks() {
        let tokens = lex("$({ls $(pwd)})");
        let ast = Parser::new(tokens).substitution().expect("Failed to parse");
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
                                            arguments: vec![Expr::Literal("pwd".into())]
                                        })],
                                    },
                                    kind: SubstitutionKind::Capture,
                                }),
                            ],
                        })]
                    })],
                },
                kind: SubstitutionKind::Capture,
            })
        );
    }
}
