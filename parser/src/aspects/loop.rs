use lexer::token::TokenType;

use crate::ast::control_flow::{Loop, While};
use crate::moves::{blanks, eox, of_type};
use crate::parser::{ParseResult, Parser};

///a parser aspect for loops and while expressions
pub trait LoopAspect<'a> {
    ///parse a while expression
    fn parse_while(&mut self) -> ParseResult<While<'a>>;
    ///parse a loop expression
    fn parse_loop(&mut self) -> ParseResult<Loop<'a>>;
}

impl<'a> LoopAspect<'a> for Parser<'a> {
    fn parse_while(&mut self) -> ParseResult<While<'a>> {
        self.cursor.force(
            of_type(TokenType::While),
            "expected 'while' at start of while expression",
        )?;
        //consume blanks before condition
        self.cursor.advance(blanks());
        let condition = Box::new(self.expression_statement()?);

        //consume blanks
        self.cursor.advance(blanks());
        //then consume eox (if any)
        self.cursor.advance(eox());

        let body = Box::new(self.expression_statement()?);

        Ok(While { condition, body })
    }

    fn parse_loop(&mut self) -> ParseResult<Loop<'a>> {
        self.cursor.force(
            of_type(TokenType::Loop),
            "expected 'loop' at start of loop expression",
        )?;
        self.cursor.advance(blanks());
        let body = Box::new(self.expression_statement()?);

        Ok(Loop { body })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::callable::Call;
    use crate::ast::control_flow::{Loop, While};
    use crate::ast::group::Block;
    use crate::ast::operation::BinaryOperation;
    use crate::ast::operation::BinaryOperator::And;
    use crate::ast::variable::VarReference;
    use crate::ast::Expr;
    use crate::ast::Expr::{Break, Continue};
    use crate::err::ParseError;
    use crate::err::ParseErrorKind::Unexpected;
    use crate::parse;
    use crate::parser::ParseResult;
    use context::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn loop_with_break_and_continues() {
        let res = parse(Source::unknown(
            "loop {
            continue; break;
            }",
        ))
        .expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Continue, Break]
                }))
            })]
        )
    }

    #[test]
    fn loop_with_break_and_continues_inline() {
        let res = parse(Source::unknown("loop ssh mabatista1@iut && break")).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Binary(BinaryOperation {
                    left: Box::new(Expr::Call(Call {
                        arguments: vec![
                            Expr::Literal("ssh".into()),
                            Expr::Literal("mabatista1@iut".into())
                        ],
                        tparams: vec![],
                    })),
                    op: And,
                    right: Box::new(Break)
                }))
            })]
        )
    }

    #[test]
    fn test_loop() {
        let res = parse(Source::unknown("loop \n\n \n \n date")).expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::Loop(Loop {
                body: Box::new(Expr::Call(Call {
                    arguments: vec![Expr::Literal("date".into())],
                    tparams: vec![],
                }))
            })]
        )
    }

    #[test]
    fn loop_no_body() {
        let content = "loop";
        let res: ParseResult<_> = parse(Source::unknown(content)).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected expression statement".to_string(),
                position: content.len()..content.len(),
                kind: Unexpected,
            })
        )
    }

    #[test]
    fn test_while() {
        let res = parse(Source::unknown("while \n\n \n \n $1 \n\n \n{ echo test }"))
            .expect("parse failed");
        assert_eq!(
            res,
            vec![Expr::While(While {
                condition: Box::new(Expr::VarReference(VarReference { name: "1" })),
                body: Box::new(Expr::Block(Block {
                    expressions: vec![Expr::Call(Call {
                        arguments: vec![Expr::Literal("echo".into()), Expr::Literal("test".into())],
                        tparams: vec![],
                    })]
                })),
            })]
        )
    }

    #[test]
    fn while_no_condition() {
        let content = "while";
        let res: ParseResult<_> = parse(Source::unknown(content)).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected expression statement".to_string(),
                position: content.len()..content.len(),
                kind: Unexpected,
            })
        )
    }

    #[test]
    fn while_no_body() {
        let content = "while $x";
        let res: ParseResult<_> = parse(Source::unknown(content)).into();
        assert_eq!(
            res,
            Err(ParseError {
                message: "Expected expression statement".to_string(),
                position: content.len()..content.len(),
                kind: Unexpected,
            })
        )
    }
}
