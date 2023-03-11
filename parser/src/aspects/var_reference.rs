use context::source::try_join_str;
use lexer::token::TokenType::*;

use ast::variable::VarReference;
use ast::Expr;
use crate::err::ParseErrorKind;
use crate::moves::{like, of_type, repeat, MoveOperations};
use crate::parser::{ParseResult, Parser};

pub trait VarReferenceAspect<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> VarReferenceAspect<'a> for Parser<'a> {
    /// Parses a variable reference.
    fn var_reference(&mut self) -> ParseResult<Expr<'a>> {
        let bracket = self.cursor.advance(of_type(CurlyLeftBracket));

        let tokens = self
            .cursor
            .select(
                of_type(Dollar) //only allow one occurrence of $
                    .or(repeat(like(|t| t != Dollar && t.is_valid_var_ref_name()))),
            )
            .leak();

        if tokens.len() == 0 {
            return self.expected(
                "variable reference with empty name",
                ParseErrorKind::Unexpected,
            );
        }

        let first = tokens[0].value;
        let name = tokens
            .into_iter()
            .skip(1)
            .fold(first, |acc, t| try_join_str(acc, t.value).unwrap());

        if let Some(bracket) = bracket {
            self.cursor.force_with(
                of_type(CurlyRightBracket),
                "Expected closing curly bracket.",
                ParseErrorKind::Unpaired(self.cursor.relative_pos(&bracket)),
            )?;
        }
        Ok(Expr::VarReference(VarReference { name }))
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::substitution::SubstitutionAspect;
    use ast::value::TemplateString;
    use ast::variable::VarReference;
    use ast::Expr;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use context::source::Source;
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_ref() {
        let source = Source::unknown("$VARIABLE");
        let ast = Parser::new(source).substitution().expect("failed to parse");
        assert_eq!(ast, Expr::VarReference(VarReference { name: "VARIABLE" }));
    }

    #[test]
    fn dollar_is_literal() {
        let source = Source::unknown("$");
        let ast = parse(source).expect("failed to parse");
        assert_eq!(ast, vec![Expr::Literal("$".into())])
    }

    #[test]
    fn special_refs() {
        let source = Source::unknown("$@;$^;$!;$!!;$$");
        let ast = parse(source).expect("failed to parse");
        assert_eq!(
            ast,
            vec![
                Expr::VarReference(VarReference { name: "@" }),
                Expr::VarReference(VarReference { name: "^" }),
                Expr::VarReference(VarReference { name: "!" }),
                Expr::VarReference(VarReference { name: "!!" }),
                Expr::VarReference(VarReference { name: "$" }),
            ]
        )
    }

    #[test]
    fn wrapped_ref() {
        let source = Source::unknown("${VAR}IABLE");
        let ast = parse(source).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::TemplateString(TemplateString {
                parts: vec![
                    Expr::VarReference(VarReference { name: "VAR" }),
                    Expr::Literal("IABLE".into()),
                ]
            })]
        )
    }

    #[test]
    fn test_ref_in_ref() {
        let content = "${V${A}R}";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Expected closing curly bracket.".to_string(),
                position: 4..5,
                kind: ParseErrorKind::Unpaired(content.find('{').map(|p| p..p + 1).unwrap()),
            })
        )
    }

    #[test]
    fn test_multiple_wrapped_ref() {
        let source = Source::unknown("${VAR}IABLE${LONG}${VERY_LONG}");
        let ast = parse(source).expect("failed to parse");
        assert_eq!(
            ast,
            vec![Expr::TemplateString(TemplateString {
                parts: vec![
                    Expr::VarReference(VarReference { name: "VAR" }),
                    Expr::Literal("IABLE".into()),
                    Expr::VarReference(VarReference { name: "LONG" }),
                    Expr::VarReference(VarReference { name: "VERY_LONG" }),
                ]
            })]
        )
    }
}
