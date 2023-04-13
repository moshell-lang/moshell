use crate::err::ParseErrorKind;

use crate::aspects::expr_list::ExpressionListAspect;
use crate::moves::{any, blanks, eox, of_type, of_types, repeat, spaces, MoveOperations};
use crate::parser::{ParseResult, Parser};
use ast::r#use::{Import, ImportList, ImportedSymbol, Use};
use ast::Expr;
use context::source::{try_join_str, SourceSegmentHolder};
use lexer::token::TokenType::{
    As, At, ColonColon, CurlyLeftBracket, CurlyRightBracket, Dot, DotDot, FloatLiteral, Identifier,
    IntLiteral, Slash, Star,
};
use lexer::token::{Token, TokenType};

/// Parser aspect to parse all expressions in relation with modules.
/// Which can be use statements, or module location prefix
pub trait ModulesAspect<'a> {
    ///parse a 'use x, y' statement
    fn parse_use(&mut self) -> ParseResult<Expr<'a>>;

    ///parse identifiers separated between `::` expressions.
    /// This method stops when it founds an expressions that is not an identifier.
    fn parse_inclusion_path(&mut self) -> ParseResult<Vec<&'a str>>;
}

impl<'a> ModulesAspect<'a> for Parser<'a> {
    fn parse_use(&mut self) -> ParseResult<Expr<'a>> {
        let start = self
            .cursor
            .force(of_type(TokenType::Use), "expected 'use'")?;

        let import = self.parse_import()?;

        self.cursor.advance(spaces()); //consume spaces

        if self.cursor.advance(eox()).is_none() {
            return self.expected(
                "expected new line or semicolon",
                ParseErrorKind::Expected("<new_line> or ';'".to_string()),
            );
        };

        let import_seg_end = import.segment().end;
        Ok(Expr::Use(Use {
            import,
            segment: self.cursor.relative_pos_ctx(start).start..import_seg_end,
        }))
    }

    fn parse_inclusion_path(&mut self) -> ParseResult<Vec<&'a str>> {
        self.parse_prefix_modules_with(Vec::new())
    }
}

impl<'a> Parser<'a> {
    fn parse_prefix_modules_with(&mut self, mut head: Vec<&'a str>) -> ParseResult<Vec<&'a str>> {
        while let Some(identifier) = self.cursor.lookahead(spaces().then(of_type(Identifier))) {
            if self
                .cursor
                .advance(any().then(spaces().then(of_type(ColonColon))))
                .is_none()
            {
                break;
            }
            head.push(identifier.value);
        }

        Ok(head)
    }

    fn parse_import(&mut self) -> ParseResult<Import<'a>> {
        self.cursor.advance(blanks()); //consume blanks

        let pivot = self.cursor.peek();

        match pivot.token_type {
            At => {
                //handle '@ENV_VARIABLE` expression
                self.cursor.next()?;
                let env_variable = self.cursor.force_with(
                    spaces().then(of_type(Identifier)),
                    "Environment variable name expected.",
                    ParseErrorKind::Expected("<identifier>".to_string()),
                )?;
                Ok(Import::Environment(
                    env_variable.value,
                    self.cursor.relative_pos_ctx(pivot..env_variable),
                ))
            }
            Star => self.expected_with(
                "import all statement needs a symbol prefix.",
                pivot,
                ParseErrorKind::Expected("module or file path".to_string()),
            ),
            CurlyLeftBracket => self
                .parse_import_list(self.cursor.peek(), Vec::new())
                .map(Import::List),

            _ => self.parse_import_with_path(),
        }
    }

    fn parse_import_list(
        &mut self,
        start: Token<'a>,
        path: Vec<&'a str>,
    ) -> ParseResult<ImportList<'a>> {
        self.parse_explicit_list(CurlyLeftBracket, CurlyRightBracket, Self::parse_import)
            .and_then(|(imports, s)| {
                if imports.is_empty() {
                    return self.expected_with(
                        "empty brackets",
                        start..self.cursor.peek(),
                        ParseErrorKind::Expected("non-empty brackets".to_string()),
                    );
                }
                Ok(ImportList {
                    path,
                    imports,
                    segment: self.cursor.relative_pos_ctx(start).start..s.end,
                })
            })
    }

    fn expect_identifier(&mut self) -> ParseResult<&'a str> {
        self.cursor.advance(spaces()); //consume spaces

        self.cursor
            .force_with(
                of_type(Identifier),
                "identifier expected, you might want to escape it",
                ParseErrorKind::UnexpectedInContext(format!("\\{}", self.cursor.peek().value)),
            )
            .map(|t| t.value)
    }

    //parse an identifier or a file path.
    fn parse_first_path_element(&mut self) -> ParseResult<Option<&'a str>> {
        //collects all tokens that can form a file path until required `::`
        let mut tokens = self.cursor.collect(
            repeat(of_types(&[
                Identifier,
                Slash,
                IntLiteral,
                FloatLiteral,
                Dot,
                DotDot,
            ]))
            .and_then(of_type(ColonColon)),
        );

        if tokens.is_empty() {
            return Ok(None);
        }

        tokens.pop(); //remove trailing '::' token

        if let Some((head, tail)) = tokens.split_first() {
            let first_path_element = tail.into_iter().fold(head.value, |acc, t| {
                try_join_str(self.source.source, acc, t.value).expect("collect should be adjacent")
            });

            return Ok(Some(first_path_element));
        }

        self.expected(
            "identifier expected",
            ParseErrorKind::Expected("<identifier>".to_string()),
        )
    }

    fn parse_import_with_path(&mut self) -> ParseResult<Import<'a>> {
        let start = self.cursor.peek();
        let mut path = self
            .parse_first_path_element()?
            .map(|i| vec![i])
            .unwrap_or_default();

        path = self.parse_prefix_modules_with(path)?;

        self.cursor.advance(spaces()); //consume spaces
        let token = self.cursor.peek();
        if token.token_type == Star {
            self.cursor.next()?;
            return Ok(Import::AllIn(
                path,
                self.cursor.relative_pos_ctx(start..token),
            ));
        } else if token.token_type == CurlyLeftBracket {
            return self.parse_import_list(start, path).map(Import::List);
        }

        let name = if token.token_type == Identifier {
            self.cursor.next()?;
            token.value
        } else {
            return self.expected(
                "identifier expected",
                ParseErrorKind::Expected("<identifier>".to_string()),
            );
        };

        let alias = self.cursor.advance(
            spaces()
                .then(of_type(As))
                .and_then(spaces())
                .then(of_type(Identifier)),
        );

        let end = alias.clone().unwrap_or(token);

        Ok(Import::Symbol(ImportedSymbol {
            path: path.to_vec(),
            name,
            alias: alias.map(|t| t.value),
            segment: self.cursor.relative_pos_ctx(start..end),
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::aspects::modules::ModulesAspect;
    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::{ParseResult, Parser};
    use ast::r#use::{Import, ImportList, ImportedSymbol, Use};
    use ast::Expr;
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::find_in;
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_use() {
        let source = Source::unknown("use std::foo::bar");
        let result = parse(source.clone()).expect("parser failed");
        assert_eq!(
            result,
            vec![Expr::Use(Use {
                import: Import::Symbol(ImportedSymbol {
                    name: "bar",
                    alias: None,
                    path: vec!["std", "foo"],
                    segment: find_in(source.source, "std::foo::bar"),
                }),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn use_with_file() {
        let source = Source::unknown("use ../foo/bar/asm/std::foo::bar");
        let result = parse(source.clone()).expect("parser failed");
        assert_eq!(
            result,
            vec![Expr::Use(Use {
                import: Import::Symbol(ImportedSymbol {
                    name: "bar",
                    alias: None,
                    path: vec!["../foo/bar/asm/std", "foo"],
                    segment: find_in(source.source, "../foo/bar/asm/std::foo::bar"),
                }),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn wrong_env_name() {
        let source = Source::unknown("use @9");
        let result = Parser::new(source.clone())
            .parse_use()
            .expect_err("no error thrown");
        assert_eq!(
            result,
            ParseError {
                message: "Environment variable name expected.".to_string(),
                kind: ParseErrorKind::Expected("<identifier>".to_string()),
                position: source.source.len()..source.source.len(),
            }
        )
    }

    #[test]
    fn list_use_aliased() {
        let source = Source::unknown("use std::foo::{bar} as X");
        let result = Parser::new(source.clone())
            .parse_use()
            .expect_err("no error thrown");
        assert_eq!(
            result,
            ParseError {
                message: "expected new line or semicolon".to_string(),
                kind: ParseErrorKind::Expected("<new_line> or ';'".to_string()),
                position: source.source.find("as").map(|i| i..i + 2).unwrap(),
            }
        )
    }

    #[test]
    fn use_empty_list() {
        let source = Source::unknown("use {}");
        let result = Parser::new(source.clone())
            .parse_use()
            .expect_err("no error raised");
        assert_eq!(
            result,
            ParseError {
                message: "empty brackets".to_string(),
                kind: ParseErrorKind::Expected("non-empty brackets".to_string()),
                position: source.source.find("{}").map(|i| i..i + 2).unwrap(),
            }
        )
    }

    #[test]
    fn use_all() {
        let source = Source::unknown("use *");
        let result = Parser::new(source.clone())
            .parse_use()
            .expect_err("no error raised");
        assert_eq!(
            result,
            ParseError {
                message: "import all statement needs a symbol prefix.".to_string(),
                kind: ParseErrorKind::Expected("module or file path".to_string()),
                position: source.source.find("*").map(|i| i..i + 1).unwrap(),
            }
        )
    }

    #[test]
    fn use_all_in() {
        let source = Source::unknown("use std::*");
        let result = Parser::new(source.clone()).parse_use().expect("parse fail");
        assert_eq!(
            result,
            Expr::Use(Use {
                import: Import::AllIn(vec!["std"], find_in(source.source, "std::*"),),
                segment: source.segment(),
            })
        )
    }

    #[test]
    fn uses() {
        let source =
            Source::unknown("use \n{std::TOKEN as X,    @A \n , @B \\\n , foo::{my_function}}");
        let result = parse(source.clone()).expect("parser failed");
        assert_eq!(
            result,
            vec![Expr::Use(Use {
                segment: source.segment(),
                import: Import::List(ImportList {
                    path: vec![],
                    segment: find_in(
                        source.source,
                        "{std::TOKEN as X,    @A \n , @B \\\n , foo::{my_function}}"
                    ),
                    imports: vec![
                        Import::Symbol(ImportedSymbol {
                            path: vec!["std"],
                            name: "TOKEN",
                            alias: Some("X"),
                            segment: find_in(source.source, "std::TOKEN as X"),
                        }),
                        Import::Environment("A", find_in(source.source, "@A")),
                        Import::Environment("B", find_in(source.source, "@B")),
                        Import::List(ImportList {
                            path: vec!["foo"],
                            segment: find_in(source.source, "foo::{my_function}"),
                            imports: vec![Import::Symbol(ImportedSymbol {
                                path: vec![],
                                name: "my_function",
                                alias: None,
                                segment: find_in(source.source, "my_function"),
                            }),],
                        }),
                    ],
                })
            })]
        )
    }

    #[test]
    fn use_trailing_comma() {
        let content = "use TOKEN, A";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "expected new line or semicolon".to_string(),
                position: content.find(',').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Expected("<new_line> or ';'".to_string()),
            })
        )
    }

    #[test]
    fn use_empty() {
        let content = "use";
        let source = Source::unknown(content);
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "identifier expected".to_string(),
                position: content.len()..content.len(),
                kind: ParseErrorKind::Expected("<identifier>".to_string()),
            })
        )
    }
}
