use ast::r#use::{Import, ImportList, ImportedSymbol, InclusionPathItem, Use};
use ast::Expr;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType::{
    As, At, Colon, ColonColon, CurlyLeftBracket, CurlyRightBracket, Dot, Identifier, Reef, Star,
};
use lexer::token::{Token, TokenType};

use crate::err::ParseErrorKind;
use crate::moves::{any, blanks, of_type, of_types, spaces, Move};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    /// Parses a 'use x, y' statement
    pub(crate) fn parse_use(&mut self) -> ParseResult<Expr> {
        let start = self
            .cursor
            .force(of_type(TokenType::Use), "expected 'use'")?;
        self.cursor.advance(blanks());
        let import = self.parse_import()?;

        let import_seg_end = import.segment().end;
        Ok(Expr::Use(Use {
            import,
            segment: start.span.start..import_seg_end,
        }))
    }

    /// Parses identifiers separated between `::` expressions.
    ///
    /// This method stops when it finds an expressions that is not an identifier.
    pub(crate) fn parse_path(&mut self) -> ParseResult<ast::variable::Path> {
        let first = self.parse_path_item()?;
        let mut path = vec![first];
        while let Some(delim) = self.cursor.advance(of_types(&[Colon, ColonColon, Dot])) {
            if delim.token_type != ColonColon {
                self.report_error(self.mk_parse_error(
                    "Expected `::`.",
                    delim.span,
                    ParseErrorKind::Unexpected,
                ));
            }
            let next = self.cursor.peek().token_type;
            if next.is_punctuation() || matches!(next, At | Star) {
                break;
            }
            path.push(self.parse_path_item()?);
        }
        Ok(ast::variable::Path { path })
    }
}

impl Parser<'_> {
    fn parse_path_item(&mut self) -> ParseResult<InclusionPathItem> {
        let start = self.cursor.next()?;

        match start.token_type {
            Identifier => Ok(InclusionPathItem::Symbol(
                ast::variable::Identifier::extract(self.source, start.span),
            )),
            Reef => Ok(InclusionPathItem::Reef(start.span)),
            _ => self.expected_with(
                "Expected identifier or `reef`.",
                start.span,
                ParseErrorKind::Unexpected,
            ),
        }
    }

    fn parse_import(&mut self) -> ParseResult<Import> {
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
                Ok(Import::Environment(ast::variable::Identifier::extract(
                    self.source,
                    env_variable.span,
                )))
            }
            Star => self.expected_with(
                "import all statement needs a symbol prefix.",
                pivot.span,
                ParseErrorKind::Expected("module path".to_string()),
            ),
            CurlyLeftBracket => self.parse_import_list(pivot, vec![]).map(Import::List),

            _ => self.parse_import_with_path(),
        }
    }

    fn parse_import_list(
        &mut self,
        start: Token,
        root: Vec<InclusionPathItem>,
    ) -> ParseResult<ImportList> {
        self.parse_explicit_list(
            CurlyLeftBracket,
            CurlyRightBracket,
            "Expected start of import list.",
            "Expected import.",
            Self::parse_import,
        )
        .and_then(|(imports, s)| {
            if imports.is_empty() {
                return self.expected_with(
                    "empty brackets",
                    start.span.start..self.cursor.peek().span.end,
                    ParseErrorKind::Expected("non-empty brackets".to_string()),
                );
            }
            Ok(ImportList {
                root,
                imports,
                segment: start.span.start..s.end,
            })
        })
    }

    fn parse_import_with_path(&mut self) -> ParseResult<Import> {
        let start = self.cursor.peek();

        let symbol_path = self.parse_path()?;
        self.cursor.advance(spaces()); //consume spaces
        let token = self.cursor.peek();

        if token.token_type == Star || token.token_type == CurlyLeftBracket {
            if token.token_type == Star {
                self.cursor.next()?;

                return Ok(Import::AllIn(
                    symbol_path.path,
                    start.span.start..token.span.end,
                ));
            }
            return self
                .parse_import_list(start, symbol_path.path)
                .map(Import::List);
        }

        let alias = self.cursor.advance(
            spaces()
                .then(of_type(As))
                .and_then(spaces())
                .then(of_type(Identifier)),
        );

        let end = alias.as_ref().map(|t| t.span.end).unwrap_or(
            if let Some(val) = symbol_path.path.last() {
                val.segment().end
            } else {
                self.expected_with(
                    "identifier expected",
                    start.span.start..self.cursor.peek().span.end,
                    ParseErrorKind::Expected("<identifier>".to_owned()),
                )?
            },
        );

        Ok(Import::Symbol(ImportedSymbol {
            path: symbol_path.path,
            alias: alias.map(|t| ast::variable::Identifier::extract(self.source, t.span)),
            segment: start.span.start..end,
        }))
    }

    pub(crate) fn is_path(&self) -> bool {
        self.cursor
            .lookahead(any().then(of_types(&[
                TokenType::ColonColon,
                TokenType::RoundedLeftBracket,
                TokenType::SquaredLeftBracket,
            ])))
            .is_some()
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use pretty_assertions::assert_eq;

    use ast::r#use::{Import, ImportList, ImportedSymbol, InclusionPathItem, Use};
    use ast::variable::{Path, TypedVariable, VarDeclaration, VarKind};
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_in, find_in_nth};

    use crate::err::{ParseError, ParseErrorKind};
    use crate::parse;
    use crate::parser::ParseResult;
    use crate::source::identifier;

    #[test]
    fn simple_use() {
        let source = "use reef::std::foo::bar";
        let result = parse(source).expect("parser failed");
        assert_eq!(
            result,
            vec![Expr::Use(Use {
                import: Import::Symbol(ImportedSymbol {
                    path: vec![
                        InclusionPathItem::Reef(find_in(source, "reef")),
                        InclusionPathItem::Symbol(identifier(source, "std")),
                        InclusionPathItem::Symbol(identifier(source, "foo")),
                        InclusionPathItem::Symbol(identifier(source, "bar")),
                    ],
                    alias: None,
                    segment: find_in(source, "reef::std::foo::bar"),
                }),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn wrong_env_name() {
        let source = "use @9";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Environment variable name expected.".to_string(),
                kind: ParseErrorKind::Expected("<identifier>".to_string()),
                position: source.len() - 1..source.len(),
            })
        )
    }

    #[test]
    fn list_use_aliased() {
        let source = "use std::foo::{bar} as X";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "expected end of expression or file".to_string(),
                kind: ParseErrorKind::Unexpected,
                position: source.find("as").map(|i| i..i + 2).unwrap(),
            })
        )
    }

    #[test]
    fn use_empty_list() {
        let source = "use {}";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "empty brackets".to_string(),
                kind: ParseErrorKind::Expected("non-empty brackets".to_string()),
                position: source.find("{}").map(|i| i..i + 2).unwrap(),
            })
        )
    }

    #[test]
    fn use_all() {
        let source = "use *";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "import all statement needs a symbol prefix.".to_string(),
                kind: ParseErrorKind::Expected("module path".to_string()),
                position: source.find('*').map(|i| i..i + 1).unwrap(),
            })
        )
    }

    #[test]
    fn use_all_in() {
        let source = "use std::*";
        let result = parse(source).expect("parser failed");
        assert_eq!(
            result,
            vec![Expr::Use(Use {
                import: Import::AllIn(
                    vec![InclusionPathItem::Symbol(identifier(source, "std"))],
                    find_in(source, "std::*"),
                ),
                segment: source.segment(),
            })]
        )
    }

    #[test]
    fn uses() {
        let source = "use \nreef::{std::TOKEN as X,    @A \n , @B \\\n , reef::foo::{my_function}}";
        let result = parse(source).expect("parser failed");
        assert_eq!(
            result,
            vec![Expr::Use(Use {
                segment: source.segment(),
                import: Import::List(ImportList {
                    root: vec![InclusionPathItem::Reef(find_in(source, "reef"))],
                    segment: find_in(
                        source,
                        "reef::{std::TOKEN as X,    @A \n , @B \\\n , reef::foo::{my_function}}"
                    ),
                    imports: vec![
                        Import::Symbol(ImportedSymbol {
                            path: vec![
                                InclusionPathItem::Symbol(identifier(source, "std")),
                                InclusionPathItem::Symbol(identifier(source, "TOKEN")),
                            ],
                            alias: Some(identifier(source, "X")),
                            segment: find_in(source, "std::TOKEN as X"),
                        }),
                        Import::Environment(identifier(source, "A")),
                        Import::Environment(identifier(source, "B")),
                        Import::List(ImportList {
                            root: vec![
                                InclusionPathItem::Reef(find_in_nth(source, "reef", 1)),
                                InclusionPathItem::Symbol(identifier(source, "foo")),
                            ],
                            segment: find_in(source, "reef::foo::{my_function}"),
                            imports: vec![Import::Symbol(ImportedSymbol {
                                path: vec![InclusionPathItem::Symbol(identifier(
                                    source,
                                    "my_function"
                                ))],
                                alias: None,
                                segment: find_in(source, "my_function"),
                            })],
                        }),
                    ],
                })
            })]
        )
    }

    #[test]
    fn use_trailing_comma() {
        let source = "use TOKEN, A";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "expected end of expression or file".to_string(),
                position: source.find(',').map(|p| p..p + 1).unwrap(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn use_empty() {
        let source = "use";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Err(ParseError {
                message: "Expected identifier or `reef`.".to_owned(),
                position: source.len()..source.len(),
                kind: ParseErrorKind::Unexpected,
            })
        )
    }

    #[test]
    fn inline_path_in_var() {
        let source = "val x = reef::math::tau";
        let result: ParseResult<_> = parse(source).into();
        assert_eq!(
            result,
            Ok(vec![Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Val,
                var: TypedVariable {
                    name: identifier(source, "x"),
                    ty: None,
                },
                initializer: Some(Box::new(Expr::Path(Path {
                    path: vec![
                        InclusionPathItem::Reef(find_in(source, "reef")),
                        InclusionPathItem::Symbol(identifier(source, "math")),
                        InclusionPathItem::Symbol(identifier(source, "tau")),
                    ],
                }))),
                segment: source.segment(),
            })])
        )
    }
}
