use ast::r#struct::{FieldDeclaration, StructDeclaration, StructImpl};
use context::source::SourceSegmentHolder;
use lexer::token::TokenType;
use lexer::token::TokenType::CurlyLeftBracket;

use crate::aspects::expr_list::ExpressionListAspect;
use crate::aspects::function_declaration::FunctionDeclarationAspect;
use crate::aspects::r#type::TypeAspect;
use crate::moves::{blanks, of_type, MoveOperations};
use crate::parser::{ParseResult, Parser};

pub trait StructAspect<'a> {
    fn parse_struct(&mut self) -> ParseResult<StructDeclaration<'a>>;

    fn parse_impl(&mut self) -> ParseResult<StructImpl<'a>>;
}

impl<'a> StructAspect<'a> for Parser<'a> {
    fn parse_struct(&mut self) -> ParseResult<StructDeclaration<'a>> {
        let start = self
            .cursor
            .force(of_type(TokenType::Struct), "`struct` expected")?;

        let name = self.cursor.force(
            blanks().then(of_type(TokenType::Identifier)),
            "identifier expected",
        )?;

        self.cursor.advance(blanks());

        let (parameters, _) = self.parse_optional_list(
            TokenType::SquaredLeftBracket,
            TokenType::SquaredRightBracket,
            "Expected type parameter.",
            Parser::parse_type_parameter,
        )?;

        self.cursor.advance(blanks());

        let (fields, body_segment) = self.parse_explicit_list(
            TokenType::CurlyLeftBracket,
            TokenType::CurlyRightBracket,
            "Expected start of structure definition.",
            "Expected field.",
            Parser::parse_field,
        )?;

        let segment_start = start.span.start;
        let segment_end = body_segment.end;
        let segment = segment_start..segment_end;

        Ok(StructDeclaration {
            name: name.text(self.source.source),
            parameters,
            fields,
            segment,
        })
    }

    fn parse_impl(&mut self) -> ParseResult<StructImpl<'a>> {
        let start = self
            .cursor
            .force(of_type(TokenType::Impl), "expected `impl`")?;

        self.cursor.advance(blanks());

        let (type_parameters, _) = self.parse_optional_list(
            TokenType::SquaredLeftBracket,
            TokenType::SquaredRightBracket,
            "Expected type parameter.",
            Parser::parse_type_parameter,
        )?;

        self.cursor.advance(blanks());

        let struct_type = self.parse_type()?;

        self.cursor
            .force(blanks().then(of_type(CurlyLeftBracket)), "expected `{`")?;

        self.cursor.advance(blanks());

        let mut functions = vec![];
        let mut end_token = None;

        while end_token.is_none() {
            self.cursor.advance(blanks());
            functions.push(self.parse_function_declaration()?);

            end_token = self
                .cursor
                .advance(blanks().then(of_type(TokenType::CurlyRightBracket)));
        }

        let segment_start = start.span.start;
        let segment_end = end_token.unwrap().span.end;
        let segment = segment_start..segment_end;

        Ok(StructImpl {
            type_parameters,
            impl_type: struct_type,
            functions,
            segment,
        })
    }
}

impl<'a> Parser<'a> {
    fn parse_field(&mut self) -> ParseResult<FieldDeclaration<'a>> {
        let name = self.cursor.force(
            of_type(TokenType::Identifier),
            "field name identifier expected",
        )?;

        self.cursor
            .force(blanks().then(of_type(TokenType::Colon)), "`:` expected")?;

        self.cursor.advance(blanks());

        let tpe = self.parse_type()?;

        let segment_start = name.span.start;
        let segment_end = tpe.segment().end;
        let segment = segment_start..segment_end;

        Ok(FieldDeclaration {
            name: name.text(self.source.source),
            tpe,
            segment,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;
    use ast::function::FunctionDeclaration;
    use ast::r#struct::{FieldDeclaration, StructDeclaration, StructImpl};
    use ast::r#type::{ParametrizedType, Type, TypeParameter};
    use ast::r#use::InclusionPathItem;
    use ast::value::Literal;
    use ast::Expr;
    use context::source::{Source, SourceSegmentHolder};
    use context::str_find::{find_in, find_in_nth};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_struct_declaration() {
        let src = Source::unknown(
            "struct Foo[A, B] {
                        a: Int,
                        b: String,
                    }",
        );
        let result = parse(src).expect("errors");

        assert_eq!(
            result,
            vec![Expr::StructDeclaration(StructDeclaration {
                name: "Foo",
                parameters: vec![
                    TypeParameter {
                        name: "A",
                        params: vec![],
                        segment: find_in(src.source, "A"),
                    },
                    TypeParameter {
                        name: "B",
                        params: vec![],
                        segment: find_in(src.source, "B"),
                    },
                ],
                fields: vec![
                    FieldDeclaration {
                        name: "a",
                        tpe: Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(
                                "Int",
                                find_in(src.source, "Int")
                            )],
                            params: vec![],
                            segment: find_in(src.source, "Int"),
                        }),
                        segment: find_in(src.source, "a: Int"),
                    },
                    FieldDeclaration {
                        name: "b",
                        tpe: Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(
                                "String",
                                find_in(src.source, "String")
                            )],
                            params: vec![],
                            segment: find_in(src.source, "String"),
                        }),
                        segment: find_in(src.source, "b: String"),
                    },
                ],
                segment: src.segment(),
            })]
        )
    }

    #[test]
    fn test_impl_block() {
        let src = Source::unknown(
            "impl[A] A {
                        fun push() = 0
                        fun len() = 1
                    }",
        );
        let result = parse(src).expect("errors");

        assert_eq!(
            result,
            vec![Expr::Impl(StructImpl {
                type_parameters: vec![TypeParameter {
                    name: "A",
                    params: vec![],
                    segment: find_in(src.source, "A"),
                }],
                impl_type: Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(
                        "A",
                        find_in_nth(src.source, "A", 1)
                    )],
                    params: vec![],
                    segment: find_in_nth(src.source, "A", 1),
                }),
                functions: vec![
                    FunctionDeclaration {
                        name: "push",
                        type_parameters: vec![],
                        parameters: vec![],
                        return_type: None,
                        body: Some(Box::new(Expr::Literal(Literal {
                            parsed: 0.into(),
                            segment: find_in(src.source, "0"),
                        }))),
                        segment: find_in(src.source, "fun push() = 0"),
                    },
                    FunctionDeclaration {
                        name: "len",
                        type_parameters: vec![],
                        parameters: vec![],
                        return_type: None,
                        body: Some(Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(src.source, "1"),
                        }))),
                        segment: find_in(src.source, "fun len() = 1"),
                    },
                ],
                segment: src.segment(),
            })]
        )
    }
}
