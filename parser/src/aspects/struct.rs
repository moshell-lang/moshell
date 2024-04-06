use ast::r#struct::{FieldDeclaration, StructDeclaration, StructImpl};
use ast::variable::Identifier;
use context::source::SourceSegmentHolder;
use lexer::token::TokenType;

use crate::moves::{blanks, eog, line_end, of_type, MoveOperations};
use crate::parser::{ParseResult, Parser};

impl Parser<'_> {
    pub(crate) fn parse_struct(&mut self) -> ParseResult<StructDeclaration> {
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
            name: Identifier::extract(self.source, name.span),
            parameters,
            fields,
            segment,
        })
    }

    pub(crate) fn parse_impl(&mut self) -> ParseResult<StructImpl> {
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

        self.cursor.force(
            blanks().then(of_type(TokenType::CurlyLeftBracket)),
            "expected `{`",
        )?;

        self.cursor.advance(blanks());

        let mut functions = vec![];
        let mut end_token = None;

        while end_token.is_none() {
            self.cursor.advance(blanks());
            match self.parse_function_declaration() {
                Ok(function) => functions.push(function),
                Err(err) => self.recover_from(err, eog()),
            }
            self.cursor.advance(line_end());

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

    fn parse_field(&mut self) -> ParseResult<FieldDeclaration> {
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
            name: Identifier::extract(self.source, name.span),
            tpe,
            segment,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;
    use crate::source::{identifier, identifier_nth};
    use ast::function::FunctionDeclaration;
    use ast::r#struct::{FieldDeclaration, StructDeclaration, StructImpl};
    use ast::r#type::{ParametrizedType, Type, TypeParameter};
    use ast::r#use::InclusionPathItem;
    use ast::value::Literal;
    use ast::Expr;
    use context::source::SourceSegmentHolder;
    use context::str_find::{find_in, find_in_nth};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_struct_declaration() {
        let src = "struct Foo[A, B] {
                        a: Int,
                        b: String,
                    }";
        let result = parse(src).expect("errors");

        assert_eq!(
            result,
            vec![Expr::StructDeclaration(StructDeclaration {
                name: identifier(src, "Foo"),
                parameters: vec![
                    TypeParameter {
                        name: identifier(src, "A"),
                        params: vec![],
                        segment: find_in(src, "A"),
                    },
                    TypeParameter {
                        name: identifier(src, "B"),
                        params: vec![],
                        segment: find_in(src, "B"),
                    },
                ],
                fields: vec![
                    FieldDeclaration {
                        name: identifier(src, "a"),
                        tpe: Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(src, "Int"))],
                            params: vec![],
                            segment: find_in(src, "Int"),
                        }),
                        segment: find_in(src, "a: Int"),
                    },
                    FieldDeclaration {
                        name: identifier(src, "b"),
                        tpe: Type::Parametrized(ParametrizedType {
                            path: vec![InclusionPathItem::Symbol(identifier(src, "String"))],
                            params: vec![],
                            segment: find_in(src, "String"),
                        }),
                        segment: find_in(src, "b: String"),
                    },
                ],
                segment: src.segment(),
            })]
        )
    }

    #[test]
    fn test_impl_block() {
        let src = "impl[A] A {
                        fun push() = 0
                        fun len() = 1
                    }";
        let result = parse(src).expect("errors");

        assert_eq!(
            result,
            vec![Expr::Impl(StructImpl {
                type_parameters: vec![TypeParameter {
                    name: identifier(src, "A"),
                    params: vec![],
                    segment: find_in(src, "A"),
                }],
                impl_type: Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier_nth(src, "A", 1))],
                    params: vec![],
                    segment: find_in_nth(src, "A", 1),
                }),
                functions: vec![
                    FunctionDeclaration {
                        name: identifier(src, "push"),
                        type_parameters: vec![],
                        parameters: vec![],
                        return_type: None,
                        body: Some(Box::new(Expr::Literal(Literal {
                            parsed: 0.into(),
                            segment: find_in(src, "0"),
                        }))),
                        segment: find_in(src, "fun push() = 0"),
                    },
                    FunctionDeclaration {
                        name: identifier(src, "len"),
                        type_parameters: vec![],
                        parameters: vec![],
                        return_type: None,
                        body: Some(Box::new(Expr::Literal(Literal {
                            parsed: 1.into(),
                            segment: find_in(src, "1"),
                        }))),
                        segment: find_in(src, "fun len() = 1"),
                    },
                ],
                segment: src.segment(),
            })]
        )
    }

    #[test]
    fn method_accepts_semi() {
        let source = "impl A { fun test(); }";
        let result = parse(source).expect("errors");
        assert_eq!(
            result,
            vec![Expr::Impl(StructImpl {
                type_parameters: vec![],
                impl_type: Type::Parametrized(ParametrizedType {
                    path: vec![InclusionPathItem::Symbol(identifier(source, "A"))],
                    params: vec![],
                    segment: find_in(source, "A"),
                }),
                functions: vec![FunctionDeclaration {
                    name: identifier(source, "test"),
                    type_parameters: vec![],
                    parameters: vec![],
                    return_type: None,
                    body: None,
                    segment: find_in(source, "fun test();"),
                }],
                segment: source.segment(),
            })]
        )
    }
}
