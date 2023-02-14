#![allow(dead_code)]

use crate::ast::Expr;
use lexer::token::Token;

use crate::parser::{ParseResult, Parser};

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
pub mod ast;
mod parser;

pub fn parse(tokens: Vec<Token>) -> ParseResult<Vec<Expr>> {
    Parser::new(tokens).parse()
}

#[cfg(test)]
mod tests {
    use crate::ast::callable::Call;
    use crate::ast::literal::{Literal, LiteralValue};
    use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind};
    use lexer::token::TokenType;

    use super::*;

    #[test]
    fn variable_type_and_initializer() {
        let tokens = vec![
            Token::new(TokenType::Var, "var"),
            Token::new(TokenType::Space, " "),
            Token::new(TokenType::Identifier, "a"),
            Token::new(TokenType::Colon, ":"),
            Token::new(TokenType::Identifier, "int"),
            Token::new(TokenType::Equal, "="),
            Token::new(TokenType::IntLiteral, "1"),
        ];
        let parsed = parse(tokens).expect("Failed to parse");

        let expected = vec![Expr::VarDeclaration(VarDeclaration {
            kind: VarKind::Var,
            var: TypedVariable {
                name: Token::new(TokenType::Identifier, "a"),
                ty: Some(Token::new(TokenType::Identifier, "int")),
            },
            initializer: Some(Box::new(Expr::Literal(Literal {
                token: Token::new(TokenType::IntLiteral, "1"),
                parsed: LiteralValue::Int(1),
            }))),
        })];
        assert_eq!(expected, parsed);
    }

    #[test]
    fn command_echo() {
        let tokens = vec![
            Token::new(TokenType::Identifier, "echo"),
            Token::new(TokenType::Quote, "'"),
            Token::new(TokenType::Identifier, "hello"),
            Token::new(TokenType::Quote, "'"),
        ];
        let parsed = parse(tokens).expect("Failed to parse");

        let expected = vec![Expr::Call(Call {
            arguments: vec![
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Identifier, "echo"),
                    parsed: LiteralValue::String("echo".to_string()),
                }),
                Expr::Literal(Literal {
                    token: Token::new(TokenType::Quote, "'"),
                    parsed: LiteralValue::String("hello".to_string()),
                }),
            ],
        })];
        assert_eq!(expected, parsed);
    }
}
