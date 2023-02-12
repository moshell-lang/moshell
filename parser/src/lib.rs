#![allow(dead_code)]

use lexer::token::Token;

use crate::ast::*;
use crate::parser::{ParseResult, Parser};

///! The parser crate contains the parser for the Moshell scripting language.
mod aspects;
mod ast;
mod parser;

pub fn parse(tokens: Vec<Token>) -> ParseResult<Vec<Expr>> {
    Parser::new(tokens).parse()
}

#[cfg(test)]
mod tests {
    use lexer::token::TokenType;

    use super::*;

    #[test]
    fn test_parser() {
        let tokens = vec![
            Token::new(TokenType::Var, "var"),
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
                value: Token::new(TokenType::IntLiteral, "1"),
            }))),
        })];
        assert_eq!(expected, parsed);
    }
}
