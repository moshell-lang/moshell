#![allow(dead_code)]

use lexer::token::Token;

use crate::ast::*;
use crate::parser::{Parser, ParseResult};

///! The parser crate contains the parser for the Moshell scripting language.
mod ast;
mod parser;
mod aspects;


fn parse(tokens: Vec<Token>) -> ParseResult<Vec<Expr>> {
    Parser::new(tokens).parse()
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use lexer::token::TokenType;

    use super::*;

    #[test]
    fn test_parser() {
        let tokens = lex("var a: int = 1");
        let parsed = parse(tokens).expect("Failed to parse");

        let _expected = vec![
            Expr::VarDeclaration(VarDeclaration {
                kind: VarKind::Var,
                var: TypedVariable {
                    name: Token::new(TokenType::Identifier, "a"),
                    ty: Some(Token::new(TokenType::Identifier, "int")),
                },
                initializer: Some(Box::new(Expr::Literal(Literal {
                    value: Token::new(TokenType::IntLiteral, "1"),
                }))),
            }),
        ];
        //assert_eq!(_expected, parsed);
        println!("{:?}", parsed);
    }
}
