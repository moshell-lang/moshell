#![allow(dead_code)]

///! The parser crate contains the parser for the Moshell scripting language.
mod ast;
mod parser;


use crate::ast::*;
use lexer::token::{Token, TokenType};
use lexer::token::TokenType::EndOfFile;
use crate::parser::ParseError;


impl<'a> Parser<'a> {
    /// Creates a new parser.
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, current: 0 }
    }







    fn literal(&mut self, token: Token<'a>) -> Result<Expr<'a>, ParseError> {
        Ok(Expr::Literal(Literal { value: token }))
    }




}

fn parse(tokens: Vec<Token>) -> Result<Vec<Expr>, ParseError> {
    Parser::new(tokens).parse()
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
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
