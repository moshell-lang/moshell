#![allow(dead_code)]

///! The parser crate contains the parser for the Moshell scripting language.
mod ast;

use crate::ast::*;
use lexer::token::{Token, TokenType};
use lexer::token::TokenType::EndOfFile;

/// An error that occurs during parsing.
#[derive(Debug)]
struct ParseError {
    message: String,
    //actual: Token<'a>,
}

/// A parser for the Moshell scripting language.
pub struct Parser<'a> {
    /// The tokens to be parsed.
    tokens: Vec<Token<'a>>,
    /// The current position in the tokens.
    current: usize,
}

impl<'a> Parser<'a> {
    /// Creates a new parser.
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, current: 0 }
    }

    /// Parses the tokens into an abstract syntax tree.
    fn parse(&mut self) -> Result<Vec<Expr<'a>>, ParseError> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.expression()?);
        }

        Ok(statements)
    }

    /// Parses an expression.
    fn expression(&mut self) -> Result<Expr<'a>, ParseError> {
        let token = self.next_token()?;
        match token.token_type {
            TokenType::Var => self.var_declaration(VarKind::Var),
            TokenType::Val => self.var_declaration(VarKind::Val),
            TokenType::IntLiteral => self.literal(token),
            //TODO add other expression parsers
            _ => self.call(),
        }
    }

    /// Parses a variable declaration.
    fn var_declaration(&mut self, kind: VarKind) -> Result<Expr<'a>, ParseError> {
        let name = self.expect_token(TokenType::Identifier, "Expected variable name.")?;

        let ty = match self.match_token(TokenType::Colon) {
            None => None,
            Some(_) => Some(self.expect_token(TokenType::Identifier, "Expected variable type")?),
        };

        let initializer = match self.match_token(TokenType::Equal) {
            None => None,
            Some(_) => Some(self.expression()?)
        };

        Ok(Expr::VarDeclaration(VarDeclaration {
            kind,
            var: TypedVariable {
                name: name.clone(),
                ty,
            },
            initializer: initializer.map(Box::new),
        }))
    }

    fn literal(&mut self, token: Token<'a>) -> Result<Expr<'a>, ParseError> {
        Ok(Expr::Literal(Literal { value: token }))
    }

    fn call(&mut self) -> Result<Expr<'a>, ParseError> {
        let name = self.expect_token(TokenType::Identifier, "Expected command name.")?;

        let mut args = Vec::new();
        while !self.is_at_end() && !self.exists_token(TokenType::NewLine) {
            args.push(self.expression()?);
        }

        Ok(Expr::Call(Call {
            name: name.clone(),
            arguments: args,
        }))
    }

    fn exists_token(&mut self, expected: TokenType) -> bool {
        let token = self.peek_token();
        if token.token_type == expected {
            self.current += 1;
            return true;
        }
        false
    }

    fn match_token(&mut self, expected: TokenType) -> Option<Token<'a>> {
        let token = self.peek_token();
        if token.token_type == expected {
            self.current += 1;
            return Some(token.clone());
        }
        None
    }

    fn expect_token(&mut self, expected: TokenType, message: &str) -> Result<Token<'a>, ParseError> {
        self.match_token(expected).ok_or_else(|| self.mk_parse_error(message))
    }

    fn peek_token(&self) -> Token<'a> {
        self.tokens.get(self.current).cloned().unwrap()
    }

    fn next_token(&mut self) -> Result<Token<'a>, ParseError> {
        let token = self.peek_token();
        if token.token_type == EndOfFile {
            self.current += 1;
            Ok(token.clone())
        } else {
            self.expected("Unexpected end of file.")
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek_token().token_type == EndOfFile
    }

    fn expected(&self, message: &str) -> Result<Token<'a>, ParseError> {
        Err(self.mk_parse_error(message))
    }

    fn mk_parse_error(&self, message: &str) -> ParseError {
        ParseError {
            message: message.to_string(),
            //actual: self.peek_token().clone(),
        }
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
