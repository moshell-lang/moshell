#![allow(dead_code)]

///! The parser crate contains the parser for the Moshell scripting language.
mod ast;

use crate::ast::*;
use lexer::token::{Token, TokenType};

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
            _ => self.call(),
        }
    }

    /// Parses a variable declaration.
    fn var_declaration(&mut self, kind: VarKind) -> Result<Expr<'a>, ParseError> {
        let name = self.expect_token(TokenType::Identifier, "Expected variable name.")?;

        let ty = if self.match_token(TokenType::Colon) {
            Some(self.expect_token(TokenType::Identifier, "Expected variable type.")?)
        } else {
            None
        };

        let initializer = if self.match_token(TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
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
        while !self.is_at_end() && !self.match_token(TokenType::NewLine) {
            args.push(self.expression()?);
        }

        Ok(Expr::Call(Call {
            name: name.clone(),
            arguments: args,
        }))
    }

    fn match_token(&mut self, expected: TokenType) -> bool {
        if let Some(actual) = self.tokens.get(self.current) {
            if actual.token_type == expected {
                self.current += 1;
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn expect_token(
        &mut self,
        expected: TokenType,
        message: &str,
    ) -> Result<Token<'a>, ParseError> {
        if let Some(actual) = self.tokens.get(self.current) {
            if actual.token_type == expected {
                self.current += 1;
                Ok(actual.clone())
            } else {
                self.expected(message.to_string())
            }
        } else {
            self.expected(message.to_string())
        }
    }

    fn peek_token(&self) -> Token {
        self.tokens
            .get(self.current)
            .cloned()
            .unwrap_or(Token::new(TokenType::EndOfFile, ""))
    }

    fn next_token(&mut self) -> Result<Token<'a>, ParseError> {
        if let Some(token) = self.tokens.get(self.current) {
            self.current += 1;
            Ok(token.clone())
        } else {
            self.expected("Unexpected end of file.".to_string())
        }
    }

    fn is_at_end(&self) -> bool {
        self.tokens.get(self.current).is_none()
    }

    fn expected(&self, message: String) -> Result<Token<'a>, ParseError> {
        Err(ParseError {
            message,
            //actual: self.peek_token().clone(),
        })
    }
}

fn parse(tokens: Vec<Token>) -> Result<Vec<Expr>, ParseError> {
    Parser::new(tokens).parse()
}

#[cfg(test)]
mod tests {
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
        assert_eq!(parsed, expected);
    }
}
