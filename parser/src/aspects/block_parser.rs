use lexer::token::TokenType;
use crate::aspects::base_parser::BaseParser;
use crate::ast::Expr;
use crate::parser::{Parser, ParseResult};
use crate::ast::statement::Block;

pub trait BlockParser<'a> {
    fn block(&mut self) -> ParseResult<Expr<'a>>;
}

impl<'a> BlockParser<'a> for Parser<'a> {
    fn block(&mut self) -> ParseResult<Expr<'a>> {
        self.expect_token(TokenType::CurlyLeftBracket, "expected start of block expression")?;
        let mut expressions: Vec<Expr<'a>> = Vec::new();
        loop {
            if self.meet_token(TokenType::CurlyRightBracket) {
                break;
            }

            let expression = self.statement()?;
            expressions.push(expression);
        };
        Ok(Expr::Block(Block {
            exprs: expressions
        }))
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::lex;
    use lexer::token::{Token, TokenType};
    use crate::aspects::base_parser::BaseParser;
    use crate::aspects::block_parser::BlockParser;
    use crate::ast::Expr;
    use crate::ast::literal::Literal;
    use crate::ast::literal::LiteralValue::{Float, Int};
    use crate::ast::statement::Block;
    use crate::ast::variable::{TypedVariable, VarDeclaration, VarKind};
    use crate::parser::Parser;

    #[test]
    fn test_empty_block() {
        let tokens = lex("{}");
        let mut parser = Parser::new(tokens);
        let ast = parser.block().expect("failed to parse block");
        assert!(parser.is_at_end());
        assert_eq!(ast, Expr::Block(Block {
            exprs: vec![]
        }))
    }

    #[test]
    fn test_block() {
        let tokens = lex("\
        {\
            var test: int = 7.0\
            val x = 8\
        }\
        ");
        let mut parser = Parser::new(tokens);
        let ast = parser.block().expect("failed to parse block");
        assert!(parser.is_at_end());
        assert_eq!(ast, Expr::Block(Block {
            exprs: vec![
                Expr::VarDeclaration(VarDeclaration {
                    kind: VarKind::Var,
                    var: TypedVariable {
                        name: Token::new(TokenType::Identifier, "test"),
                        ty: Some(Token::new(TokenType::Identifier, "int")),
                    },
                    initializer: Some(Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::FloatLiteral, "7.0"),
                        parsed: Float(7.0),
                    }))),
                }),
                Expr::VarDeclaration(VarDeclaration {
                    kind: VarKind::Val,
                    var: TypedVariable {
                        name: Token::new(TokenType::Identifier, "x"),
                        ty: None,
                    },
                    initializer: Some(Box::new(Expr::Literal(Literal {
                        token: Token::new(TokenType::IntLiteral, "8"),
                        parsed: Int(8),
                    }))),
                }),
            ]
        }))
    }
}