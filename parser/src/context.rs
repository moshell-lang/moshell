use lexer::token::TokenType;

use crate::ast::operation::{ARITHMETICS, BinaryOperator, BOOLEANS, COMPARISONS};

#[derive(Clone)]
pub struct ParserContext {
    pub allowed_operators: Vec<BinaryOperator>,
    pub parsing_value: bool,
    pub enclosing_end: Option<TokenType>,
}

impl Default for ParserContext {
    fn default() -> Self {
        Self {
            allowed_operators: [BOOLEANS].concat(),
            parsing_value: false,
            enclosing_end: None,
        }
    }
}


impl ParserContext {
    pub fn value_hold() -> Self {
        Self {
            allowed_operators: [ARITHMETICS, COMPARISONS, BOOLEANS].concat(),
            parsing_value: true,
            enclosing_end: None,
        }
    }

    pub fn with_parsing_value(&self, parsing_value: bool) -> Self {
        return Self {
            parsing_value,
            allowed_operators: self.allowed_operators.clone(),
            enclosing_end: self.enclosing_end,
        }
    }

    pub fn with_allowed_ops(&self, allowed_operators: Vec<BinaryOperator>) -> ParserContext {
        return Self {
            parsing_value: self.parsing_value,
            allowed_operators,
            enclosing_end: self.enclosing_end,
        }
    }

    pub fn with_enclosing_end(&self, enclosing_end: Option<TokenType>) -> ParserContext {
        return Self {
            parsing_value: self.parsing_value,
            allowed_operators: self.allowed_operators.clone(),
            enclosing_end,
        }
    }
}
