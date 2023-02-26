use lexer::token::Token;
use crate::ast::operation::{ARITHMETICS, BinaryOperator, BOOLEANS, COMPARISONS};
use crate::moves::{AndThenMove, eox, Move, OrMove, PredicateMove};

#[derive(Clone)]
pub struct ParserContext<M: Move + Copy> {
    pub allowed_operators: Vec<BinaryOperator>,
    pub parsing_value: bool,
    pub eox: M,
}

impl Default for ParserContext<OrMove<
    OrMove<
        AndThenMove<
            PredicateMove<for<'a> fn(Token<'a>) -> bool>,
            PredicateMove<for<'a> fn(Token<'a>) -> bool>
        >,
        PredicateMove<for<'a> fn(Token<'a>) -> bool>
    >,
    PredicateMove<for<'a> fn(Token<'a>) -> bool>
>> {
    fn default() -> Self {
        let x: () = Self {
            allowed_operators: [BOOLEANS].concat(),
            parsing_value: false,
            eox: eox(),
        };
        todo!()
    }
}


impl<A: Move + Copy> ParserContext<A> {
    pub fn value_hold() -> ParserContext<OrMove<
        OrMove<
            AndThenMove<
                PredicateMove<for<'a> fn(Token<'a>) -> bool>,
                PredicateMove<for<'a> fn(Token<'a>) -> bool>
            >,
            PredicateMove<for<'a> fn(Token<'a>) -> bool>
        >,
        PredicateMove<for<'a> fn(Token<'a>) -> bool>
    >> {
        ParserContext {
            allowed_operators: [ARITHMETICS, COMPARISONS, BOOLEANS].concat(),
            parsing_value: true,
            eox: eox(),
        }
    }

    pub fn with_parsing_value(&self, parsing_value: bool) -> ParserContext<A> {
        return Self {
            parsing_value,
            allowed_operators: self.allowed_operators.clone(),
            eox: self.eox,
        }
    }

    pub fn with_allowed_ops(&self, allowed_operators: Vec<BinaryOperator>) -> ParserContext<A> {
        return Self {
            parsing_value: self.parsing_value,
            allowed_operators,
            eox: self.eox,
        }
    }

    pub fn with_eox<B: Move + Copy>(&self, eox: B) -> ParserContext<B> {
        return Self {
            parsing_value: self.parsing_value,
            allowed_operators: self.allowed_operators.clone(),
            eox,
        }
    }
}
