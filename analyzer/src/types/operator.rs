use ast::operation::BinaryOperator;

pub fn name_operator_method(bin_op: BinaryOperator) -> &'static str {
    use BinaryOperator::*;
    match bin_op {
        Plus => "add",
        Minus => "sub",
        Times => "mul",
        Divide => "div",
        Modulo => "mod",
        And => "and",
        Or => "or",
        EqualEqual => "eq",
        NotEqual => "ne",
        Less => "lt",
        LessEqual => "le",
        Greater => "gt",
        GreaterEqual => "ge",
    }
}
