use ast::operation::BinaryOperator;

/// Gets the name of the method that translates the given operator.
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
