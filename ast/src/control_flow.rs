use src_macros::segment_holder;

use crate::variable::Identifier;
use crate::Expr;

///An if statement
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct If {
    ///The if condition expression
    pub condition: Box<Expr>,
    ///The executed branch if the condition succeeds
    pub success_branch: Box<Expr>,
    ///The 'else' branch if the condition fails
    pub fail_branch: Option<Box<Expr>>,
}

///A while loop statement
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct While {
    ///The while condition expression
    pub condition: Box<Expr>,
    ///The loop's body expression
    pub body: Box<Expr>,
}

///A loop statement
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct Loop {
    ///The loop's body expression
    pub body: Box<Expr>,
}

/// A for loop.
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct For {
    /// The type of the for loop.
    pub kind: Box<ForKind>,
    /// The body of the for loop.
    pub body: Box<Expr>,
}

/// A for loop can be either a range loop or a conditional loop.
#[derive(Debug, Clone, PartialEq)]
pub enum ForKind {
    Range(RangeFor),
    Conditional(ConditionalFor),
}

/// A for in range loop, e.g. `for i in 1..10; ...`.
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct RangeFor {
    /// The variable name that will be used in the loop to designate the current item.
    pub receiver: Identifier,
    /// The range of values that will be iterated over.
    pub iterable: Expr,
}

/// A for in conditional loop, e.g. `for (( i = 0; i < 10; i++ )); ...`.
#[segment_holder]
#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalFor {
    /// The initialization expression.
    pub initializer: Expr,
    /// The condition expression.
    pub condition: Expr,
    /// The increment expression.
    pub increment: Expr,
}
