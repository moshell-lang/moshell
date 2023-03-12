use crate::range::Iterable;
use crate::Expr;
use dbg_pls::DebugPls;

///An if statement
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct If<'a> {
    ///The if condition expression
    pub condition: Box<Expr<'a>>,
    ///The executed branch if the condition succeeds
    pub success_branch: Box<Expr<'a>>,
    ///The 'else' branch if the condition fails
    pub fail_branch: Option<Box<Expr<'a>>>,
}

///A while loop statement
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct While<'a> {
    ///The while condition expression
    pub condition: Box<Expr<'a>>,
    ///The loop's body expression
    pub body: Box<Expr<'a>>,
}

///A loop statement
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Loop<'a> {
    ///The loop's body expression
    pub body: Box<Expr<'a>>,
}

/// A for loop.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct For<'a> {
    /// The type of the for loop.
    pub kind: Box<ForKind<'a>>,
    /// The body of the for loop.
    pub body: Box<Expr<'a>>,
}

/// A for loop can be either a range loop or a conditional loop.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub enum ForKind<'a> {
    Range(RangeFor<'a>),
    Conditional(ConditionalFor<'a>),
}

/// A for in range loop, e.g. `for i in 1..10; ...`.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct RangeFor<'a> {
    /// The variable name that will be used in the loop to designate the current item.
    pub receiver: &'a str,
    /// The range of values that will be iterated over.
    pub iterable: Iterable<'a>,
}

/// A for in conditional loop, e.g. `for (( i = 0; i < 10; i++ )); ...`.
#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct ConditionalFor<'a> {
    /// The initialization expression.
    pub initializer: Expr<'a>,
    /// The condition expression.
    pub condition: Expr<'a>,
    /// The increment expression.
    pub increment: Expr<'a>,
}
