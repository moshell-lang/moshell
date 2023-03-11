use crate::range::Iterable;
use crate::Expr;
use dbg_pls::DebugPls;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct If<'a> {
    pub condition: Box<Expr<'a>>,
    pub success_branch: Box<Expr<'a>>,
    pub fail_branch: Option<Box<Expr<'a>>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct While<'a> {
    pub condition: Box<Expr<'a>>,
    pub body: Box<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Loop<'a> {
    pub body: Box<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct For<'a> {
    pub kind: Box<ForKind<'a>>,
    pub body: Box<Expr<'a>>,
}

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
