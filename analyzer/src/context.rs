use crate::types::{Type, Variable};
use indexmap::IndexMap;

/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
    /// A set of constraints mapping from variables to types.
    pub(crate) substitution: IndexMap<Variable, Type>,

    /// A counter used to generate fresh variables.
    next: Variable,
}

impl Context {
    /// Create a new variable from the next unused number.
    pub fn new_variable(&mut self) -> Type {
        self.next += 1;
        Type::Variable(self.next - 1)
    }

    /// Create a new substitution for variable number `v` to the given type `t`.
    pub fn extend(&mut self, v: Variable, t: Type) {
        if v >= self.next {
            self.next = v + 1;
        }
        self.substitution.insert(v, t);
    }
}
