///! The type environment of the compiler.
///!
///! A environment maps local variable names to their types and keep tracks of scopes.
///! The same variable name can be accessed in different scopes, and can have different types in
///! different stack frames. For example:
///! ```text
///! {
///!     // The variable `n` doesn't exist yet.
///!     val n = 9; // Create a new variable `n` with type `int`.
///!     // In this frame, the variable `n` of type `int` is in scope.
///!     {
///!         // The variable `n` exists, and refers to the variable in the outer scope.
///!         val n = "9"; // Create a new variable `n` with type `any` that shadows the outer `n`.
///!         echo $n;
///!         // In this frame, the variable `n` of type `any` is in scope.
///!     }
///!     // In this frame, the variable `n` of type `int` is in scope.
///!     echo $n;
///! }
///! ```
use crate::types::Type;

/// A variable environment.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Environment {
    /// The local variables.
    locals: Vec<Local>,

    /// The current scope depth.
    scope_depth: usize,
}

/// A local variable.
#[derive(Debug, Clone, PartialEq)]
struct Local {
    /// The name of the variable.
    name: String,

    /// The type of the variable.
    ty: Type,

    /// The depth of the scope in which the variable was declared.
    depth: usize,
}

impl Environment {
    /// Resolve a variable name, starting from the current scope and going up.
    ///
    /// If the variable is not in scope, `None` is returned.
    pub fn lookup(&self, key: &str) -> Option<Type> {
        self.locals
            .iter()
            .rev()
            .find(|local| local.name == key && local.depth <= self.scope_depth)
            .map(|local| local.ty.clone())
    }

    /// Start a new scope.
    pub(crate) fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    /// Add a new local variable to the environment.
    ///
    /// The variable will be added to the current scope.
    pub(crate) fn add_local(&mut self, name: &str, hint: Type) {
        self.locals.push(Local {
            name: name.to_owned(),
            ty: hint,
            depth: self.scope_depth,
        });
    }

    /// End the current scope.
    pub(crate) fn end_scope(&mut self) {
        self.scope_depth -= 1;
        self.locals.retain(|local| local.depth <= self.scope_depth);
    }
}
