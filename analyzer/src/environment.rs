pub mod variables;

use crate::name::Name;
use crate::environment::variables::Variables;


///! The type environment of the analyzer.
///!
///! An environment maps local variable names to their type and keep tracks of scopes.
///! The same variable name can be accessed in different scopes, and can have different type in
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

/// An environment.
/// The Environment contains the defined types, variables, structure and function definitions of a certain scope.
/// It can have dependencies over other environments.
#[derive(Debug, Clone)]
pub struct Environment<'a> {
    ///Fully qualified name of the environment
    pub fqn: Name,

    /// The variables that are declared in the environment.
    pub variables: Variables<'a>,
}

impl<'a> Environment<'a> {
    pub fn named(name: Name) -> Self {
        Self {
            fqn: name.clone(),
            variables: Variables::default(),
        }
    }

    pub fn fork(&self, name: &str) -> Environment {
        let env_fqn = self.fqn.child(name);

        Self {
            fqn: env_fqn,
            variables: Variables::default(),
        }
    }

    pub fn begin_scope(&mut self) {
        self.variables.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.variables.end_scope();
    }
}
