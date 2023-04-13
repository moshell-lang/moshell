///! The type environment of the compiler.
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
use crate::types::context::TypeContext;
use std::cell::RefCell;
use std::default::Default;
use std::rc::Rc;
use crate::imports::{EnvImport};

/// An environment.
/// The Environment contains the defined types, variables, structure and function definitions of a certain scope.
/// It can have dependencies over other dependencies.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Environment {
    name: String,

    /// The environment's type context.
    pub type_context: Rc<RefCell<TypeContext>>,

    ///All the direct dependencies of the environment.
    imports: Vec<EnvImport>,
}

impl Environment {
    pub fn lang() -> Self {
        Self {
            type_context: TypeContext::lang(),
            name: "lang".to_string(),
            ..Self::default()
        }
    }

    pub(crate) fn fork(env: Rc<RefCell<Environment>>) -> Environment {
        let mut fork_imports = env.borrow().imports.clone();
        fork_imports.push(EnvImport::all(env));
        Self {
            type_context: Rc::new(RefCell::new(TypeContext::default())),
            imports: fork_imports,
            ..Self::default()
        }
    }

}
