use crate::types::context::TypeContext;
use std::cell::RefCell;
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
use std::default::Default;
use std::rc::Rc;

/// An environment.
/// The Environment contains the defined types, variables, structure and function definitions of a certain scope.
/// It can have dependencies over other dependences.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Environment<'a> {
    /// The type context.
    pub type_context: Rc<RefCell<TypeContext>>,

    ///All the direct dependencies of the environment.
    dependencies: Vec<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn lang() -> Self {
        Self {
            type_context: TypeContext::lang(),
            ..Self::default()
        }
    }

    pub(crate) fn fork(&'a self) -> Environment<'a> {
        Self {
            type_context: Rc::new(RefCell::new(TypeContext::fork(self.type_context.clone()))),
            dependencies: vec![self],
            ..Self::default()
        }
    }
}
