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
use crate::identity::Identity;
use crate::module::ModuleLayers;

/// An environment.
/// The Environment contains the defined types, variables, structure and function definitions of a certain scope.
/// It can have dependencies over other dependencies.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Environment {
    pub identity: Identity,

    /// The environment's type context.
    pub type_context: Rc<RefCell<TypeContext>>,

}

impl Environment {
    pub fn new(fqn: Identity, layers: Rc<RefCell<ModuleLayers>>) -> Self {
        Self {
            type_context: Rc::new(RefCell::new(TypeContext::new(fqn.clone(), layers))),
            identity: fqn,
        }
    }
    pub fn lang(layers: Rc<RefCell<ModuleLayers>>) -> Self {
        Self {
            type_context: TypeContext::lang(layers),
            identity: Identity {
                absolute_path: Vec::new(),
                name: "lang".to_string(),
            },
        }
    }

    pub(crate) fn fork(env: Rc<RefCell<Environment>>, name: &str) -> Environment {
        Self {
            type_context: Rc::new(RefCell::new(TypeContext::fork(env.borrow().type_context.clone(), name))),
            identity: env.borrow().identity.child(name)
        }
    }
}

pub trait EnvironmentContext<V> {
    fn from_env(env: &Environment) -> Rc<RefCell<Self>>;

    fn find(&self, name: &str) -> Option<V>;
}
