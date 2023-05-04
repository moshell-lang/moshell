use crate::import_engine::ContextExports;
use crate::name::Name;
use crate::types::class::TypeClass;
use crate::types::context::TypeContext;
use crate::variables::Variables;
use std::cell::RefCell;
use std::rc::Rc;

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
pub struct Environment {
    ///Fully qualified name of the environment
    pub fqn: Name,

    /// The environment's type context.
    pub type_context: Rc<RefCell<TypeContext>>,

    /// The variables that are declared in the environment.
    pub variables: Variables,
}

///All kind of symbols in the environment
pub enum Symbol {
    /// The type class symbol from the type context
    TypeClass(Rc<TypeClass>),
}

/// Top level context implementation for the environment.
impl ContextExports<Symbol> for Environment {
    fn from_env(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        env
    }

    ///Finds the exported symbol.
    /// Types haves priority over other symbols (function, global)
    fn find_exported(&self, name: &Name) -> Option<Symbol> {
        self.type_context
            .borrow()
            .find_exported(name)
            .map(Symbol::TypeClass)
    }

    ///Appends all exported names from the different sub contexts of the environment.
    fn list_exported_names(&self, symbol: Option<Name>) -> Vec<Name> {
        self.type_context.borrow().list_exported_names(symbol)
    }
}

impl Environment {
    pub fn named(name: Name) -> Self {
        Self {
            fqn: name.clone(),
            type_context: Rc::new(RefCell::new(TypeContext::new(name))),
            variables: Variables::default(),
        }
    }

    pub fn fork(&self, name: &str) -> Environment {
        let env_fqn = self.fqn.child(name);

        let type_context = TypeContext::new(env_fqn.clone());
        let type_context = Rc::new(RefCell::new(type_context));
        Self {
            type_context,
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
