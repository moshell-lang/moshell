use crate::import_engine::{ContextExports, ImportEngine};
use crate::layers::ModuleLayers;
use crate::name::Name;
use crate::types::class::TypeClass;
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
use crate::types::context::TypeContext;
use std::cell::RefCell;
use std::rc::Rc;

/// An environment.
/// The Environment contains the defined types, variables, structure and function definitions of a certain scope.
/// It can have dependencies over other environments.
#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    ///Fully qualified name of the environment
    pub fqn: Name,

    ///The import engine, see [[ImportEngine]] for further details.
    pub imports: ImportEngine,

    /// The environment's type context.
    pub type_context: Rc<RefCell<TypeContext>>,
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
    pub fn new(fqn: Name, layers: Rc<RefCell<ModuleLayers>>) -> Self {
        let imports = ImportEngine::new(layers);
        Self {
            imports: imports.clone(),
            type_context: Rc::new(RefCell::new(TypeContext::new(fqn.clone(), imports.fixed()))),
            fqn,
        }
    }

    pub fn lang(layers: Rc<RefCell<ModuleLayers>>) -> Self {
        let imports = ImportEngine::empty(layers);
        Self {
            imports: imports.clone(),
            type_context: TypeContext::lang(imports.fixed()),
            fqn: Name::new("lang"),
        }
    }

    pub(crate) fn fork(env: Rc<RefCell<Environment>>, name: &str) -> Result<Environment, String> {
        let env = env.borrow();
        let identity = env.fqn.child(name);
        let mut imports = env.imports.clone();
        imports.import_all_in(identity.clone())?;

        let type_context = TypeContext::new(identity.clone(), imports.fixed());
        let type_context = Rc::new(RefCell::new(type_context));
        Ok(Self {
            imports,
            type_context,
            fqn: identity,
        })
    }
}
