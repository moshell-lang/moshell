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
use crate::context::TypeContext;
use crate::types::Type;
use std::default::Default;
use std::rc::Rc;
use crate::types::Type::Unknown;

/// An environment.
/// The Environment contains the defined types, variables, structure and function definitions of a certain scope.
/// It can have dependencies over other dependences.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Environment<'a> {
    identity: usize,

    /// The type context.
    pub(crate) types: TypeContext<'a>,

    /// The local variables.
    locals: Vec<Local>,

    ///All the direct dependencies of the environment.
    dependencies: Vec<&'a Environment<'a>>
}

/// A local variable.
#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    /// The name of the variable.
    pub name: String,

    /// The type of the variable.
    pub ty: Type,

    /// Whether the variable has been initialized.
    pub is_initialized: bool,
}

impl<'a> Environment<'a> {
    pub fn lang() -> Self {
        Self {
            types: TypeContext::lang(),
            ..Self::default()
        }
    }

    /// Resolve a variable name, starting from the current scope and going up.
    ///
    /// If the variable is not in scope, `None` is returned.
    pub fn lookup_local(&self, name: &str) -> Option<&Local> {
        self.locals
            .iter()
            .find(|local| local.name == name)
            .or_else(|| {
                let iter = self.dependencies.iter();
                for dep in iter {
                    if let Some(local) = dep.lookup_local(name) {
                        return Some(local)
                    }
                }
                None
            })
    }


    /// Add a new local variable to the environment.
    ///
    /// The variable will be added to the current scope.
    pub(crate) fn set_local(&mut self, name: &str) {
        self.locals.push(Local {
            name: name.to_owned(),
            ty: Unknown,
            is_initialized: false,
        })
    }

    pub(crate) fn define_local(&mut self, name: &str, tpe: Type) {
        self.locals.push(Local {
            name: name.to_owned(),
            ty: tpe,
            is_initialized: true,
        });
    }


    pub(crate) fn context(&self) -> &TypeContext {
        &self.types
    }

    pub(crate) fn fork(&'a self) -> Environment<'a> {
        Self {
            types: self.types.fork(),
            dependencies: vec!(self),
            ..Self::default()
        }
    }

}
