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
use crate::classes::ClassType;
use crate::context::Context;
use crate::types::{Type, TypeScheme, Variable};

/// A variable environment.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Environment {
    /// The type context.
    types: Context,

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
    ty: Variable,

    /// Whether the variable has been initialized.
    is_initialized: bool,

    /// The depth of the scope in which the variable was declared.
    depth: usize,
}

impl Environment {
    pub fn top_level() -> Self {
        let mut env = Self::default();
        env.types.fill_with_builtins();
        env
    }

    /// Resolve a variable name, starting from the current scope and going up.
    ///
    /// If the variable is not in scope, `None` is returned.
    pub fn lookup(&self, key: &str) -> Option<Variable> {
        self.locals
            .iter()
            .rev()
            .find(|local| local.name == key && local.depth <= self.scope_depth)
            .map(|local| local.ty)
    }

    pub fn lookup_type(&self, key: &str) -> Option<Type> {
        self.types.lookup_class_name_type(key)
    }

    pub fn lookup_type_scheme(&self, key: &str) -> Option<TypeScheme> {
        self.lookup(key).and_then(|v| self.types.extract(v))
    }

    pub fn lookup_definition(&self, key: &str) -> Option<&ClassType> {
        self.lookup(key)
            .and_then(|v| self.types.lookup_definition(v))
    }

    /// Start a new scope.
    pub(crate) fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    /// Add a new local variable to the environment.
    ///
    /// The variable will be added to the current scope.
    pub(crate) fn add_local(&mut self, name: &str) -> Variable {
        let ty = self.types.new_variable();
        self.locals.push(Local {
            name: name.to_owned(),
            ty,
            is_initialized: false,
            depth: self.scope_depth,
        });
        ty
    }

    pub(crate) fn define_local(&mut self, name: &str, class: ClassType) {
        let ty = self.types.define(name.to_owned(), class);
        self.locals.push(Local {
            name: name.to_owned(),
            ty,
            is_initialized: true,
            depth: self.scope_depth,
        });
    }

    pub(crate) fn add_reference(&mut self, ty: &Type) -> Variable {
        let var = self.types.new_variable();
        self.types.extend(var, ty.clone());
        var
    }

    pub(crate) fn emit_string(&self) -> Variable {
        self.types
            .lookup_class_name("Str")
            .expect("Str class not found")
    }

    pub(crate) fn emit_int(&self) -> Variable {
        self.types
            .lookup_class_name("Int")
            .expect("Int class not found")
    }

    pub(crate) fn emit_float(&self) -> Variable {
        self.types
            .lookup_class_name("Float")
            .expect("Float class not found")
    }

    pub(crate) fn emit_nil(&self) -> Variable {
        self.types
            .lookup_class_name("Nothing")
            .expect("Nothing class not found")
    }

    /// End the current scope.
    pub(crate) fn end_scope(&mut self) {
        self.scope_depth -= 1;
        self.locals.retain(|local| local.depth <= self.scope_depth);
    }

    pub(crate) fn context(&self) -> &Context {
        &self.types
    }

    pub(crate) fn context_mut(&mut self) -> &mut Context {
        &mut self.types
    }
}
