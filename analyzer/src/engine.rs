use crate::environment::Environment;
use crate::name::Name;

use crate::relations::SourceObjectId;
use ast::Expr;

/// Owns references to the global AST and its environments.
#[derive(Debug, Default)]
pub struct Engine<'a> {
    /// The engine has the ownership of the AST.
    #[allow(clippy::vec_box)]
    // Box is used to ensure that the reference behind is still valid after vector's realloc
    asts: Vec<Box<Expr<'a>>>,

    /// Associates a module id to the corresponding environment.
    ///
    /// Those are origins of symbols that are available locally in the environment,
    /// which may also be the source of unresolved symbols, tracked in the Relations.
    origins: Vec<(&'a Expr<'a>, Box<Environment>)>,
}

impl<'a> Engine<'a> {
    /// Takes ownership of an expression and returns a reference to it.
    pub fn take(&mut self, ast: Expr<'a>) -> &'a Expr<'a> {
        self.asts.push(Box::new(ast));
        unsafe {
            // SAFETY: Assume for now that expressions are never removed from the engine.
            // The reference behind Box does not change and is valid for the lifetime of the engine.
            std::mem::transmute::<&Expr<'a>, &'a Expr<'a>>(self.asts.last().unwrap())
        }
    }

    ///Returns an iterator over environments contained in engine
    pub fn environments(&self) -> impl Iterator<Item = (SourceObjectId, &Environment)> {
        self.origins
            .iter()
            .enumerate()
            .map(|(id, (_, env))| (SourceObjectId(id), env.as_ref()))
    }

    /// Adds a new origin to the engine and returns its given id.
    ///
    /// The same environment can be retrieved by its id, using [`Engine::get_environment`].
    pub fn track(&mut self, ast: &'a Expr<'a>, env: Environment) -> SourceObjectId {
        let id = self.origins.len();
        self.origins.push((ast, Box::new(env)));
        SourceObjectId(id)
    }

    ///Finds an environment by its fully qualified name.
    pub fn find_environment_by_name(&self, name: &Name) -> Option<(SourceObjectId, &Environment)> {
        self.origins
            .iter()
            .enumerate()
            .find(|(_, (_, env))| &env.fqn == name)
            .map(|(idx, (_, env))| (SourceObjectId(idx), env.as_ref()))
    }

    /// Gets an environment by its identifier.
    pub fn get_environment(&self, id: SourceObjectId) -> Option<&Environment> {
        self.origins.get(id.0).map(|(_, env)| env.as_ref())
    }

    /// Gets a mutable environment by its object id.
    ///
    /// If the environment is present, if it was just inserted for instance, it can be
    /// safely unwrapped.
    pub fn get_mut(&mut self, id: SourceObjectId) -> Option<&'a mut Environment> {
        self.origins.get_mut(id.0).map(|(_, env)| unsafe {
            // SAFETY: Assume that expressions are never removed from the engine and
            // that the reference behind Box does not change.
            std::mem::transmute::<&mut Environment, &'a mut Environment>(env)
        })
    }

    /// Gets the next available id for a new origin.
    pub fn peek_id(&self) -> SourceObjectId {
        SourceObjectId(self.origins.len())
    }
}
