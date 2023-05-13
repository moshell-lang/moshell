use analyzer_system::environment::Environment;
use analyzer_system::name::Name;
use analyzer_system::resolver::SourceObjectId;
use ast::Expr;

/// Owns references to the global AST and its environments.
#[derive(Debug, Default)]
pub struct Engine<'a> {
    /// The engine has the ownership of the AST.
    #[allow(clippy::vec_box)] // Box is used to ensure that the reference behind is still valid after vector's realloc
    asts: Vec<Box<Expr<'a>>>,

    /// Associates a module id to the corresponding environment.
    ///
    /// Those are origins of symbols that are available locally in the environment,
    /// which may also be the source of unresolved symbols, tracked in the resolver.
    pub origins: Vec<(&'a Expr<'a>, Option<Environment>)>,
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

    /// Adds a new origin to the engine and returns its given id.
    ///
    /// A call to this method must be followed by a call to [`Engine::attach`] with the same id
    /// after the environment has been built.
    pub fn track(&mut self, ast: &'a Expr<'a>) -> SourceObjectId {
        let id = self.origins.len();
        self.origins.push((ast, None));
        SourceObjectId(id)
    }

    /// Attaches an environment to an origin if the origin does not already have an attached environment.
    pub fn attach(&mut self, id: SourceObjectId, env: Environment) {
        debug_assert!(self.origins[id.0].1.is_none(), "Could not attach environment to a source that is already attached");
        self.origins[id.0].1.replace(env);
    }

    ///Finds an environment by its fully qualified name.
    pub fn find_environment_by_name(&self, name: &Name) -> Option<SourceObjectId> {
        self.origins
            .iter()
            .position(|(_, env)| {
                if let Some(env) = env {
                    &env.fqn == name
                } else {
                    false
                }
            })
            .map(SourceObjectId)
    }

    ///Finds an environment by its identifier.
    pub fn find_environment(&self, id: SourceObjectId) -> Option<&Environment> {
        self.origins.get(id.0).and_then(|(_, env)| env.as_ref())
    }
}
