use analyzer_system::environment::Environment;
use analyzer_system::name::Name;
use analyzer_system::resolver::SourceObjectId;
use ast::Expr;

/// Owns references to the global AST and its environments.
#[derive(Debug, Default)]
pub struct Engine<'a> {
    /// The engine has the ownership of the AST.
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
    pub fn attach(&mut self, id: SourceObjectId, env: Environment) -> Result<(), String> {
        if self.origins[id.0].1.is_some() {
            return Err(format!("Could not attach environment to {}: an environment is already attached", id.0))
        }
        self.origins[id.0].1 = Some(env);
        Ok(())
    }

    pub fn find_environment_by_name(&self, name: &Name) -> Option<SourceObjectId> {
        let mut index: usize = 0;
        for (_, env) in &self.origins {
            if let Some(env) = env {
                if &env.fqn == name {
                    return Some(SourceObjectId(index))
                }
            }
            index += 1;
        }
        return None
    }

    pub fn find_environment(&self, id: SourceObjectId) -> Option<&'a Environment> {
        self.origins.get(id.0).and_then(|(_, env)| {
            // SAFETY: As the Engine hosts the Environments and may not remove or replace an environment,
            //The reference behind Environment does not change and is valid for the lifetime of the engine.
            unsafe {
                std::mem::transmute::<Option<&Environment>, Option<&'a Environment>>(env.as_ref())
            }
        })
    }
}
