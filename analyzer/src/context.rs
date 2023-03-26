use std::collections::HashMap;
use crate::classes::ClassType;
use crate::types::{DefinedType, Type};
use crate::builtin_types::*;

/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context<'a> {
    /// Records the type of each class by name.
    classes: HashMap<DefinedType, ClassType>,

    parent: Option<&'a Context<'a>>,
}

impl<'a> Context<'a> {
    pub fn lang() -> Self {
        let mut ctx = Context::default();

        ctx.classes.insert(float(), ClassType::cons(float()));
        ctx.classes.insert(bool(), ClassType::cons(bool()));
        ctx.classes.insert(str(), ClassType::cons(str()));
        ctx.classes.insert(unit(), ClassType::cons(unit()));

        ctx.register_specialized(float(), int()).expect("lang type registration");
        ctx.register_specialized(int(), exitcode()).expect("lang type registration");

        ctx
    }


    /// Creates and registers a new ClassType for given type, the given type must be subtype of given type
    pub fn register_specialized(&mut self, super_type: DefinedType, registered: DefinedType) -> Result<(), String> {
        if self.classes.contains_key(&registered) {
            return Err(format!("type already contained in context {}", registered.name).to_owned())
        }

        self.classes.insert(
            registered.clone(),
            ClassType {
                base: registered.clone(),
                constraint_type: Some(super_type),

                callable: None,
                params: registered.params,
            },
        );
        Ok(())
    }

    ///perform a class type lookup based on the defined type.
    /// If the type is not directly found in this context, then the context
    /// will lookup in parent's context.
    pub fn lookup_definition(&'a self, tpe: &DefinedType) -> Result<&'a ClassType, String> {
        match self.classes.get(&tpe) {
            Some(v) => Ok(v),
            None => {
                if let Some(parent) = self.parent {
                    return parent.lookup_definition(tpe)
                }
                Err("Unknown type".to_owned())
            }
        }
    }
    /*
        pub fn resolve(&self, declared_type: &TypeScheme) -> Result<Variable, String> {
            match declared_type {
                TypeScheme::Monotype(t) => self.resolve_monotype(t),
                TypeScheme::Polytype { .. } => todo!("resolve polytype"),
            }
        }
        */
    /*
        pub fn resolve_monotype(&self, declared_type: &Type) -> Result<Variable, String> {
            match declared_type {
                Type::Variable(v) => self
                    .substitution
                    .get(v)
                    .map(|t| self.resolve(t))
                    .unwrap_or(Ok(*v)),
                Type::Defined(name, args) => {
                    let var = self
                        .classes
                        .get(name)
                        .ok_or_else(|| format!("Unknown class {}", name))?;
                    let class = self
                        .definitions
                        .get(var)
                        .ok_or_else(|| format!("Unknown class {}", name))?;
                    assert_eq!(class.type_args.len(), args.len());
                    assert_eq!(class.type_args.len(), 0);
                    Ok(*var)
                }
            }
        }
    */
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<Type, String> {
        self.unify_internal(t1, t2)
    }

    pub(crate) fn fork(&self) -> Context {
        Context {
            parent: Some(self),
            ..Default::default()
        }
    }

    ///Find largest possible type between two class types
    fn unify_internal(&self, t1: &Type, t2: &Type) -> Result<Type, String> {
        match (t1, t2) {
            (any, Type::Nothing) => Ok(any.clone()),
            (Type::Nothing, any) => Ok(any.clone()),

            (Type::Unknown, _) => Ok(Type::Unknown),
            (_, Type::Unknown) => Ok(Type::Unknown),

            (Type::Defined(def1), Type::Defined(def2)) => {
                let cl1 = self.lookup_definition(def1)?;

                cl1.unify_base(self, def2)
                    .and_then(|opt|
                        opt.map(Type::Defined)
                            .ok_or("Type 1 and Type 2 are not inferable".to_owned())
                    )

            }
            (Type::Callable(_), _) => {
                Err("Cannot handle callables yet".to_owned())
            }
            (_, Type::Callable(_)) => {
                Err("Cannot handle callables yet".to_owned())
            }
            (_, _) => Err(format!("Incompatible types {:?} and {:?}", t1, t2))
        }
    }

}
