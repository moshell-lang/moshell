use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;
use crate::class::ClassType;
use crate::types::{DefinedType, Type};
use crate::lang_types::*;

/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeContext<'a> {
    /// Records the type of each class by name.
    classes: HashMap<DefinedType, Rc<ClassType>>,

    dependencies: Vec<&'a TypeContext<'a>>,
}


//as current structures does not handle random accesses, we cannot share type contexts between threads
//then, we create
thread_local! {
    pub static LANG: TypeContext<'static> = TypeContext::lang();
}

impl<'a> TypeContext<'a> {
    ///Definitions of the lang type context.
    pub fn lang() -> Self {
        let mut ctx = TypeContext::default();

        const MSG: &str = "lang type registration";

        ctx.classes.insert(any(), Rc::new(ClassType::new(None, any(), Vec::new())));
        ctx.define_class(&any(), float()).expect(MSG);
        ctx.define_class(&any(), bool()).expect(MSG);
        ctx.define_class(&any(), str()).expect(MSG);
        ctx.define_class(&any(), unit()).expect(MSG);

        ctx.define_class(&float(), int()).expect(MSG);
        ctx.define_class(&int(), exitcode()).expect(MSG);

        ctx
    }

    /// Creates and registers a new ClassType for given type, the given type must be subtype of given type
    pub fn define_class(&mut self, super_type: &DefinedType, registered: DefinedType) -> Result<(), String> {
        if self.classes.contains_key(&registered) {
            return Err(format!("type already contained in context {}", registered).to_owned())
        }

        let sup = self.lookup_definition(super_type)?;


        self.classes.insert(
            registered.clone(),
            Rc::new(ClassType::new(Some(sup), registered)),
        );
        Ok(())
    }

    ///perform a class type lookup based on the defined type.
    /// If the type is not directly found in this context, then the context
    /// will lookup in parent's context.
    pub fn lookup_definition(&self, tpe: &DefinedType) -> Result<Rc<ClassType>, String> {
        match self.classes.get(&tpe) {
            Some(v) => Ok(v.clone()),
            None => {
                let iter = self.dependencies.iter();
                for dep in iter {
                    if let Some(found) = dep.lookup_definition(tpe).ok() {
                        return Ok(found)
                    }
                }
                Err("Unknown type".to_owned())
            }
        }
    }

    pub fn unify(&self, t1: &Type, t2: &Type) -> Result<Type, String> {
        self.unify_internal(t1, t2)
    }

    pub(crate) fn fork(&self) -> TypeContext {
        TypeContext {
            dependencies: vec!(self),
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

            (Type::Defined(def1 @ DefinedType::Parameterized(_)),
                Type::Defined(def2 @ DefinedType::Parameterized(_))) => {
                let cl1 = self.lookup_definition(def1)?;

                cl1.unify_with(self, def2)
                    .and_then(|opt|
                        opt.map(Type::Defined)
                            .ok_or("Type 1 and Type 2 are not inferable".to_owned())
                    )
            }

            (Type::Defined(DefinedType::Callable(_)), _) => {
                Err("Cannot handle callables yet".to_owned())
            }
            (_, Type::Defined(DefinedType::Callable(_))) => {
                Err("Cannot handle callables yet".to_owned())
            }
            (_, _) => Err(format!("Incompatible types {:?} and {:?}", t1, t2))
        }
    }

}
