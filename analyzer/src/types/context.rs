use crate::types::class::{ClassTypeDefinition, TypeClass};
use crate::types::types::{DefinedType, Type};
use std::cell::RefCell;
use std::collections::hash_map::{DefaultHasher, Entry};
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Debug, Clone, Eq, Default)]
pub struct TypeContext {
    /// Records the type of each class by their identity.
    pub(crate) classes: HashMap<u64, Rc<TypeClass>>,

    pub(crate) dependencies: Vec<Rc<RefCell<TypeContext>>>,
}

impl PartialEq for TypeContext {
    fn eq(&self, other: &Self) -> bool {
        self.classes == other.classes
    }
}

//temporary function until an identification system is implemented
pub(crate) fn hash_of<H: Hash>(hashable: &H) -> u64 {
    let mut hasher = DefaultHasher::new();
    hashable.hash(&mut hasher);
    hasher.finish()
}

impl TypeContext {
    ///Definitions of the lang type context.
    pub fn lang() -> Rc<RefCell<Self>> {
        let ctx_rc = Rc::new(RefCell::new(TypeContext::default()));
        let mut ctx = ctx_rc.borrow_mut();

        const MSG: &str = "lang type registration";

        let any_cl = &Rc::new(TypeClass {
            super_type: None,
            name: "Any".to_owned(),
            generic_parameters: vec![],
            super_params_associations: vec![],
            identity: hash_of(&"Any"),
            context: Rc::new(RefCell::new(Self::fork(ctx_rc.clone()))),
        });
        ctx.classes.insert(any_cl.identity, any_cl.clone());
        drop(ctx);

        let float =
            Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new("Float")).expect(MSG);

        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new("Bool")).expect(MSG);
        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new("Str")).expect(MSG);
        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new("Unit")).expect(MSG);

        let int = Self::define_class(
            ctx_rc.clone(),
            ClassTypeDefinition::new("Int").with_super(float),
        )
        .expect(MSG);

        Self::define_class(
            ctx_rc.clone(),
            ClassTypeDefinition::new("Exitcode").with_super(int),
        )
        .expect(MSG);

        ctx_rc
    }

    /// Creates and registers a new ClassType for given types, the given type must be subtype of given types
    pub fn define_class(
        ctx: Rc<RefCell<Self>>,
        def: ClassTypeDefinition,
    ) -> Result<Rc<TypeClass>, String> {
        let id = hash_of(&def.name);

        let defined = def.with_identity(id).build(ctx.clone())?;
        let defined = Rc::new(defined);

        let mut ctx = ctx.borrow_mut();
        match ctx.classes.entry(defined.identity) {
            Entry::Occupied(_) => Err(format!(
                "type already contained in context {}",
                defined.name
            )),
            Entry::Vacant(vacant) => {
                vacant.insert(defined.clone());
                Ok(defined)
            }
        }
    }

    ///perform a class type lookup based on the defined types.
    /// If the type is not directly found in this context, then the context
    /// will lookup in parent's context.
    pub fn lookup_id(&self, id: u64) -> Result<Rc<TypeClass>, String> {
        match self.classes.get(&id) {
            Some(v) => Ok(v.clone()),
            None => {
                let iter = self.dependencies.iter();
                for dep in iter {
                    if let Ok(found) = dep.borrow().lookup_id(id) {
                        return Ok(found);
                    }
                }
                Err("Unknown types".to_owned())
            }
        }
    }

    pub fn lookup_defined(&self, def: &DefinedType) -> Result<Rc<TypeClass>, String> {
        match def {
            DefinedType::Parameterized(p) => self.lookup_id(hash_of(&p.name)),
        }
    }

    pub(crate) fn fork(ctx: Rc<RefCell<Self>>) -> TypeContext {
        TypeContext {
            dependencies: vec![ctx],
            ..Default::default()
        }
    }

    /// Find nearest type between two class types.
    pub fn unify(&self, t1: &Type, t2: &Type) -> Result<Type, String> {
        match (t1, t2) {
            (any, Type::Nothing) => Ok(any.clone()),
            (Type::Nothing, any) => Ok(any.clone()),

            (Type::Unknown, _) => Ok(Type::Unknown),
            (_, Type::Unknown) => Ok(Type::Unknown),

            (
                Type::Defined(DefinedType::Parameterized(p1)),
                Type::Defined(DefinedType::Parameterized(p2)),
            ) => {
                let cl1 = self.lookup_defined(&DefinedType::Parameterized(p1.clone()))?;
                let cl2 = self.lookup_defined(&DefinedType::Parameterized(p2.clone()))?;

                let common = cl1.get_common_parent(cl2);

                let vec: Vec<_> = common
                    .generic_parameters
                    .clone()
                    .into_iter()
                    .map(|_| Type::Unknown)
                    .collect();

                Ok(Type::parametrized(&common.name, vec.as_slice()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang_types::any;
    use crate::types::class::ClassTypeDefinition;
    use crate::types::context::TypeContext;
    use crate::types::types::Type;
    use pretty_assertions::assert_eq;
    use std::cell::RefCell;
    use std::rc::Rc;
    #[test]
    fn simple_union() -> Result<(), String> {
        let lang = TypeContext::lang();
        let ctx = Rc::new(RefCell::new(TypeContext::fork(lang.clone())));

        //Iterable[A]
        let iterable_cl = TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new("Iterable").with_generic("A", any()),
        )?;

        //Map[K, V]: Iterable[K]
        TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new("Map")
                .with_super(iterable_cl.clone())
                .with_generic("K", any())
                .with_generic("V", any())
                .with_association(0, Type::cons("K")),
        )?;

        //List[A]: Iterable[A]
        TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new("List")
                .with_super(iterable_cl.clone())
                .with_generic("A", any())
                .with_association(0, Type::cons("A")),
        )?;

        let res1 = ctx.borrow().unify(
            &Type::parametrized("List", &[Type::cons("Str")]),
            &Type::parametrized("Map", &[Type::cons("Str"), Type::cons("Int")]),
        )?;

        let res2 = ctx.borrow().unify(
            &Type::parametrized("Map", &[Type::cons("Str"), Type::cons("Int")]),
            &Type::parametrized("List", &[Type::cons("Str")]),
        )?;

        assert_eq!(res1, Type::parametrized("Iterable", &[Type::Unknown]));
        assert_eq!(res2, Type::parametrized("Iterable", &[Type::Unknown]));

        Ok(())
    }
}
