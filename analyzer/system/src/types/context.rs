use crate::types::class::{ClassTypeDefinition, TypeClass};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

use crate::import_engine::ContextExports;
use crate::name::Name;
use crate::types::Type;

/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Clone)]
pub struct TypeContext {
    /// Records the type of each class by their basename.
    classes: HashMap<Name, Rc<TypeClass>>,

    ///Environment's fully qualified name
    pub fqn: Name,
}

impl PartialEq for TypeContext {
    fn eq(&self, other: &Self) -> bool {
        self.fqn == other.fqn && self.classes == other.classes
    }
}

impl ContextExports<Rc<TypeClass>> for TypeContext {
    fn find_exported(&self, name: &Name) -> Option<Rc<TypeClass>> {
        self.classes.get(name).filter(|tc| tc.is_exported).cloned()
    }

    fn list_exported_names(&self, symbol: Option<Name>) -> Vec<Name> {
        let parts = symbol.map(Name::into_vec).unwrap_or_default();
        self.classes
            .iter()
            .filter_map(|(k, v)| {
                if k.path() == parts.as_slice() {
                    Some(v.fqcn.relative_to(&self.fqn).unwrap())
                } else {
                    None
                }
            })
            .collect()
    }
}

impl Debug for TypeContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut classes: Vec<_> = self.classes.iter().collect();
        classes.sort_by_key(|(k, _)| *k);

        f.debug_struct("TypeContext")
            .field("identity", &self.fqn)
            .field("classes", &classes)
            .finish()
    }
}

impl TypeContext {
    pub(crate) fn with_classes<const T: usize>(
        classes: [(Name, TypeClass); T],
        identity: Name,
    ) -> Self {
        let classes = classes.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();
        Self {
            classes,
            fqn: identity,
        }
    }

    pub(crate) fn new(identity: Name) -> Self {
        Self {
            fqn: identity,
            classes: HashMap::new(),
        }
    }

    ///Definitions of the lang type context.
    pub fn lang() -> Self {
        let mut ctx = Self {
            fqn: Name::new("lang"),
            classes: Default::default(),
        };

        const MSG: &str = "lang type registration";

        let any_cl = &Rc::new(TypeClass {
            super_type: None,
            is_exported: true,
            generic_parameters: vec![],
            super_params_associations: vec![],
            fqcn: ctx.fqn.child("Any"),
        });
        ctx.classes
            .insert(Name::new(any_cl.fqcn.simple_name()), any_cl.clone());

        let float = ctx
            .define_class(ClassTypeDefinition::new(Name::new("Float")))
            .expect(MSG);

        ctx.define_class(ClassTypeDefinition::new(Name::new("Bool")))
            .expect(MSG);
        ctx.define_class(ClassTypeDefinition::new(Name::new("Str")))
            .expect(MSG);
        ctx.define_class(ClassTypeDefinition::new(Name::new("Unit")))
            .expect(MSG);

        let int = ctx
            .define_class(ClassTypeDefinition::new(Name::new("Int")).with_super(float))
            .expect(MSG);

        ctx.define_class(ClassTypeDefinition::new(Name::new("Exitcode")).with_super(int))
            .expect(MSG);

        ctx
    }

    /// Creates and registers a new ClassType for given types, the given type must be subtype of given types
    pub fn define_class(&mut self, def: ClassTypeDefinition) -> Result<Rc<TypeClass>, String> {
        let name = def.name.clone();
        let defined = def.build(self)?;
        let defined = Rc::new(defined);

        match self.classes.entry(name.clone()) {
            Entry::Occupied(_) => Err(format!("type {name} already contained in context")),
            Entry::Vacant(vacant) => {
                vacant.insert(defined.clone());
                Ok(defined)
            }
        }
    }

    ///perform a class type lookup based on the defined types.
    /// If the type is not directly found in this context, then the context
    /// will lookup in parent's context.
    pub fn use_class(&mut self, _name: &str) -> Result<Rc<TypeClass>, String> {
        todo!("use_class")
    }

    /// Find nearest type between two class types.
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<Type, String> {
        match (t1, t2) {
            (any, Type::Nothing) => Ok(any.clone()),
            (Type::Nothing, any) => Ok(any.clone()),

            (Type::Unknown, _) => Ok(Type::Unknown),
            (_, Type::Unknown) => Ok(Type::Unknown),

            (Type::Parametrized(p1), Type::Parametrized(p2)) => {
                let cl1 = self.use_class(&p1.name)?;
                let cl2 = self.use_class(&p2.name)?;

                let common = cl1.get_common_parent(cl2);

                let vec: Vec<_> = common
                    .generic_parameters
                    .clone()
                    .into_iter()
                    .map(|_| Type::Unknown)
                    .collect();

                let name = common.fqcn.to_string();
                Ok(Type::parametrized(&name, vec.as_slice()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang_types::any;
    use crate::name::Name;
    use crate::types::class::ClassTypeDefinition;
    use crate::types::context::TypeContext;
    use crate::types::Type;
    use pretty_assertions::assert_eq;

    #[test]
    #[ignore]
    fn simple_union() {
        let mut ctx = TypeContext::new(Name::new("std"));

        //Iterable[A]
        let iterable_cl = ctx
            .define_class(ClassTypeDefinition::new(Name::new("Iterable")).with_generic("A", any()))
            .expect("error");

        //Map[K, V]: Iterable[K]
        ctx.define_class(
            ClassTypeDefinition::new(Name::new("Map"))
                .with_super(iterable_cl.clone())
                .with_generic("K", any())
                .with_generic("V", any())
                .with_association(0, Type::cons("Map::K")),
        )
        .expect("error");

        //List[A]: Iterable[A]
        ctx.define_class(
            ClassTypeDefinition::new(Name::new("List"))
                .with_super(iterable_cl.clone())
                .with_generic("A", any())
                .with_association(0, Type::cons("List::A")),
        )
        .expect("error");

        let res1 = ctx
            .unify(
                &Type::parametrized("List", &[Type::cons("Str")]),
                &Type::parametrized("Map", &[Type::cons("Str"), Type::cons("Int")]),
            )
            .expect("error");

        let res2 = ctx
            .unify(
                &Type::parametrized("Map", &[Type::cons("Str"), Type::cons("Int")]),
                &Type::parametrized("List", &[Type::cons("Str")]),
            )
            .expect("error");

        assert_eq!(res1, Type::parametrized("std::Iterable", &[Type::Unknown]));
        assert_eq!(res2, Type::parametrized("std::Iterable", &[Type::Unknown]));
    }
}
