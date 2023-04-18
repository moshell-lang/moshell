use crate::types::class::{ClassTypeDefinition, TypeClass};
use crate::types::types::{Type};
use std::cell::RefCell;
use std::collections::hash_map::{Entry};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use crate::environment::{Environment, ContextExports};

use crate::name::Name;
use crate::import_engine::{ReadOnlyImportEngine};
use crate::visibility::ScopeVisibility;
use crate::visibility::ScopeVisibility::Public;


/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Clone)]
pub struct TypeContext {
    /// Records the type of each class by their basename.
    classes: HashMap<Name, Rc<TypeClass>>,

    ///View of the environment's engine.
    imports: ReadOnlyImportEngine,

    ///Environment's fully qualified name
    pub fqn: Name,
}

impl PartialEq for TypeContext {
    fn eq(&self, other: &Self) -> bool {
        self.fqn == other.fqn && self.classes == other.classes
    }
}

impl ContextExports<Rc<TypeClass>> for TypeContext {
    fn from_env(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        env.borrow().type_context.clone()
    }

    fn find_exported(&self, name: &Name) -> Option<Rc<TypeClass>> {
        self.classes.get(name).filter(|tc| tc.visibility == Public).cloned()
    }

    fn list_exported_names(&self, symbol: Option<Name>) -> Vec<Name> {
        self.classes
            .clone()
            .into_iter()
            .filter(|(k, _)| {
                let parts = symbol.clone().map(|s| s.parts()).unwrap_or_default().leak();
                k.parts().starts_with(parts)
            })
            .map(|(_, v)| v.fqcn.relative_to(&self.fqn).unwrap())
            .collect()
    }
}

impl Debug for TypeContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut classes: Vec<_> = self.classes.iter().collect();
        classes.sort_by_key(|(k, _)| k.clone());

        f.debug_struct("TypeContext").field("identity", &self.fqn)
            .field("imports", &self.imports)
            .field("classes", &classes)
            .finish()
    }
}


impl TypeContext {
    pub(crate) fn with_classes<const T: usize>(classes: [(Name, TypeClass); T],
                                               identity: Name,
                                               imports: ReadOnlyImportEngine) -> Self {
        let classes = classes
            .into_iter()
            .map(|(k, v)| (k, Rc::new(v)))
            .collect();
        Self {
            classes,
            imports,
            fqn: identity,
        }
    }

    pub(crate) fn new(identity: Name, imports: ReadOnlyImportEngine) -> Self {
        Self {
            fqn: identity,
            imports,
            classes: HashMap::new(),
        }
    }

    ///Definitions of the lang type context.
    pub fn lang(imports: ReadOnlyImportEngine) -> Rc<RefCell<Self>> {
        let ctx = Self {
            fqn: Name::new("lang"),
            imports,
            classes: Default::default(),
        };
        let ctx_rc = Rc::new(RefCell::new(ctx));
        let mut ctx = ctx_rc.borrow_mut();

        const MSG: &str = "lang type registration";

        let any_cl = &Rc::new(TypeClass {
            super_type: None,
            visibility: ScopeVisibility::Public,
            generic_parameters: vec![],
            super_params_associations: vec![],
            fqcn: ctx.fqn.child("Any"),
        });
        ctx.classes.insert(Name::new(&any_cl.fqcn.name), any_cl.clone());
        drop(ctx);

        let float =
            Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new(Name::new("Float"))).expect(MSG);

        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new(Name::new("Bool"))).expect(MSG);
        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new(Name::new("Str"))).expect(MSG);
        Self::define_class(ctx_rc.clone(), ClassTypeDefinition::new(Name::new("Unit"))).expect(MSG);

        let int = Self::define_class(
            ctx_rc.clone(),
            ClassTypeDefinition::new(Name::new("Int")).with_super(float),
        )
            .expect(MSG);

        Self::define_class(
            ctx_rc.clone(),
            ClassTypeDefinition::new(Name::new("Exitcode")).with_super(int),
        )
            .expect(MSG);

        ctx_rc
    }

    /// Creates and registers a new ClassType for given types, the given type must be subtype of given types
    pub fn define_class(
        ctx: Rc<RefCell<Self>>,
        def: ClassTypeDefinition,
    ) -> Result<Rc<TypeClass>, String> {

        let name = def.name.clone();
        let defined = def.build(ctx.clone())?;
        let defined = Rc::new(defined);

        let mut ctx = ctx.borrow_mut();

        match ctx.classes.entry(name.clone()) {
            Entry::Occupied(_) => Err(format!(
                "type {} already contained in context",
                name
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
    pub fn use_class(&mut self, name: &str) -> Result<Rc<TypeClass>, String> {
        let name = Name::new(name);
        match self.classes.get(&name) {
            Some(v) => Ok(v.clone()),
            None => self.imports
                .use_element::<Rc<TypeClass>, Self>(&name)
                .ok_or(format!("Unknown type {}", &name))
        }
    }

    /// Find nearest type between two class types.
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<Type, String> {
        match (t1, t2) {
            (any, Type::Nothing) => Ok(any.clone()),
            (Type::Nothing, any) => Ok(any.clone()),

            (Type::Unknown, _) => Ok(Type::Unknown),
            (_, Type::Unknown) => Ok(Type::Unknown),

            (
                Type::Parametrized(p1),
                Type::Parametrized(p2),
            ) => {
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
    use crate::types::class::{ClassTypeDefinition};
    use crate::types::context::TypeContext;
    use crate::types::types::Type;
    use pretty_assertions::assert_eq;
    use crate::name::Name;
    use crate::layers::ModuleLayers;

    #[test]
    fn simple_union() {
        let layers = ModuleLayers::new();

        let ctx = ModuleLayers::declare_env(layers.clone(), Name::new("std")).expect("error")
            .borrow()
            .type_context
            .clone();


        //Iterable[A]
        let iterable_cl = TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new(Name::new("Iterable"))
                .with_generic("A", any()),
        ).expect("error");

        //Map[K, V]: Iterable[K]
        TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new(Name::new("Map"))
                .with_super(iterable_cl.clone())
                .with_generic("K", any())
                .with_generic("V", any())
                .with_association(0, Type::cons("Map::K")),
        ).expect("error");

        //List[A]: Iterable[A]
        TypeContext::define_class(
            ctx.clone(),
            ClassTypeDefinition::new(Name::new("List"))
                .with_super(iterable_cl.clone())
                .with_generic("A", any())
                .with_association(0, Type::cons("List::A")),
        ).expect("error");

        let mut ctx = ctx.borrow_mut();

        let res1 = ctx.unify(
            &Type::parametrized("List", &[Type::cons("Str")]),
            &Type::parametrized("Map", &[Type::cons("Str"), Type::cons("Int")]),
        ).expect("error");

        let res2 = ctx.unify(
            &Type::parametrized("Map", &[Type::cons("Str"), Type::cons("Int")]),
            &Type::parametrized("List", &[Type::cons("Str")]),
        ).expect("error");

        assert_eq!(res1, Type::parametrized("std::Iterable", &[Type::Unknown]));
        assert_eq!(res2, Type::parametrized("std::Iterable", &[Type::Unknown]));

    }
}