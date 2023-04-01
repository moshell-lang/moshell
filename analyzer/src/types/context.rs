use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use crate::types::class::{ClassTypeDefinition, TypeClass};
use crate::types::types::{DefinedType, ParameterizedType, Type};

/// A type environment.
///
/// Contexts track substitutions and generate fresh types variables.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeContext<'a> {
    /// Records the types of each class by their identity.
    classes: HashMap<u64, Rc<TypeClass>>,

    dependencies: Vec<&'a TypeContext<'a>>,
}

//as current structures does not handle random accesses, we cannot share types contexts between threads
thread_local! {
    pub static LANG: TypeContext<'static> = TypeContext::lang();
}

fn hash_of<H: Hash>(hashable: &H) -> u64 {
    let mut hasher = DefaultHasher::new();
    hashable.hash(&mut hasher);
    hasher.finish()
}

impl<'a> TypeContext<'a> {
    ///Definitions of the lang types context.
    pub fn lang() -> Self {
        let mut ctx = TypeContext::default();

        const MSG: &str = "lang types registration";

        let any_cl = &Rc::new(TypeClass {
            super_type: None,
            name: "Any".to_owned(),
            generic_parameters: vec![],
            super_params_associations: vec![],
            identity: hash_of(&"Any"),
        });
        ctx.classes.insert(any_cl.identity, any_cl.clone());

        let float = ctx.define_class(ctx.mk_definition("Float")).expect(MSG);

        ctx.define_class(ctx.mk_definition("Bool")).expect(MSG);
        ctx.define_class(ctx.mk_definition("Str")).expect(MSG);
        ctx.define_class(ctx.mk_definition("Unit")).expect(MSG);

        let int = ctx.define_class(
            ctx.mk_definition("Int")
                .with_parent(float)
        ).expect(MSG);

        ctx.define_class(
            ctx.mk_definition("Exitcode")
                .with_parent(int)
        ).expect(MSG);

        ctx
    }

    pub fn mk_definition(&self, name: &str) -> ClassTypeDefinition {
        ClassTypeDefinition::new(name.to_owned(), hash_of(&name))
    }

    /// Creates and registers a new ClassType for given types, the given types must be subtype of given types
    pub fn define_class(&mut self, def: ClassTypeDefinition) -> Result<Rc<TypeClass>, String> {
        let defined = Rc::new(def.build(self)?);
        if self.classes.contains_key(&defined.identity) {
            return Err(format!("types already contained in context {}", defined.name).to_owned())
        }

        self.classes.insert(
            defined.identity,
            defined.clone(),
        );
        Ok(defined)
    }

    ///perform a class types lookup based on the defined types.
    /// If the types is not directly found in this context, then the context
    /// will lookup in parent's context.
    pub fn lookup_id(&self, id: u64) -> Result<Rc<TypeClass>, String> {
        match self.classes.get(&id) {
            Some(v) => Ok(v.clone()),
            None => {
                let iter = self.dependencies.iter();
                for dep in iter {
                    if let Some(found) = dep.lookup_id(id).ok() {
                        return Ok(found)
                    }
                }
                Err("Unknown types".to_owned())
            }
        }
    }

    pub fn lookup_defined(&self, def: DefinedType) -> Result<Rc<TypeClass>, String> {
        match def {
            DefinedType::Parameterized(p) => self.lookup_id(hash_of(&p.name)),
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

    ///Find largest possible types between two class types
    fn unify_internal(&self, t1: &Type, t2: &Type) -> Result<Type, String> {
        match (t1, t2) {
            (any, Type::Nothing) => Ok(any.clone()),
            (Type::Nothing, any) => Ok(any.clone()),

            (Type::Unknown, _) => Ok(Type::Unknown),
            (_, Type::Unknown) => Ok(Type::Unknown),

            (Type::Defined(DefinedType::Parameterized(p1)),
                Type::Defined(DefinedType::Parameterized(p2))) => {
                self.unify_parameterized(p1, p2)
                    .map(DefinedType::Parameterized)
                    .map(Type::Defined)
            }

        }
    }

    fn unify_parameterized(&self, p1: &ParameterizedType, p2: &ParameterizedType) -> Result<ParameterizedType, String> {
        let cl1 = self.lookup_defined(DefinedType::Parameterized(p1.clone()))?;
        let cl2 = self.lookup_defined(DefinedType::Parameterized(p2.clone()))?;

        let common = cl1.get_common_parent(cl2);

        todo!()
    }

}
