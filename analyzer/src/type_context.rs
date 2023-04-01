use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::f32::consts::E;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use crate::class::{ClassTypeDef, ClassTypeDefinition};
use crate::types::{DefinedType, ParameterizedType, Type};
use crate::lang_types::*;

/// A type environment.
///
/// Contexts track substitutions and generate fresh type variables.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeContext<'a> {
    /// Records the type of each class by their identity.
    classes: HashMap<u64, Rc<ClassTypeDef>>,

    dependencies: Vec<&'a TypeContext<'a>>,
}


//as current structures does not handle random accesses, we cannot share type contexts between threads
thread_local! {
    pub static LANG: TypeContext<'static> = TypeContext::lang();
}




fn hash_of<H: Hash>(hashable: &H) -> u64 {
    let mut hasher = DefaultHasher::new();
    hashable.hash(&mut hasher);
    hasher.finish()
}

impl<'a> TypeContext<'a> {
    ///Definitions of the lang type context.
    pub fn lang() -> Self {
        let mut ctx = TypeContext::default();

        const MSG: &str = "lang type registration";

        let any_cl = &Rc::new(ClassTypeDef {
            super_type: None,
            name: "Any".to_owned(),
            generic_parameters: vec![],
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

    /// Creates and registers a new ClassType for given type, the given type must be subtype of given type
    pub fn define_class(&mut self, def: ClassTypeDefinition) -> Result<Rc<ClassTypeDef>, String> {
        let defined = Rc::new(def.build(self)?);
        if self.classes.contains_key(&defined.identity) {
            return Err(format!("type already contained in context {}", defined.name).to_owned())
        }

        self.classes.insert(
            defined.identity,
            defined.clone(),
        );
        Ok(defined)
    }

    ///perform a class type lookup based on the defined type.
    /// If the type is not directly found in this context, then the context
    /// will lookup in parent's context.
    pub fn lookup_id(&self, id: u64) -> Result<Rc<ClassTypeDef>, String> {
        match self.classes.get(&id) {
            Some(v) => Ok(v.clone()),
            None => {
                let iter = self.dependencies.iter();
                for dep in iter {
                    if let Some(found) = dep.lookup_id(id).ok() {
                        return Ok(found)
                    }
                }
                Err("Unknown type".to_owned())
            }
        }
    }

    pub fn lookup_defined(&self, def: DefinedType) -> Result<Rc<ClassTypeDef>, String> {
        match def {
            DefinedType::Parameterized(p) => self.lookup_id(hash_of(&p.name)),
            DefinedType::Callable(_) => todo!("implement Callable identity")
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

            (Type::Defined(DefinedType::Parameterized(def1)),
                Type::Defined(def2 @ DefinedType::Parameterized(_))) => {
                let cl1 = self.lookup_id(hash_of(&def1.name))?;

                Ok(Type::Defined(cl1.unify_with(self, def2)))
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
