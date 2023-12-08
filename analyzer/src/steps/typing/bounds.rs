use crate::reef::ReefId;
use crate::steps::typing::exploration::Exploration;
use crate::steps::typing::function::infer_return_from_hint;
use crate::types;
use crate::types::engine::FunctionId;
use crate::types::ty::{Type, TypeRef};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter::once;

/// Binds a polytype to largest possible monotype
#[derive(Default, Clone)]
pub struct TypesBounds {
    bounds: HashMap<TypeRef, TypeRef>,
}

impl TypesBounds {
    /// Construct a type bounds with no polytype to bound, which makes it inactive
    pub fn inactive() -> Self {
        Self::new(HashMap::new())
    }

    pub fn new(base: HashMap<TypeRef, TypeRef>) -> Self {
        Self { bounds: base }
    }

    pub fn get_bound(&self, ty: TypeRef) -> TypeRef {
        *self.bounds.get(&ty).unwrap_or(&ty)
    }
    /// return true if given type is registered as a bound type but is bound to itself
    pub fn is_self_bound(&self, ty: TypeRef) -> bool {
        self.bounds.get(&ty).is_some_and(|t| *t == ty)
    }

    /// update bounds of registered polytypes from given type scheme correlated with given bounds.
    /// This method will only update bounds that are larger than the current polytypes bounds
    pub(super) fn update_bounds(
        &mut self,
        base: TypeRef,
        new_bounds: TypeRef,
        exploration: &Exploration,
    ) {
        match self.bounds.entry(base) {
            Entry::Occupied(mut o) => {
                // As there is no real hierarchy for now, only the Nothing type can be more specific than any other type
                // if the base type already had a bound (other than himself), we can accept larger types only (thus no NOTHING currently)
                if *o.get() != base && new_bounds == types::NOTHING {
                    return;
                }
                o.insert(new_bounds);
            }
            Entry::Vacant(_) => {
                let base_type = exploration.get_type(base).unwrap();
                let bound_type = exploration.get_type(new_bounds).unwrap();
                if let (Type::Instantiated(b1, p1), Type::Instantiated(b2, p2)) =
                    (base_type, bound_type)
                {
                    for (base, bounds) in p1.iter().zip(p2) {
                        self.update_bounds(*base, *bounds, exploration);
                    }
                    self.update_bounds(*b1, *b2, exploration);
                }
            }
        }
    }
}

fn extract_polytypes(tpe_ref: TypeRef, exploration: &Exploration) -> Vec<TypeRef> {
    let tpe = exploration.get_type(tpe_ref).unwrap();
    match tpe {
        Type::Polytype => once(tpe_ref).collect(),
        Type::Instantiated(base, params) => extract_polytypes(*base, exploration)
            .into_iter()
            .chain(
                params
                    .iter()
                    .flat_map(|ty| extract_polytypes(*ty, exploration)),
            )
            .collect(),
        _ => Vec::new(),
    }
}

/// build type parameters bounds of a user-defined function.
/// The return hint is only applied if the function's return type does not depend on function's parameters.
/// Set `obj` to Some type if the function is a method that applies to the object
pub(super) fn build_bounds(
    user_bounds: &[TypeRef],
    obj: Option<TypeRef>,
    fun_reef: ReefId,
    function_id: FunctionId,
    return_hint: Option<TypeRef>,
    exploration: &Exploration,
) -> TypesBounds {
    let function = exploration.get_function(fun_reef, function_id).unwrap();

    let mut bounds = HashMap::new();

    // add in bounds the object's type instance parameters bounds
    if let Some(ty) = obj {
        let base = exploration.get_type(ty).unwrap();
        if let Type::Instantiated(base, tparams) = base {
            let Type::Structure(_, structure_id) = exploration.get_type(*base).unwrap() else {
                panic!("type instance is not of a structured type")
            };
            let base_tparams = exploration
                .get_structure(base.reef, *structure_id)
                .unwrap()
                .type_parameters
                .iter()
                .map(|ty| TypeRef::new(base.reef, *ty));
            bounds.extend(base_tparams.zip(tparams.clone()))
        }
    }

    // collect the functions' type parameters used in the parameters.
    let parameters_polytypes = function
        .parameters
        .iter()
        .flat_map(|p| extract_polytypes(p.ty, exploration))
        .collect();

    // Use the return type hint only if it does not contains a polytype bound with the parameters
    if !type_depends_of(function.return_type, &parameters_polytypes, exploration) {
        if let Some(hint) = return_hint {
            infer_return_from_hint(exploration, function.return_type, hint, &mut bounds);
        }
    }

    for (idx, type_param) in function.type_parameters.iter().enumerate() {
        let type_param = TypeRef::new(fun_reef, *type_param);
        let user_bound = user_bounds.get(idx).cloned();

        // user has explicitly set a type bound
        if let Some(user_bound) = user_bound {
            bounds.insert(type_param, user_bound);
        } else {
            // user expects an inference
            // if bounds is already know thanks to the given return type hint correlation with function types parameters
            // let it as is, else, bound the type param with itself
            bounds.entry(type_param).or_insert(type_param);
        }
    }

    TypesBounds::new(bounds)
}

/// search if given type is contained in given polytypes or has any type parameter contained in this list.
fn type_depends_of(tpe: TypeRef, polytypes: &Vec<TypeRef>, exploration: &Exploration) -> bool {
    if polytypes.contains(&tpe) {
        return true;
    }

    if let Type::Instantiated(base, params) = exploration.get_type(tpe).unwrap() {
        return type_depends_of(*base, polytypes, exploration)
            || params
                .iter()
                .any(|ty| type_depends_of(*ty, polytypes, exploration));
    }
    false
}

pub(super) fn apply_bounds(
    exploration: &mut Exploration,
    ty: TypeRef,
    bounds: &TypesBounds,
) -> TypeRef {
    let ty_ref = bounds.get_bound(ty);
    let ty = exploration.get_type(ty_ref).unwrap();
    if let Type::Instantiated(base, params) = ty {
        let base = bounds.get_bound(*base);
        let params: Vec<_> = params
            .clone()
            .into_iter()
            .map(|ty| apply_bounds(exploration, ty, bounds))
            .collect();

        let type_id = exploration
            .typing
            .add_type(Type::Instantiated(base, params), None);
        return TypeRef::new(exploration.externals.current, type_id);
    }

    ty_ref
}
