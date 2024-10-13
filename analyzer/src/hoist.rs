use crate::module::{Export, ModuleTree, ModuleView};
use crate::symbol::{SymbolRegistry, SymbolTable, UndefinedSymbol};
use crate::typing::function::{Function, FunctionKind};
use crate::typing::schema::Schema;
use crate::typing::user::{
    lookup_builtin_type, TypeId, UserType, ERROR_TYPE, STRING_TYPE, UNIT_TYPE,
};
use crate::typing::{Parameter, TypeChecker, TypeErrorKind};
use crate::{Reef, SourceLocation, TypeError, UnitKey};
use ast::function::{FunctionDeclaration, FunctionParameter};
use ast::r#struct::{StructDeclaration, StructImpl};
use ast::r#type::Type;
use ast::r#use::{Import, ImportList, ImportedSymbol, InclusionPathItem};
use ast::variable::TypedVariable;
use ast::Expr;
use context::source::{SourceSegmentHolder, Span};
use std::collections::{HashMap, HashSet};
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};

/// Places functions and types at the top level of the symbol table.
///
/// Those symbols may be used before they are declared, so their name needs to be forward declared,
/// and partially known ahead of time. For instance, the following code should be valid, even though
/// `Bar` is used before it is declared:
/// ```text
/// struct Foo {
///   bar: Bar,
/// }
/// struct Bar {}
/// ```
///
/// This step performs multiple passes where types are added to the symbol table, and then their
/// fields are added to the type. A third pass is done to hoist functions, now that all parameters
/// and return types may be known.
///
/// # Exploration order
///
/// The hoisting phase creates an identity for each module and outlines the dependencies between
/// them. It does then compute a satisfying order to analyse each module, one at a time, that should
/// not have cyclic dependencies for variables.
pub(super) fn hoist_files(
    foreign: &HashMap<OsString, ModuleTree>,
    reef: &mut Reef,
    checker: &mut TypeChecker,
    mut keys: Vec<UnitKey>,
) -> HoistingResult {
    let mut errors = Vec::<TypeError>::new();
    let mut graph = HashMap::new();
    for key @ UnitKey { path, .. } in &keys {
        let root = reef.files.get(key).expect("unit should exist");
        let mut table = reef
            .symbols
            .remove(path)
            .unwrap_or_else(|| SymbolTable::new(path.clone()));
        let mut exports = reef.exports.take_exports(path); // This is not problematic if the export is not found, but it shouldn't happen
        let modules = ModuleView::new(&reef.exports, foreign);
        let mut deps = Dependencies {
            modules,
            requires: Vec::new(),
        };
        hoist_type_names(root, checker, &mut table, &mut exports);
        hoist_signatures(
            root,
            checker,
            &mut table,
            &mut exports,
            &mut deps,
            &mut errors,
        );
        graph.insert(path.clone(), deps.requires);
        reef.exports.insert(path, exports);
        reef.symbols.insert(path.clone(), table);
    }

    let mut sorted: Vec<UnitKey> = Vec::new();
    let mut frontier: Vec<PathBuf> = graph
        .keys()
        .filter(|module| graph.values().all(|deps| !deps.contains(module)))
        .cloned()
        .collect();
    let mut seen: HashSet<PathBuf> = frontier.iter().cloned().collect(); // Deduplicate when the same file is imported multiple times
    while let Some(module) = frontier.pop() {
        let key = keys.swap_remove(
            keys.iter()
                .position(|key| key.path == module)
                .expect("module should exist"),
        );
        sorted.push(key);
        if let Some(requires) = graph.remove(&module) {
            for require in requires {
                if !seen.contains(&require) && !graph.values().any(|deps| deps.contains(&require)) {
                    seen.insert(require.clone());
                    frontier.push(require);
                }
            }
        }
    }
    sorted.reverse();

    if !graph.is_empty() {
        let mut cycle = graph
            .drain()
            .map(|(path, _)| path)
            .collect::<Vec<PathBuf>>();
        cycle.sort();
        let source = cycle.pop().expect("at least one item");
        errors.push(TypeError::new(
            TypeErrorKind::CircularDependency { cycle },
            SourceLocation::new(source, Span::default()),
        ));
    }

    HoistingResult { errors, sorted }
}

pub(crate) struct HoistingResult {
    pub(crate) errors: Vec<TypeError>,
    pub(crate) sorted: Vec<UnitKey>,
}

struct Dependencies<'a> {
    modules: ModuleView<'a>,
    requires: Vec<PathBuf>, // TODO: use the entire path instead
}

fn hoist_type_names(
    root: &[Expr],
    checker: &mut TypeChecker,
    table: &mut SymbolTable,
    exports: &mut [Export],
) {
    for expr in root.iter() {
        if let Expr::StructDeclaration(StructDeclaration {
            name,
            segment: span,
            ..
        }) = expr
        {
            let schema = checker
                .registry
                .define_schema(Schema::new(name.value.to_string()));
            let ty = checker.types.alloc(UserType::Parametrized {
                schema,
                params: Vec::new(),
            });
            table.insert_local(name.to_string(), ty, span.clone(), SymbolRegistry::Type);
            if let Some(export) = exports
                .iter_mut()
                .find(|export| export.name == name.value && export.registry == SymbolRegistry::Type)
            {
                export.ty = ty;
            }
        }
    }
}
/// hoists functions, structures and implementations
fn hoist_signatures(
    root: &[Expr],
    checker: &mut TypeChecker,
    table: &mut SymbolTable,
    exports: &mut [Export],
    deps: &mut Dependencies,
    errors: &mut Vec<TypeError>,
) {
    for expr in root.iter() {
        match expr {
            Expr::FunctionDeclaration(fn_decl) => {
                hoist_fn_decl(fn_decl, None, checker, table, exports, errors);
            }
            Expr::StructDeclaration(struct_decl) => {
                hoist_struct_decl(struct_decl, checker, table, errors);
            }
            Expr::Impl(impl_decl) => {
                hoist_impl_decl(impl_decl, checker, table, exports, errors);
            }
            Expr::Use(import) => {
                match &import.import {
                    Import::Symbol(ImportedSymbol {
                        path,
                        alias,
                        segment: span,
                    }) => {
                        if matches!(path.first(), Some(InclusionPathItem::Symbol(_))) {
                            continue; // Exclude inter-reefs dependencies
                        }
                        let (last, rest) = path.split_last().expect("at least one item");
                        if let Some(module) = deps.modules.get_direct(rest) {
                            for export in &module.exports {
                                if export.name == last.name() {
                                    table.insert_remote(
                                        alias
                                            .as_ref()
                                            .map(|alias| alias.value.as_str())
                                            .unwrap_or(last.name())
                                            .to_owned(),
                                        span.clone(),
                                        export,
                                    );
                                    if export.registry == SymbolRegistry::Variable {
                                        // FIXME: only work in 'reef::path::path::variable' case
                                        let path = rest
                                            .iter()
                                            .skip_while(|item| {
                                                matches!(item, InclusionPathItem::Reef(_))
                                            })
                                            .map(|item| item.name())
                                            .collect::<PathBuf>();
                                        deps.requires.push(path);
                                    }
                                }
                            }
                        }
                    }
                    Import::AllIn(path, _) => {
                        if matches!(path.first(), Some(InclusionPathItem::Symbol(_))) {
                            continue; // Exclude inter-reefs dependencies
                        }
                        if let Some(module) = deps.modules.get_direct(path) {
                            for export in &module.exports {
                                table.insert_remote(export.name.clone(), import.segment(), export);
                            }
                        }
                    }
                    Import::Environment(_) => {}
                    Import::List(ImportList {
                        root,
                        imports,
                        segment: span,
                    }) => {
                        if matches!(root.first(), Some(InclusionPathItem::Symbol(_))) {
                            continue; // Exclude inter-reefs dependencies
                        }
                        let base = root
                            .iter()
                            .skip_while(|item| matches!(item, InclusionPathItem::Reef(_)))
                            .map(|item| item.name())
                            .collect::<PathBuf>();
                        if let Some(mut module) = deps.modules.get_direct(root) {
                            for import in imports {
                                match import {
                                    Import::Symbol(symbol) => {
                                        let mut path = base.clone();
                                        for part in symbol.path.iter() {
                                            if let InclusionPathItem::Symbol(ident) = part {
                                                if let Some(tree) =
                                                    module.get(OsStr::new(ident.value.as_str()))
                                                {
                                                    path.push(ident.value.as_str());
                                                    module = tree;
                                                } else {
                                                    deps.requires.push(path);
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                    //TODO implement recursive operation
                                    Import::AllIn(_, _) => {}
                                    Import::Environment(_) => {}
                                    Import::List(_) => {}
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

struct SelfType {
    self_ty: TypeId,
    self_generics: Vec<TypeId>,
}

fn hoist_fn_decl(
    FunctionDeclaration {
        name,
        type_parameters,
        parameters,
        body,
        return_type,
        ..
    }: &FunctionDeclaration,
    self_ty: Option<SelfType>,
    checker: &mut TypeChecker,
    table: &mut SymbolTable,
    exports: &mut [Export],
    errors: &mut Vec<TypeError>,
) {
    table.enter_scope();
    let (current_ty, mut generic_variables) = match self_ty {
        Some(SelfType {
            self_ty: current_ty,
            self_generics: current_generics,
        }) => (Some(current_ty), current_generics),
        None => (None, Vec::new()),
    };
    generic_variables.extend(type_parameters.iter().map(|param| {
        checker
            .types
            .alloc(UserType::GenericVariable(param.name.to_string()))
    }));
    for (name, ty) in type_parameters.iter().zip(generic_variables.iter()) {
        table.insert_local(
            name.name.to_string(),
            *ty,
            name.segment(),
            SymbolRegistry::Type,
        );
    }
    let param_types = parameters
        .iter()
        .map(|param| {
            let ty = match check_parameter_type(current_ty, param, table, checker) {
                Ok(ty) => ty,
                Err(err) => {
                    errors.push(err.into_general(&table.path));
                    ERROR_TYPE
                }
            };
            Parameter {
                ty,
                span: param.segment(),
            }
        })
        .collect::<Vec<Parameter>>();
    let return_type = match return_type
        .as_ref()
        .map(|ty| check_type(ty, table, checker))
    {
        Some(Ok(ty)) => ty,
        Some(Err(err)) => {
            errors.push(err.into_general(&table.path));
            ERROR_TYPE
        }
        None => UNIT_TYPE,
    };
    table.exit_scope();
    let fqn = if let Some(current_ty) = current_ty {
        let UserType::Parametrized { schema, .. } = checker.types[current_ty] else {
            panic!(
                "the current type should be a struct, got {:?}",
                checker.types[current_ty]
            );
        };
        let Schema {
            name: ref type_name,
            ..
        } = checker.registry[schema];
        let mut fqn = PathBuf::from(type_name);
        fqn.push(name.value.to_string());
        fqn
    } else {
        let mut fqn = table.path.clone();
        fqn.push(name.value.to_string());
        fqn
    };
    let function = checker.registry.define_function(Function {
        declared_at: table.path.clone(),
        fqn,
        generic_variables,
        param_types,
        return_type,
        kind: if body.is_some() {
            FunctionKind::Function
        } else {
            FunctionKind::Intrinsic
        },
    });
    let function_type = checker.types.alloc(UserType::Function(function));
    match current_ty {
        Some(current_ty) => {
            let UserType::Parametrized { schema, .. } = checker.types[current_ty] else {
                panic!(
                    "the current type should be a struct, got {:?}",
                    checker.types[current_ty]
                );
            };
            let Schema {
                ref mut methods, ..
            } = checker.registry[schema];
            methods.insert(name.to_string(), function);
        }
        None => table.insert_local(
            name.to_string(),
            function_type,
            name.segment(),
            SymbolRegistry::Function,
        ),
    };
    if let Some(Export { ty, .. }) = exports
        .iter_mut()
        .find(|export| export.name == *name.value && export.registry == SymbolRegistry::Function)
    {
        *ty = function_type;
    }
}

fn hoist_struct_decl(
    StructDeclaration {
        name,
        parameters,
        fields,
        ..
    }: &StructDeclaration,
    checker: &mut TypeChecker,
    table: &mut SymbolTable,
    errors: &mut Vec<TypeError>,
) {
    let ty = table
        .get(name.value.as_str(), SymbolRegistry::Type)
        .expect("the type should be in the table")
        .ty;
    let UserType::Parametrized { schema, .. } = checker.types[ty] else {
        panic!("the type should have a schema");
    };
    let generics = parameters
        .iter()
        .map(|param| {
            checker
                .types
                .alloc(UserType::GenericVariable(param.name.to_string()))
        })
        .collect::<Vec<TypeId>>();
    table.enter_scope();
    for (name, ty) in parameters.iter().zip(generics.iter()) {
        table.insert_local(
            name.name.to_string(),
            *ty,
            name.segment(),
            SymbolRegistry::Type,
        );
    }
    let fields_types = fields
        .iter()
        .map(|field| {
            let ty = match check_type(&field.tpe, table, checker) {
                Ok(ty) => ty,
                Err(err) => {
                    errors.push(err.into_general(&table.path));
                    ERROR_TYPE
                }
            };
            (
                field.name.to_string(),
                Parameter {
                    ty,
                    span: field.tpe.segment(),
                },
            )
        })
        .collect::<HashMap<String, Parameter>>();
    let Schema {
        ref mut generic_variables,
        ref mut fields,
        ..
    } = checker.registry[schema];
    generic_variables.extend(generics);
    fields.extend(fields_types);
    table.exit_scope();
}

fn hoist_impl_decl(
    StructImpl {
        type_parameters,
        impl_type,
        functions,
        ..
    }: &StructImpl,
    checker: &mut TypeChecker,
    table: &mut SymbolTable,
    exports: &mut [Export],
    errors: &mut Vec<TypeError>,
) {
    table.enter_scope();
    let generic_variables = type_parameters
        .iter()
        .map(|param| {
            checker
                .types
                .alloc(UserType::GenericVariable(param.name.to_string()))
        })
        .collect::<Vec<TypeId>>();
    for (name, ty) in type_parameters.iter().zip(generic_variables.iter()) {
        table.insert_local(
            name.name.to_string(),
            *ty,
            name.segment(),
            SymbolRegistry::Type,
        );
    }
    let impl_ty = match check_type(impl_type, table, checker) {
        Ok(ty) => {
            if let UserType::Parametrized { .. } = checker.types[ty] {
                ty
            } else {
                errors.push(TypeError::new(
                    TypeErrorKind::CannotImplPrimitive,
                    SourceLocation::new(table.path.clone(), impl_type.segment()),
                ));
                ERROR_TYPE
            }
        }
        Err(err) => {
            errors.push(err.into_general(&table.path));
            ERROR_TYPE
        }
    };
    if impl_ty.is_ok() {
        for function in functions {
            let current = SelfType {
                self_ty: impl_ty,
                self_generics: generic_variables.clone(),
            };
            hoist_fn_decl(function, Some(current), checker, table, exports, errors);
        }
    }
    table.exit_scope();
}

fn check_parameter_type<'a>(
    current_ty: Option<TypeId>,
    param: &'a FunctionParameter,
    table: &'a SymbolTable,
    checker: &mut TypeChecker,
) -> Result<TypeId, InvalidType<'a>> {
    match param {
        FunctionParameter::Named(ty) => check_type_sig(ty, table, checker),
        FunctionParameter::Variadic(Some(ty), _) => check_type(ty, table, checker),
        FunctionParameter::Variadic(None, _) => Ok(STRING_TYPE),
        FunctionParameter::Slf(_) => {
            current_ty.ok_or(InvalidType::SelfOutsideImpl(param.segment()))
        }
    }
}

fn check_type_sig<'a>(
    sig: &'a TypedVariable,
    table: &SymbolTable,
    checker: &mut TypeChecker,
) -> Result<TypeId, InvalidType<'a>> {
    if let Some(ty) = &sig.ty {
        check_type(ty, table, checker)
    } else {
        Err(InvalidType::MissingType(sig.segment()))
    }
}

fn check_type<'a>(
    ty: &'a Type,
    table: &SymbolTable,
    checker: &mut TypeChecker,
) -> Result<TypeId, InvalidType<'a>> {
    let Type::Parametrized(ty) = ty else {
        return Err(InvalidType::MissingType(ty.segment()));
    };
    let [path] = ty.path.as_slice() else {
        return Err(InvalidType::MissingType(ty.segment.clone()));
    };
    let params = ty
        .params
        .iter()
        .map(|param| check_type(param, table, checker))
        .collect::<Result<Vec<TypeId>, _>>()?;
    let name = path.name();
    let ty = match lookup_builtin_type(name) {
        Some(ty) => ty,
        None => match table.lookup(name, SymbolRegistry::Type) {
            Ok(symbol) => symbol.ty,
            Err(inner) => {
                return Err(InvalidType::UnknownType {
                    name,
                    span: ty.segment.clone(),
                    inner,
                });
            }
        },
    };
    match checker.types[ty] {
        UserType::Parametrized { schema, .. } => Ok(if params.is_empty() {
            ty
        } else {
            checker
                .types
                .alloc(UserType::Parametrized { schema, params })
        }),
        _ => Ok(ty),
    }
}

pub(super) enum InvalidType<'a> {
    SelfOutsideImpl(Span),
    MissingType(Span),
    UnknownType {
        name: &'a str,
        span: Span,
        inner: UndefinedSymbol,
    },
}

impl InvalidType<'_> {
    pub(super) fn into_general(self, path: &Path) -> TypeError {
        match self {
            Self::SelfOutsideImpl(span) => TypeError::new(
                TypeErrorKind::UnexpectedSelfParameter,
                SourceLocation::new(path.to_owned(), span),
            ),
            Self::MissingType(span) => TypeError::new(
                TypeErrorKind::MissingType,
                SourceLocation::new(path.to_owned(), span),
            ),
            Self::UnknownType { name, span, inner } => TypeError::new(
                TypeErrorKind::UndefinedSymbol {
                    name: name.to_owned(),
                    expected: SymbolRegistry::Type,
                    found: inner.into(),
                },
                SourceLocation::new(path.to_owned(), span),
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::module::import_multi;
    use crate::MemoryFilesystem;
    use std::ffi::OsString;
    use std::path::PathBuf;

    fn hoist_files(fs: MemoryFilesystem, entrypoint: &str) -> Vec<TypeError> {
        let mut reef = Reef::new(OsString::from("test"));
        let import_result = import_multi(&mut reef, &fs, entrypoint);
        assert_eq!(import_result.errors, [], "no import errors should be found");
        super::hoist_files(
            &mut HashMap::new(),
            &mut reef,
            &mut TypeChecker::default(),
            import_result.keys,
        )
        .errors
    }

    fn hoist(source: &str) -> Vec<TypeError> {
        let fs = MemoryFilesystem::new(HashMap::from([(PathBuf::from("main.msh"), source)]));
        hoist_files(fs, "main.msh")
    }

    fn hoist_multi<const N: usize>(sources: [(PathBuf, &str); N]) -> Vec<TypeError> {
        let entrypoint = sources
            .first()
            .expect("at least one source")
            .0
            .display()
            .to_string();
        hoist_files(
            MemoryFilesystem::from_iter(sources.into_iter()),
            &entrypoint,
        )
    }

    #[test]
    fn function_use_builtin_type() {
        let errors = hoist("fun a(x: Int);");
        assert_eq!(errors, []);
    }

    #[test]
    fn function_use_unknown_type() {
        let errors = hoist("fun a(x: Foo);");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::UndefinedSymbol {
                    name: "Foo".to_string(),
                    expected: SymbolRegistry::Type,
                    found: None,
                },
                SourceLocation::new(PathBuf::from("main.msh"), 9..12),
            )]
        );
    }

    #[test]
    fn function_use_type_below() {
        let errors = hoist("fun a(x: Foo); struct Foo {}");
        assert_eq!(errors, []);
    }

    #[test]
    fn self_outside_impl() {
        let errors = hoist("fun a(self);");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::UnexpectedSelfParameter,
                SourceLocation::new(PathBuf::from("main.msh"), 6..10),
            )]
        );
    }

    #[test]
    fn generic_field_struct() {
        let errors = hoist("struct Box[T] { value: T }");
        assert_eq!(errors, []);
    }

    #[test]
    fn dont_share_t() {
        let errors = hoist("struct Foo[T] {}\nfun test() -> T;");
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::UndefinedSymbol {
                    name: "T".to_owned(),
                    expected: SymbolRegistry::Type,
                    found: None,
                },
                SourceLocation::new(PathBuf::from("main.msh"), 31..32)
            )]
        );
    }

    #[test]
    fn import_type() {
        let errors = hoist_multi([
            (PathBuf::from("main"), "use reef::foo::Foo; fun a(x: Foo);"),
            (PathBuf::from("foo"), "struct Foo {}"),
        ]);
        assert_eq!(errors, []);
    }

    #[test]
    fn report_variable_cycle() {
        let errors = hoist_multi([
            (PathBuf::from("main"), "use reef::test::bar\nval expose"),
            (PathBuf::from("test"), "use reef::main::expose\nval bar"),
        ]);
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::CircularDependency {
                    cycle: vec![PathBuf::from("main")],
                },
                SourceLocation::new(PathBuf::from("test"), 0..0)
            )]
        );
    }

    #[test]
    fn no_variable_init_order() {
        let errors = hoist_multi([
            (
                PathBuf::from("foo"),
                "use reef::math::bar as baz
val foo = 9
val bar = $baz",
            ),
            (
                PathBuf::from("math"),
                "use reef::foo::foo
val foo = $foo
val bar = 4",
            ),
        ]);
        assert_eq!(
            errors,
            [TypeError::new(
                TypeErrorKind::CircularDependency {
                    cycle: vec![PathBuf::from("foo")],
                },
                SourceLocation::new(PathBuf::from("math"), 0..0)
            )]
        );
    }
}
