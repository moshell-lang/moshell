use crate::diagnostic::{Diagnostic, DiagnosticID, Observation, SourceLocation};
use crate::engine::Engine;
use crate::environment::variables::resolve_loc;
use crate::environment::Environment;
use crate::imports::{ResolvedImport, SourceImports, UnresolvedImport};
use crate::name::Name;
use crate::reef::ReefContext;
use crate::relations::{ResolvedSymbol, SourceId};
use crate::steps::resolve::{diagnose_unresolved_import, SymbolResolver};

impl<'a, 'ca, 'e> SymbolResolver<'a, 'ca, 'e> {
    /// Attempts to resolve all given unresolved imports, returning a [ResolvedImports] structure containing the
    /// imports that could get resolved.
    /// This method will append a new diagnostic for each imports that could not be resolved.
    pub fn resolve_imports(
        context: &ReefContext<'ca, 'e>,
        env_id: SourceId,
        imports: &mut SourceImports,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        //iterate over our unresolved imports
        for (unresolved, segment) in imports.take_unresolved_imports() {
            match unresolved {
                // If the unresolved import is a symbol
                UnresolvedImport::Symbol { alias, loc } => {
                    // resolve path and targeted reef

                    match resolve_loc(&loc, context) {
                        None => diagnostics.push(
                            Diagnostic::new(DiagnosticID::InvalidSymbolPath, "Invalid import")
                                .with_observation(Observation::context(
                                    env_id,
                                    segment,
                                    format!("Cannot find reef `{}`", loc.name.root()),
                                )),
                        ),
                        Some((reef, reef_id)) => {
                            let name = &loc.name;
                            // try to get referenced module of the reef
                            let result = get_mod(name, &reef.engine);
                            match result {
                                None => {
                                    // if the environment wasn't found, and its name was already known, push a diagnostic as it does not exists
                                    let diagnostic = diagnose_unresolved_import(
                                        env_id,
                                        name,
                                        None,
                                        segment.clone(),
                                    );
                                    diagnostics.push(diagnostic);
                                    imports.set_resolved_import(
                                        alias.unwrap_or(name.simple_name().to_string()),
                                        ResolvedImport::Dead,
                                        segment,
                                    );
                                }
                                //else, try to resolve it
                                Some((found_env_id, found_env)) => {
                                    let symbol_name = name.simple_name().to_string();
                                    if found_env.fqn == *name {
                                        //it's the environment that is being imported
                                        imports.set_resolved_import(
                                            alias.unwrap_or(symbol_name),
                                            ResolvedImport::Env(found_env_id),
                                            segment,
                                        );
                                        continue;
                                    }

                                    let symbol_id = found_env.variables.find_exported(&symbol_name);

                                    if let Some(symbol_id) = symbol_id {
                                        let resolved =
                                            ResolvedSymbol::new(reef_id, found_env_id, symbol_id);
                                        imports.set_resolved_import(
                                            alias.unwrap_or(symbol_name),
                                            ResolvedImport::Symbol(resolved),
                                            segment,
                                        );
                                        continue;
                                    }
                                    // the symbol inside the resolved environment could not be found
                                    let diagnostic = diagnose_unresolved_import(
                                        env_id,
                                        name,
                                        Some(found_env.fqn.clone()),
                                        segment.clone(),
                                    );
                                    imports.set_resolved_import(
                                        alias.unwrap_or(symbol_name),
                                        ResolvedImport::Dead,
                                        segment,
                                    );
                                    diagnostics.push(diagnostic);
                                }
                            }
                        }
                    }
                }

                //if the unresolved import is an 'AllIn' import, meaning that it imports all symbols from given module
                UnresolvedImport::AllIn(loc) => {
                    match resolve_loc(&loc, context) {
                        None => diagnostics.push(
                            Diagnostic::new(
                                DiagnosticID::ImportResolution,
                                format!("unable to find reef `{}`.", loc.name.root()),
                            )
                            .with_observation(Observation::new(
                                SourceLocation::new(env_id, segment),
                            )),
                        ),
                        Some((reef, _)) => {
                            let name = loc.name;
                            // try to get referenced environment of the import
                            match get_mod(&name, &reef.engine) {
                                None => {
                                    // if the environment wasn't found, and its name was already known, push a diagnostic as it does not exists
                                    let diagnostic = diagnose_unresolved_import(
                                        env_id,
                                        &name,
                                        None,
                                        segment.clone(),
                                    );
                                    diagnostics.push(diagnostic);
                                }
                                Some((env_id, env)) => {
                                    for var in env.variables.exported_vars() {
                                        let var_id =
                                            env.variables.find_exported(&var.name).unwrap();
                                        let import_symbol =
                                            ResolvedSymbol::new(context.reef_id, env_id, var_id);
                                        imports.set_resolved_import(
                                            var.name.clone(),
                                            ResolvedImport::Symbol(import_symbol),
                                            segment.clone(),
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Gets a module environment from the given fully qualified name, then pop the name until it finds a valid environment.
/// returns None if the name's root could not get resolved
fn get_mod<'a>(name: &Name, engine: &'a Engine) -> Option<(SourceId, &'a Environment)> {
    let mut env_name = Some(name.clone());
    while let Some(name) = env_name {
        if let Some((id, env)) = engine.find_environment_by_name(&name) {
            if env.parent.is_none() {
                return Some((id, env));
            }
        }
        env_name = name.tail();
    }
    None
}
