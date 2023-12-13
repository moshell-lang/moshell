use crate::engine::Engine;
use crate::environment::symbols::SymbolRegistry;
use crate::environment::Environment;
use crate::imports::{ResolvedImport, SourceImports};
use crate::name::Name;
use crate::reef::{Externals, ReefId};
use crate::relations::{ResolvedSymbol, SourceId};

/// The result of a symbol resolution attempt
#[derive(PartialEq)]
pub enum SymbolResolutionResult {
    /// The symbol is resolved, where `ResolvedSymbol` is the resolved symbol
    Resolved(ResolvedSymbol),
    /// The symbol is resolved, but it's invalid. This result usually implies a diagnostic emission.
    /// Where the `ResolvedSymbol` is the resolved symbol
    Invalid(ResolvedSymbol),
    /// The symbol is imported but its import was invalidated
    DeadImport,
    /// The symbol could not be found.
    NotFound,
}

pub fn resolve_symbol_from_locals(
    env_id: SourceId,
    env: &Environment,
    symbol_name: &Name,
    current_reef: ReefId,
    registry: SymbolRegistry,
) -> SymbolResolutionResult {
    if let Some(var_id) = env.symbols.find_exported(symbol_name.root(), registry) {
        let symbol = ResolvedSymbol {
            reef: current_reef,
            source: env_id,
            object_id: var_id,
        };
        if symbol_name.is_qualified() {
            return SymbolResolutionResult::Invalid(symbol);
        }
        return SymbolResolutionResult::Resolved(symbol);
    }
    if registry != SymbolRegistry::Objects {
        return SymbolResolutionResult::NotFound;
    }

    // If the symbol is an object, but it exists a type symbol, return it :
    // if the symbol should not be a type, the analyzer will report it.
    // It exists a case where a type can be used as a function call: when calling a type constructor.
    if let Some(var_id) = env
        .symbols
        .find_exported(symbol_name.root(), SymbolRegistry::Types)
    {
        let symbol = ResolvedSymbol {
            reef: current_reef,
            source: env_id,
            object_id: var_id,
        };
        return SymbolResolutionResult::Resolved(symbol);
    }

    SymbolResolutionResult::NotFound
}

pub fn resolve_absolute_symbol(
    engine: &Engine,
    name: &Name,
    reef: ReefId,
    registry: SymbolRegistry,
) -> SymbolResolutionResult {
    // As we could not resolve the symbol using imports, try to find the symbol from
    // an absolute qualified name
    let env_name = name.tail().unwrap_or(name.clone());
    let env_result = engine.find_environment_by_name(&env_name);

    if let Some((env_id, env)) = env_result {
        return resolve_symbol_from_locals(
            env_id,
            env,
            &Name::new(name.simple_name()),
            reef,
            registry,
        );
    }
    SymbolResolutionResult::NotFound
}

/// resolves the symbols of given environment, using given resolved imports for external symbol references.
pub fn resolve_symbol_from_imports(
    engine: &Engine,
    imports: &SourceImports,
    name: &Name,
    externals: &Externals,
    registry: SymbolRegistry,
) -> SymbolResolutionResult {
    let name_root = name.root();

    // try to resolve the relation by looking the name's root inside imports
    match imports.get_import(name_root) {
        Some(ResolvedImport::Symbols(resolved_symbol)) => {
            if let Some(resolved_symbol) = resolved_symbol.get(&registry) {
                return if !name.is_qualified() {
                    SymbolResolutionResult::Resolved(*resolved_symbol)
                } else {
                    SymbolResolutionResult::Invalid(*resolved_symbol)
                };
            }
        }
        Some(ResolvedImport::Env {
            reef: resolved_reef,
            source: resolved_module,
        }) => {
            let env = if *resolved_reef == externals.current {
                engine
            } else {
                &externals
                    .get_reef(*resolved_reef)
                    .expect("resolved import points to an unknown reef")
                    .engine
            }
            .get_environment(*resolved_module)
            .expect("resolved import points to an unknown environment");

            return resolve_symbol_from_locals(
                *resolved_module,
                env,
                &Name::new(name.simple_name()),
                *resolved_reef,
                registry,
            );
        }
        Some(ResolvedImport::Dead) => return SymbolResolutionResult::DeadImport,
        None => {} //simply fallback to NotFound
    };
    SymbolResolutionResult::NotFound
}
