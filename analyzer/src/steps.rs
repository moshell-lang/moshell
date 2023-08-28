use crate::diagnostic::Diagnostic;
use crate::importer::ASTImporter;
use crate::name::Name;
use crate::reef::ReefContext;
use crate::steps::collect::SymbolCollector;
use crate::steps::resolve::SymbolResolver;
use crate::ResolutionResult;

pub mod collect;
pub mod resolve;
mod shared_diagnostics;
pub mod typing;

pub(super) fn resolve_sources<'ca, 'e>(
    mut to_visit: Vec<Name>,
    result: &mut ResolutionResult,
    importer: &mut impl ASTImporter<'e>,
    context: &mut ReefContext<'ca, 'e>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    while !to_visit.is_empty() {
        diagnostics.extend(SymbolCollector::collect_symbols(
            &mut result.imports,
            context,
            &mut to_visit,
            &mut result.visited,
            importer,
        ));
        diagnostics.extend(SymbolResolver::resolve_symbols(
            &mut result.imports,
            context,
            &mut to_visit,
            &result.visited,
        ));
        // The cycle ended, if `to_visit` is still non empty, a new cycle will be started
        // to resolve the modules to visit and so on
    }
}
