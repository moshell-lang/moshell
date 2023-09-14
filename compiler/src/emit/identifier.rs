use crate::constant_pool::ConstantPool;
use crate::emit::EmitterContext;
use analyzer::relations::{LocalId, ResolvedSymbol};
use analyzer::types::hir::Var;

/// An identifier for a variable in the bytecode.
pub(super) enum Identifier {
    /// A local variable stored in the classical stack.
    Local(LocalId),

    /// A local variable that escape the current scope due to a closure.
    Capture(ResolvedSymbol),

    /// An external variable, that is stored separately in a memory page.
    External(u32),
}

/// Converts a symbol to an identifier for the runtime.
///
/// The runtime needs to treat certain variables as exported to store them separately.
/// It mays refers to a local variable or an external symbol, so it needs to be converted
/// to an identifier before being emitted.
pub(super) fn expose_variable(ctx: &EmitterContext, var: Var, cp: &mut ConstantPool) -> Identifier {
    match var {
        Var::Local(id) => {
            let variable = ctx
                .environment
                .symbols
                .get(id)
                .expect("The declared variable should be in the current environment.");
            if variable.is_exported() && ctx.environment.is_script {
                let name = &variable.name;
                let symbol_id = cp
                    .get_external(name)
                    .expect("External symbol not previously emitted");
                Identifier::External(symbol_id)
            } else {
                Identifier::Local(id)
            }
        }
        Var::External(resolved) => {
            // Distinguish captures and static variables.
            let environment = ctx
                .get_engine(resolved.reef)
                .unwrap()
                .get_environment(resolved.source)
                .expect("Resolved relation targets an unknown environment");
            let variable = environment
                .symbols
                .get(resolved.object_id)
                .expect("Resolved relation targets an unknown variable");
            let is_exported_dynsym = variable.is_exported() && environment.is_script;
            if is_exported_dynsym {
                let import = &environment.fqn;
                let name = &variable.name;
                Identifier::External(cp.insert_dynsym(&import.to_string(), name))
            } else {
                Identifier::Capture(resolved)
            }
        }
    }
}
