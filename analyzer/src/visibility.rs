use crate::name::Name;
use crate::visibility::ScopeVisibility::SymbolOnly;

#[derive(Clone, PartialEq, Debug)]
pub enum ScopeVisibility {
    Public,
    ModuleOnly,
    SymbolOnly { symbol: Name },
}

impl ScopeVisibility {
    pub fn does_accept(&self, fqn: Name) -> bool {
        match &self {
            SymbolOnly { symbol } => fqn
                .parts
                .starts_with(&symbol.parts),
            _ => true,
        }
    }
}
