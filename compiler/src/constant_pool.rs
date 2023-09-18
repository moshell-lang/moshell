use indexmap::IndexSet;

/// Contains the constants defined in a module constant pool
#[derive(Default)]
pub struct ConstantPool {
    pub strings: IndexSet<String>,

    /// The symbols that the module needs to find at runtime.
    pub dynsym: IndexSet<u32>,

    /// The symbols that the module expose to the outside world.
    ///
    /// All symbols names are expected to be unique and stored in the
    /// [`ConstantPool::strings`] pool.
    pub exported: Vec<ExportedSymbol>,
}

/// A symbol exported by a module.
///
/// A bytecode unit relies on the name of the exported symbol, that will be offset
/// at runtime by the linker.
#[derive(Debug, Clone)]
pub struct ExportedSymbol {
    pub name_index: u32,
    pub local_offset: u32,
    pub is_obj_ref: bool,
}

impl ConstantPool {
    /// inserts if not already contained a string in the constant pool, returning it's pool identifier
    pub fn insert_string(&mut self, str: impl ToString) -> u32 {
        let constant_index = self.strings.insert_full(str.to_string()).0;
        u32::try_from(constant_index).expect("constant pool exceeded max capacity")
    }

    /// Adds a new symbol that will need to be found at runtime.
    pub fn insert_dynsym(&mut self, import: &str, symbol: &str) -> u32 {
        self.strings.insert(import.to_owned());
        let dynsym = self.insert_string(symbol);
        self.dynsym.insert_full(dynsym).0 as u32
    }

    /// References a new symbol that is provided by the current module.
    ///
    /// While this method shouldn't be used for variables that are shadowed later in the code,
    /// it is not an actual problem as the linker will resolve the correct symbol.
    pub fn insert_exported(&mut self, symbol: &str, local_offset: u32, is_obj_ref: bool) {
        let name_index = self.insert_string(symbol);
        self.exported.push(ExportedSymbol {
            name_index,
            local_offset,
            is_obj_ref,
        });
        self.dynsym.insert_full(name_index);
    }

    /// Gets the index of an external symbol string in the constant pool.
    pub fn get_external(&self, symbol: &str) -> Option<u32> {
        self.strings
            .get_index_of(symbol)
            .and_then(|id| self.dynsym.get_index_of(&(id as u32)).map(|id| id as u32))
    }
}
