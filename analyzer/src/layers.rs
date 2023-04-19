use crate::environment::Environment;
use crate::name::Name;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct ModuleLayers {
    roots: HashMap<String, Module>,
}

impl ModuleLayers {

    //declares a new ModuleLayers that contains the lang environment
    pub fn new() -> Rc<RefCell<Self>> {
        let layers = Rc::new(RefCell::new(ModuleLayers::default()));
        let lang = Environment::lang(layers.clone());
        layers.borrow_mut()
            .roots
            .insert(lang.fqn.name().to_string(), Module::with_env(lang));

        layers
    }

    pub fn get_env(&self, env_fqn: &Name) -> Option<Rc<RefCell<Environment>>> {
        self.retrieve_env(env_fqn, false)
    }

    pub fn get_env_of(&self, symbol_fqn: &Name) -> Option<Rc<RefCell<Environment>>> {
        self.retrieve_env(symbol_fqn, true)
    }

    fn retrieve_env(&self, name: &Name, of_symbol: bool) -> Option<Rc<RefCell<Environment>>> {

        let root_name = name.root();

        let mut module = self.roots.get(root_name);

        for name in name.path() {
            match module {
                Some(m) => {
                    let m = m.childs.get(name);
                    if m.is_none() {
                        if of_symbol {
                            break
                        }
                        return None
                    }
                    module = m
                },
                None => break
            }
        }

        module.and_then(|m| m.env.clone())
    }

    pub fn declare_env(layers: Rc<RefCell<Self>>, name: Name) -> Result<Rc<RefCell<Environment>>, String> {
        let env = Rc::new(RefCell::new(Environment::new(name.clone(), layers.clone())));

        let mut layers_ref = layers.borrow_mut();
        let module = layers_ref.declare_module(name)?;

        module.env = Some(env.clone());
        Ok(env)
    }

    fn declare_module(&mut self, name: Name) -> Result<&mut Module, String> {

        let root_name = name.root();

        let mut module= match self.roots.entry(root_name.to_string()) {
            Entry::Occupied(o) => o.into_mut(),
            Entry::Vacant(v) => v.insert(Module::new(Name::new(&root_name)))
        };

        for name in name.path() {
            module = module.declare_module(name.clone())?
        }

        Ok(module)
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Module {
    pub full_name: Name,
    pub env: Option<Rc<RefCell<Environment>>>,
    childs: HashMap<String, Module>,
}

impl Module {
    fn new(full_name: Name) -> Self {
        Self {
            full_name,
            env: None,
            childs: HashMap::new(),
        }
    }

    fn with_env(env: Environment) -> Self {
        Self {
            full_name: env.fqn.clone(),
            env: Some(Rc::new(RefCell::new(env))),
            childs: HashMap::new()
        }
    }

    fn declare_module(&mut self, name: String) -> Result<&mut Module, String> {
        match self.childs.entry(name.clone()) {
            Entry::Occupied(_) => {
                Err(format!(
                    "cannot declare a new module: {}::{} already exists",
                    self.full_name, name
                ))
            }
            Entry::Vacant(v) => {
                let module = Module::new(self.full_name.child(&name));
                v.insert(module.clone());
                //we can safely unwrap as we just inserted the module
                Ok(self.childs.get_mut(&name).unwrap())
            }
        }
    }



}
