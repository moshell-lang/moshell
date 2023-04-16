use crate::environment::Environment;
use crate::name::Name;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Default, Debug, Clone)]
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
            .insert(lang.identity.name.clone(), Module::with_env(lang));

        layers
    }

    pub fn get_env(&self, name: Name) -> Option<Rc<RefCell<Environment>>> {
        let mut names = name.path.into_iter().chain(vec![name.name].into_iter());

        //we can safely unwrap here as names has at least `name.name` as element
        let root_name = names.next().unwrap();

        let mut module = self.roots.get(&root_name);

        for name in names {
            match module {
                Some(m) => module = m.childs.get(&name),
                None => return None
            }
        }

        module.and_then(|m| m.env.clone())
    }

    pub fn declare_env(layers: Rc<RefCell<Self>>, name: Name) -> Result<Rc<RefCell<Environment>>, String> {
        let mut layers_ref = layers.borrow_mut();
        let module = layers_ref.declare_module(name.clone())?;

        let env = Rc::new(RefCell::new(Environment::new(name, layers.clone())));

        module.env = Some(env.clone());
        Ok(env)
    }

    fn declare_module(&mut self, name: Name) -> Result<&mut Module, String> {
        let mut names = name.path.into_iter().chain(vec![name.name].into_iter());

        //we can safely unwrap here as names has at least `name.name` as element
        let root_name = names.next().unwrap();

        let mut module= match self.roots.entry(root_name.clone()) {
            Entry::Occupied(o) => o.into_mut(),
            Entry::Vacant(v) => v.insert(Module::new(Name::new(&root_name)))
        };

        for name in names {
            module = module.declare_module(name)?
        }

        Ok(module)
    }
}

#[derive(Default, Debug, Clone)]
pub struct Module {
    full_name: Name,
    env: Option<Rc<RefCell<Environment>>>,
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
            full_name: env.identity.clone(),
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
