use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::environment::Environment;

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Roots {
    roots: HashMap<String, Environment>
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Module {
    name: String,
    env: Option<Rc<RefCell<Environment>>>,
    childs: HashMap<String, Module>
}