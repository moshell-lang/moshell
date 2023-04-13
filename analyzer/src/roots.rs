use std::cell::RefCell;
use std::rc::Rc;
use crate::environment::Environment;

pub struct Roots {
    roots: Vec<EnvNode>
}

pub struct EnvNode {
    env: Rc<RefCell<Environment>>,
    childs: Vec<EnvNode>
}