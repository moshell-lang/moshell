use dbg_pls::DebugPls;

#[derive(Debug, Clone, PartialEq, DebugPls)]
pub struct Type<'a> {
    pub name: &'a str,
    pub params: Vec<Type<'a>>,
}
