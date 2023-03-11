///a `use x, y, z` expression
#[derive(Debug, Clone, PartialEq, dbg_pls::DebugPls)]
pub struct Use<'a> {
    ///all the used variables/functions etc names
    pub uses: Vec<&'a str>,
}
