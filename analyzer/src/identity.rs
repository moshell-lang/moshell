use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Identity {
    pub absolute_path: Vec<String>, //TODO use &'a str to optimize space
    pub name: String
}

impl Display for Identity {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for module in &self.absolute_path {
            write!(f, "{}::", module)?;
        }
        write!(f, "{}", self.name)?;
        Ok(())
    }
}