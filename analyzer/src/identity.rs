use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Identity {
    pub absolute_path: Vec<String>, //TODO use &'a str to optimize space
    pub name: String
}

impl Identity {


    pub fn new(fqn: &str) -> Result<Self, String> {
        let elements: Vec<String> = fqn.split("::").into_iter().map(|s| s.to_string()).collect();
        let (name, path) = elements.split_last().ok_or("empty input string")?;

        Ok(Self {
            absolute_path: path.into(),
            name: name.clone()
        })
    }

    pub fn child(&self, name: &str) -> Identity {
        let mut path = self.absolute_path.clone();
        path.push(self.name.to_string());
        Identity {
            absolute_path: path,
            name: name.to_string()
        }
    }
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