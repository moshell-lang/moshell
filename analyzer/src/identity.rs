use std::fmt::{Display, Formatter};

#[derive(Debug, PartialOrd, Ord, Clone, PartialEq, Eq, Hash, Default)]
pub struct Name {
    pub absolute_path: Vec<String>,
    pub name: String
}

impl Name {

    pub fn new(name: &str) -> Self {
        let elements: Vec<String> = name.split("::").into_iter().map(|s| s.to_string()).collect();
        let (name, path) = elements.split_last().unwrap();

        Self {
            absolute_path: path.into(),
            name: name.clone()
        }
    }

    pub fn parts(&self) -> Vec<String> {
        let mut vec = self.absolute_path.clone();
        vec.push(self.name.clone());
        vec
    }

    pub fn root(&self) -> &str {
        self.absolute_path.first().unwrap_or(&self.name)
    }

    pub fn child(&self, name: &str) -> Name {
        let mut path = self.absolute_path.clone();
        path.push(self.name.to_string());
        Name {
            absolute_path: path,
            name: name.to_string()
        }
    }

    pub fn appended(&self, mut name: Name) -> Name {
        let mut path = self.absolute_path.clone();
        path.push(self.name.to_string());
        path.append(&mut name.absolute_path);
        Name {
            absolute_path: path,
            name: name.name
        }
    }
}

impl From<Vec<String>> for Name {
    fn from(value: Vec<String>) -> Self {
        if let Some((name, path)) = value.split_last() {
            return Self {
                absolute_path: path.to_vec(),
                name: name.to_string()
            }
        }
        panic!()
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for module in &self.absolute_path {
            write!(f, "{}::", module)?;
        }
        write!(f, "{}", self.name)?;
        Ok(())
    }
}