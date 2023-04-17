use std::fmt::{Display, Formatter};

#[derive(Debug, PartialOrd, Ord, Clone, PartialEq, Eq, Hash, Default)]
pub struct Name {
    pub path: Vec<String>,
    pub name: String
}

impl Name {
    pub fn new(name: &str) -> Self {
        let elements: Vec<String> = name.split("::").into_iter().map(|s| s.to_string()).collect();
        let (name, path) = elements.split_last().unwrap();

        Self {
            path: path.into(),
            name: name.clone(),
        }
    }

    pub fn part_count(&self) -> usize {
        self.path.len() + 1
    }

    pub fn with_name(self, name: &str) -> Self {
        Self {
            path: self.path,
            name: name.to_string()
        }
    }

    pub fn relative_to(&self, other: Name) -> Option<Name> {
        let parts: Vec<_> = other.parts()
            .clone()
            .into_iter()
            .zip(self.parts())
            .skip_while(|(a, b)| a == b)
            .map(|(a, _)| a)
            .collect();

        if parts.is_empty() {
            return None
        }
        Some(Name::from(parts))
    }

    pub fn parts(&self) -> Vec<String> {
        let mut vec = self.path.clone();
        vec.push(self.name.clone());
        vec
    }

    pub fn root(&self) -> &str {
        self.path.first().unwrap_or(&self.name)
    }

    pub fn child(&self, name: &str) -> Self {
        let mut path = self.path.clone();
        path.push(self.name.to_string());
        Self {
            path: path,
            name: name.to_string()
        }
    }

    pub fn tail(&self) -> Option<Self> {
        if self.path.is_empty() {
            return None
        }
        self.parts()
            .split_first()
            .map(|(_, tail)| Name::from(tail.to_vec()))
    }

    pub fn appended(&self, mut name: Self) -> Self {
        let mut path = self.path.clone();
        path.push(self.name.to_string());
        path.append(&mut name.path);
        Self {
            path,
            name: name.name
        }
    }
}

impl From<Vec<String>> for Name {
    fn from(value: Vec<String>) -> Self {
        if let Some((name, path)) = value.split_last() {
            return Self {
                path: path.to_vec(),
                name: name.to_string()
            }
        }
        panic!("empty vec input")
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for module in &self.path {
            write!(f, "{}::", module)?;
        }
        write!(f, "{}", self.name)?;
        Ok(())
    }
}