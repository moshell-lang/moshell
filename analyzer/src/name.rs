use std::fmt::{Display, Formatter};

#[derive(Debug, PartialOrd, Ord, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    parts: Vec<String>,
}

impl Name {
    pub fn new(name: &str) -> Self {
        let parts: Vec<String> = name
            .split("::")
            .map(|s| s.to_string())
            .collect();

        Self { parts }
    }

    pub fn with_name(mut self, name: &str) -> Self {
        let last_idx = self.parts.len() - 1;
        self.parts[last_idx] = name.to_string();
        self
    }

    pub fn parts(&self) -> &[String] {
        &self.parts
    }

    pub fn into_vec(self) -> Vec<String> {
        self.parts
    }

    pub fn relative_to(&self, other: &Name) -> Option<Name> {
        let common_parts_len = other
            .parts
            .clone()
            .into_iter()
            .zip(&self.parts)
            .take_while(|(a, b)| a == *b)
            .count();

        if common_parts_len == self.parts.len() {
            return None;
        }
        let parts: Vec<_> = self
            .parts
            .clone()
            .into_iter()
            .skip(common_parts_len)
            .collect();
        Some(Name::from(parts))
    }

    pub fn path(&self) -> &[String] {
        self.parts.split_last().unwrap().1 //Names cannot be empty
    }

    pub fn root(&self) -> &str {
        self.parts.first().unwrap() //Names cannot be empty
    }

    pub fn name(&self) -> &str {
        self.parts.last().unwrap() //Names cannot be empty
    }

    pub fn child(&self, name: &str) -> Self {
        let mut parts = self.parts.clone();
        parts.push(name.to_string());
        Self { parts }
    }

    pub fn tail(&self) -> Option<Self> {
        if self.path().is_empty() {
            return None;
        }
        self.parts
            .split_first()
            .map(|(_, tail)| Name::from(tail.to_vec()))
    }

    pub fn appended(&self, mut name: Self) -> Self {
        let mut parts = self.parts.clone();
        parts.append(&mut name.parts);
        Self { parts }
    }
}

impl From<Vec<String>> for Name {
    fn from(value: Vec<String>) -> Self {
        if value.is_empty() {
            panic!("empty vec input")
        }
        Self { parts: value }
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some((name, tail)) = self.parts.split_last() {
            for module in tail {
                write!(f, "{module}::")?;
            }
            write!(f, "{name}")?;
        }
        Ok(())
    }
}
