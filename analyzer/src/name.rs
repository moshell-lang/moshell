use std::fmt::{Display, Formatter};

///The name of a symbol, a module or a context.
#[derive(Debug, PartialOrd, Ord, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    parts: Vec<String>,
}

impl Name {
    ///Parses a new name from the given string.
    pub fn new(name: &str) -> Self {
        let parts: Vec<String> = name.split("::").map(|s| s.to_string()).collect();

        Self { parts }
    }

    /// Tests if this name contains a path and a simple name.
    pub fn is_qualified(&self) -> bool {
        self.parts.len() != 1
    }

    pub fn qualified(mut path: Vec<String>, name: String) -> Self {
        path.push(name);
        Self::from(path)
    }

    ///Creates a new Name with the simple name changed with given input
    pub fn with_name(mut self, simple_name: &str) -> Self {
        let last_idx = self.parts.len() - 1;
        self.parts[last_idx] = simple_name.to_string();
        self
    }

    ///The parts of this Name
    pub fn parts(&self) -> &[String] {
        &self.parts
    }

    ///Convert this Name in a Vec<String>
    pub fn into_vec(self) -> Vec<String> {
        self.parts
    }

    ///Creates a new name relative to given input.
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

    ///returns an iterator over the prefixed path of the name
    pub fn path(&self) -> &[String] {
        self.parts.split_last().unwrap().1 //Names cannot be empty
    }

    ///returns the name's root (its very first part)
    pub fn root(&self) -> &str {
        self.parts.first().unwrap() //Names cannot be empty
    }

    ///returns the simple name of name
    pub fn simple_name(&self) -> &str {
        self.parts.last().unwrap() //Names cannot be empty
    }

    ///Creates a new name with this name as a prefixed path
    pub fn child(&self, name: &str) -> Self {
        let mut parts = self.parts.clone();
        parts.push(name.to_string());
        Self { parts }
    }

    ///Returns the tail of the name (the name without it's root part, or None if this name have only one part)
    pub fn tail(&self) -> Option<Self> {
        if self.path().is_empty() {
            return None;
        }
        self.parts
            .split_last()
            .map(|(_, tail)| Name::from(tail.to_vec()))
    }

    ///Returns a name with given name merged
    pub fn appended(&self, mut name: Self) -> Self {
        let mut parts = self.parts.clone();
        parts.append(&mut name.parts);
        Self { parts }
    }
}

impl From<Vec<String>> for Name {
    fn from(value: Vec<String>) -> Self {
        assert!(!value.is_empty(), "cannot create a name from an empty vec");
        Self { parts: value }
    }
}

impl From<&[String]> for Name {
    fn from(value: &[String]) -> Self {
        value.to_vec().into()
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
