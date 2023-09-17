use crate::VmValueFFI;

#[derive(Clone, PartialEq, Debug)]
pub enum VmValue {
    Void,
    Byte(u8),
    Int(i64),
    Double(f64),
    String(String),
    Vec(Vec<VmValue>),
}

impl From<Vec<&str>> for VmValue {
    fn from(value: Vec<&str>) -> Self {
        let vec = value
            .into_iter()
            .map(|s| Self::String(s.to_string()))
            .collect();

        Self::Vec(vec)
    }
}

impl From<&str> for VmValue {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}
