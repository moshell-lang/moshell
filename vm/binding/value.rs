use crate::{VmObjectFFI, VmObjectType, VmValueFFI};

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
impl From<VmObjectFFI> for VmValue {
    fn from(value: VmObjectFFI) -> Self {
        unsafe {
            match value.0 {
                VmObjectType::Int => VmValue::Int(value.unbox().get_as_i64()),
                VmObjectType::Double => VmValue::Double(value.unbox().get_as_double()),
                VmObjectType::Str => VmValue::String(value.get_as_string()),
                VmObjectType::Vec => {
                    VmValue::Vec(value.get_as_vec().into_iter().map(VmValue::from).collect())
                }
            }
        }
    }
}
