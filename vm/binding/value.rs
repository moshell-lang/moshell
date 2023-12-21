use crate::{VmObjectFFI, VmObjectType};

#[derive(Clone, PartialEq, Debug)]
pub enum VmValue {
    Void,
    Byte(u8),
    Int(i64),
    Double(f64),
    String(String),
    Vec(Vec<Option<VmValue>>),
    Struct(Vec<Option<VmValue>>),
}

impl<T: Into<VmValue>> From<Vec<T>> for VmValue {
    fn from(value: Vec<T>) -> Self {
        let vec = value.into_iter().map(|s| Some(s.into())).collect();

        Self::Vec(vec)
    }
}

impl From<i64> for VmValue {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<f64> for VmValue {
    fn from(value: f64) -> Self {
        Self::Double(value)
    }
}

impl From<&str> for VmValue {
    fn from(value: &str) -> Self {
        Self::String(value.to_owned())
    }
}

impl VmValue {
    /// retrieve maximum amount of information from a given value
    /// In case of structures, as the layout isn't provided by FFIs, the
    /// returned value is an empty structure.
    pub fn deduce(value: VmObjectFFI) -> Self {
        unsafe {
            match value.0 {
                VmObjectType::Int => VmValue::Int(value.unbox().get_as_i64()),
                VmObjectType::Double => VmValue::Double(value.unbox().get_as_double()),
                VmObjectType::Byte => VmValue::Byte(value.unbox().get_as_u8()),
                VmObjectType::Str => VmValue::String(value.get_as_string()),
                VmObjectType::Vec => VmValue::Vec(
                    value
                        .get_as_vec()
                        .into_iter()
                        .map(|v| Some(VmValue::deduce(v)))
                        .collect(),
                ),
                VmObjectType::Struct => {
                    // we have no information about
                    VmValue::Struct(vec![])
                }
            }
        }
    }
}
