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
