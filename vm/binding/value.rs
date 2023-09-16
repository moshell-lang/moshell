use crate::VmValueFFI;

pub enum VmValue {
    Byte(i8),
    Int(i64),
    Double(f64),
    String(String),
    Vec(Vec<VmValue>)
}

impl From<VmValueFFI> for VmValue {
    fn from(value: VmValueFFI) -> Self {
        unsafe {
            match value.val_type {
                0 => VmValue::Byte(*value.ptr.cast::<i8>()),
                1 => VmValue::Int(*value.ptr.cast::<i64>()),
                2 => VmValue::Double(*value.ptr.cast::<f64>()),
                3 => VmValue::String((*value.ptr.cast::<&str>()).to_string()),
                4 => {
                    let slice = *value.ptr.cast::<&[VmValueFFI]>();
                    let vec = slice.iter()
                        .copied()
                        .map(VmValue::from)
                        .collect();
                    VmValue::Vec(vec)
                },
                _ => panic!("unknown moshell object of type {}", value.val_type)
            }
        }
    }
}