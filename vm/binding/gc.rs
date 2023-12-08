use crate::value::VmValue;
use crate::{moshell_vm_gc_collect, moshell_vm_gc_run, VmFFI};

pub struct GC {
    pub(crate) vm: VmFFI,
}

impl GC {
    pub fn run(&mut self) {
        unsafe { moshell_vm_gc_run(self.vm) }
    }

    pub fn collect(&mut self) -> Vec<VmValue> {
        unsafe {
            let result = moshell_vm_gc_collect(self.vm);

            let size = result.collected_objects_count as usize;
            let mut vec = Vec::with_capacity(size);

            for i in 0..size {
                let obj = *(result.collected_objects.wrapping_add(i));
                vec.push(VmValue::deduce(obj))
            }
            vec
        }
    }
}
