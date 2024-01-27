use std::ffi;
use std::ffi::CStr;

use context::source::ContentId;

use crate::gc::GC;

pub mod gc;
pub mod value;

/// Executes the given bytecode.
///
/// The execution will block the thread until the Moshell program terminates.
///
/// # Safety
/// If the given bytecode is invalid, this function can cause undefined behavior.
/// The caller must ensure that the given bytecode is valid.
pub unsafe fn execute_bytecode(bytes: &[u8]) -> Result<(), VmError> {
    match moshell_exec(bytes.as_ptr(), bytes.len()) {
        0 => Ok(()),
        1 => Err(VmError::Panic),
        _ => Err(VmError::Internal),
    }
}

#[repr(C)]
#[derive(Copy, Clone)]
struct VmFFI(*mut ffi::c_void);

/// A virtual machine that can execute Moshell bytecode.
pub struct VM {
    ffi: VmFFI,
    pub gc: GC,
}

/// An error that occurred during the VM lifetime.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VmError {
    Panic,
    Internal,
}

impl VM {
    /// Creates a new virtual machine.
    pub fn new(args: Vec<String>) -> Self {
        let ffi = if cfg!(not(miri)) {
            unsafe {
                let lens: Vec<_> = args.iter().map(|s| s.len()).collect();
                let args: Vec<_> = args.iter().map(|s| s.as_ptr()).collect();
                moshell_vm_init(args.as_ptr().cast(), lens.len(), lens.as_ptr())
            }
        } else {
            VmFFI(std::ptr::null_mut())
        };
        Self {
            ffi,
            gc: GC { vm: ffi },
        }
    }

    /// Appends new bytecode to the VM.
    ///
    /// # Safety
    /// An invalid bytecode will almost certainly result in a deterministic error during loading,
    /// or a non-deterministic error during execution.
    pub fn register(&mut self, bytes: &[u8]) -> Result<(), VmError> {
        if cfg!(miri) {
            return Ok(()); // Not supported
        }
        unsafe { moshell_vm_register(self.ffi, bytes.as_ptr(), bytes.len()) != -1 }
            .then_some(())
            .ok_or(VmError::Internal)
    }

    /// Executes the remaining bytecode.
    ///
    /// # Safety
    /// The caller must ensure that the previously registered bytecode is valid.
    pub unsafe fn run(&mut self) -> Result<(), VmError> {
        if cfg!(miri) {
            return Ok(()); // Not supported
        }
        match moshell_vm_run(self.ffi) {
            0 => Ok(()),
            1 => Err(VmError::Panic),
            _ => Err(VmError::Internal),
        }
    }

    /// Sets the process group ID if running in a terminal.
    ///
    /// Forked processes will be attached to this process group.
    pub fn set_pgid(&mut self, pgid: i32) {
        if cfg!(miri) {
            return; // Not supported
        }
        unsafe { moshell_set_pgid(self.ffi, pgid) }
    }

    /// Gets the next page of bytecode to be executed.
    pub fn get_next_page(&self) -> ContentId {
        ContentId(unsafe { moshell_vm_next_page(self.ffi) })
    }

    pub fn get_exported_var(&self, name: &str) -> VmValueFFI {
        unsafe { moshell_vm_get_exported(self.ffi, name.as_ptr().cast(), name.len()) }
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new(vec![])
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        if cfg!(not(miri)) {
            unsafe { moshell_vm_free(self.ffi) }
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone)]
struct VmArrayFFI(usize, *mut VmValueFFI);
#[repr(C)]
#[derive(Copy, Clone)]
struct VmStructureFFI(usize, *mut ffi::c_char);

#[repr(C)]
#[derive(Copy, Clone)]
pub union VmValueFFI {
    int: i64,
    byte: u8,
    double: f64,
    ptr: *const ffi::c_void,
}

impl VmValueFFI {
    pub fn ptr(ptr: *const ffi::c_void) -> Self {
        Self { ptr }
    }
}

#[repr(C)]
#[derive(Copy, Clone)]
#[allow(dead_code)]
enum VmObjectType {
    Str,
    Int,
    Byte,
    Double,
    Vec,
    Struct,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct VmObjectFFI(VmObjectType, *mut ffi::c_void);

impl VmValueFFI {
    /// # Safety
    /// The caller must ensure that the value has the correct type.
    pub unsafe fn get_as_u8(self) -> u8 {
        moshell_value_get_as_byte(self)
    }
    /// # Safety
    /// The caller must ensure that the value has the correct type.
    pub unsafe fn get_as_i64(self) -> i64 {
        moshell_value_get_as_int(self)
    }
    /// # Safety
    /// The caller must ensure that the value has the correct type.
    pub unsafe fn get_as_double(self) -> f64 {
        moshell_value_get_as_double(self)
    }
    /// # Safety
    /// The caller must ensure that the value has the correct type.
    pub unsafe fn get_as_obj(self) -> VmObjectFFI {
        moshell_value_get_as_object(self)
    }

    pub fn is_ptr_null(self) -> bool {
        unsafe { self.ptr.is_null() }
    }
}

impl VmObjectFFI {
    /// # Safety
    /// The caller must ensure that the value has the correct type.
    pub unsafe fn unbox(self) -> VmValueFFI {
        moshell_object_unbox(self)
    }
    /// # Safety
    /// The caller must ensure that the value has the correct type.
    pub unsafe fn get_as_string(self) -> String {
        let buff = moshell_object_get_as_string(self);
        let c_str = CStr::from_ptr(buff.cast_mut());
        let str = c_str.to_str().expect("utf8 error");
        str.to_string()
    }
    /// # Safety
    /// The caller must ensure that the value has the correct type.
    pub unsafe fn get_as_vec(self) -> Vec<VmObjectFFI> {
        let msh_array = moshell_object_get_as_array(self);
        let mut vec = Vec::with_capacity(msh_array.0);

        for i in 0..msh_array.0 {
            let val = *msh_array.1.wrapping_add(i);
            vec.push(val.get_as_obj())
        }
        vec
    }

    /// # Safety
    /// The caller must ensure that the value has the correct type.
    pub unsafe fn get_as_struct(self) -> Vec<u8> {
        let msh_struct = moshell_object_get_as_struct(self);
        let mut vec = Vec::with_capacity(msh_struct.0);
        for i in 0..msh_struct.0 {
            vec.push(*msh_struct.1.add(i) as u8)
        }
        vec
    }
}

#[repr(C)]
#[derive(Clone)]
struct VmGcResultFFI {
    collected_objects_count: u64,
    collected_objects: *const VmObjectFFI,
}

impl Drop for VmGcResultFFI {
    fn drop(&mut self) {
        unsafe { gc_collection_result_free(self.clone()) }
    }
}

#[link(name = "vm", kind = "static")]
extern "C" {
    fn moshell_value_get_as_byte(val: VmValueFFI) -> u8;
    fn moshell_value_get_as_int(val: VmValueFFI) -> i64;
    fn moshell_value_get_as_double(val: VmValueFFI) -> f64;
    fn moshell_value_get_as_object(val: VmValueFFI) -> VmObjectFFI;
    fn moshell_object_unbox(val: VmObjectFFI) -> VmValueFFI;
    fn moshell_object_get_as_string(val: VmObjectFFI) -> *const ffi::c_char;
    fn moshell_object_get_as_array(val: VmObjectFFI) -> VmArrayFFI;
    fn moshell_object_get_as_struct(val: VmObjectFFI) -> VmStructureFFI;

    fn moshell_exec(bytes: *const u8, byte_count: usize) -> ffi::c_int;

    fn moshell_vm_init(
        args: *const *const ffi::c_char,
        arg_count: usize,
        lens: *const usize,
    ) -> VmFFI;

    fn moshell_vm_register(vm: VmFFI, bytes: *const u8, bytes_count: usize) -> ffi::c_int;

    fn moshell_vm_run(vm: VmFFI) -> ffi::c_int;

    fn moshell_set_pgid(vm: VmFFI, pgid: ffi::c_int);

    fn moshell_vm_next_page(vm: VmFFI) -> usize;

    fn moshell_vm_free(vm: VmFFI);

    fn moshell_vm_get_exported(vm: VmFFI, name: *const ffi::c_char, name_len: usize) -> VmValueFFI;

    fn moshell_vm_gc_collect(vm: VmFFI) -> VmGcResultFFI;
    fn moshell_vm_gc_run(vm: VmFFI);
    fn gc_collection_result_free(res: VmGcResultFFI);
}
