use context::source::ContentId;
use std::ffi;

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

/// A virtual machine that can execute Moshell bytecode.
pub struct VM(VmFFI);

/// An error that occurred during the VM lifetime.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VmError {
    Panic,
    Internal,
}

impl VM {
    /// Creates a new virtual machine.
    pub fn new() -> Self {
        Self(unsafe { moshell_vm_init() })
    }

    /// Appends new bytecode to the VM.
    ///
    /// # Safety
    /// An invalid bytecode will almost certainly result in a deterministic error during loading,
    /// or a non-deterministic error during execution.
    pub fn register(&mut self, bytes: &[u8]) -> Result<(), VmError> {
        unsafe { moshell_vm_register(self.0, bytes.as_ptr(), bytes.len()) != -1 }
            .then_some(())
            .ok_or(VmError::Internal)
    }

    /// Executes the remaining bytecode.
    ///
    /// # Safety
    /// The caller must ensure that the previously registered bytecode is valid.
    pub unsafe fn run(&mut self) -> Result<(), VmError> {
        match moshell_vm_run(self.0) {
            0 => Ok(()),
            1 => Err(VmError::Panic),
            _ => Err(VmError::Internal),
        }
    }

    /// Gets the next page of bytecode to be executed.
    pub fn get_next_page(&self) -> ContentId {
        ContentId(unsafe { moshell_vm_next_page(self.0) })
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        unsafe { moshell_vm_free(self.0) }
    }
}

#[repr(C)]
#[derive(Copy, Clone)]
struct VmFFI(*mut ffi::c_void);

#[link(name = "vm", kind = "static")]
extern "C" {
    fn moshell_exec(bytes: *const u8, byte_count: usize) -> ffi::c_int;

    fn moshell_vm_init() -> VmFFI;

    fn moshell_vm_register(vm: VmFFI, bytes: *const u8, bytes_count: usize) -> ffi::c_int;

    fn moshell_vm_run(vm: VmFFI) -> ffi::c_int;

    fn moshell_vm_next_page(vm: VmFFI) -> usize;

    fn moshell_vm_free(vm: VmFFI);
}
