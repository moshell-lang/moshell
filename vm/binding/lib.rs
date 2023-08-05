use std::ffi;

/// Executes the given bytecode.
///
/// The execution will block the thread until the Moshell program terminates.
///
/// # Safety
/// If the given bytecode is invalid, this function can cause undefined behavior.
/// The caller must ensure that the given bytecode is valid.
pub unsafe fn execute_bytecode(bytes: &[u8]) -> i32 {
    moshell_exec(bytes.as_ptr(), bytes.len())
}

#[link(name = "vm", kind = "static")]
extern "C" {
    fn moshell_exec(bytes: *const u8, byte_count: usize) -> ffi::c_int;
}
