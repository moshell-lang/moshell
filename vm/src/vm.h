#pragma once
#include <cstdint>
#include <stddef.h> // NOLINT(*-deprecated-headers)

#if UINTPTR_MAX > 0xFFFFFFFFFFFFFFFFu // 64 bits
#error "VM only supports architectures less than 64 bits architectures"
#endif

/**
 * The exit code of a Moshell program that did panic
 */
#define MOSHELL_PANIC 255

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    const void *val;
} moshell_value;

typedef struct {
    uint64_t size;
    const moshell_value *val;
} moshell_array;

uint8_t moshell_value_get_as_byte(moshell_value val);
int64_t moshell_value_get_as_int(moshell_value val);
double moshell_value_get_as_double(moshell_value val);
const char *moshell_value_get_as_string(moshell_value val);
moshell_array moshell_value_get_as_array(moshell_value val);

/**
 * An opaque handle to a Moshell VM.
 */
typedef struct {
    void *vm;
} moshell_vm;

/**
 * Executes the given Moshell bytecode.
 *
 * The bytecode will be immediately executed without any sanity checks.
 * This function will not return until the interpreter halts.
 * Due to the C++ interop, this function might throw a C++ exception
 * if the given bytecode is invalid, which will be caught and printed to
 * stderr. The error itself cannot be obtained in any way by the caller
 * of this function.
 *
 * @param bytes The bytecode to execute.
 * @param byte_count The number of bytes in the bytecode.
 * @return an exitcode where:
 *    - 0, the vm exited successfully
 *    - 1, the vm aborted due to a panic
 * @deprecated Use `moshell_vm_init`, `moshell_vm_register` and `moshell_vm_run_all` instead.
 */
int moshell_exec(const char *bytes, size_t byte_count);

/**
 * Creates a new virtual machine.
 *
 * @return A new Moshell VM.
 */
moshell_vm moshell_vm_init();

/**
 * Appends the given bytecode to the VM.
 *
 * @param vm The VM to append the bytecode to.
 * @param bytes The bytecode to append.
 * @param byte_count The number of bytes in the `bytes` array.
 * @return 0 if the registration was successful, -1 otherwise.
 */
int moshell_vm_register(moshell_vm vm, const char *bytes, size_t byte_count);

/**
 * Executes the remaining bytecode pages in the VM.
 *
 * @param vm The VM to execute.
 * @return 0 if the execution was successful, -1 otherwise.
 */
int moshell_vm_run(moshell_vm vm);

moshell_value moshell_vm_get_exported(moshell_vm vm, char *name);

/**
 * Returns the next page identifier to be executed.
 *
 * @param vm The VM to query.
 * @return The next page identifier to be executed.
 */
size_t moshell_vm_next_page(moshell_vm vm);

/**
 * Frees the given VM.
 *
 * @param vm The VM to free.
 */
void moshell_vm_free(moshell_vm vm);

#ifdef __cplusplus
}
#endif
