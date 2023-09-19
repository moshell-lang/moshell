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

typedef union {
    int64_t i;
    uint8_t b;
    double d;
    const void *ptr;
} moshell_value;

typedef enum {
    OBJ_STR,
    OBJ_INT,
    OBJ_DOUBLE,
    OBJ_VEC
} moshell_object_type;

typedef struct {
    moshell_object_type type;
    const void *val;
} moshell_object;

typedef struct {
    const uint64_t size;
    const moshell_value *data;
} moshell_array;

uint8_t moshell_value_get_as_byte(moshell_value val);
int64_t moshell_value_get_as_int(moshell_value val);
double moshell_value_get_as_double(moshell_value val);
moshell_object moshell_value_get_as_object(moshell_value val);

moshell_value moshell_object_unbox(moshell_object obj);
const char *moshell_object_get_as_string(moshell_object obj);
moshell_array moshell_object_get_as_array(moshell_object obj);

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
 *    - -1, tje vm aborted due to an internal error
 * @deprecated Use `moshell_vm_init`, `moshell_vm_register` and `moshell_vm_run` instead.
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
 * VM runtime does not supports threading and this function must not be called concurrently
 *
 * @param vm The VM to execute.
 * @return 0 if the execution was successful, -1 otherwise.
 */
int moshell_vm_run(moshell_vm vm);

/**
 * Return an exported value from its name identifier
 * */
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

/**
 * Run a new Garbage Collection cycle
 * to remove detached objects from heap.
 * */
void moshell_vm_gc_run(moshell_vm vm);

typedef struct {
    const uint64_t collected_objects_count;
    const moshell_object *collected_objects;
} gc_collection_result;

/**
 * For debug and test purposes, runs a new gc cycle
 * that will not free any object but will collect them and return
 * them under a gc_collection_result structure.
 *
 * Be aware that all the contained objects will be invalidated at the next gc run
 * */
gc_collection_result moshell_vm_gc_collect(moshell_vm vm);

/**
 * free collection result
 * */
void gc_collection_result_free(gc_collection_result res);

#ifdef __cplusplus
}
#endif
