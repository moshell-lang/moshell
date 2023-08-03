#pragma once
#include <cstddef>

#if UINTPTR_MAX > 0xFFFFFFFFFFFFFFFFu // 64 bits
#error "VM only supports architectures less than 64 bits architectures"
#endif

/**
 * The exit code of a Moshell program that did panic
 */
#define MOSHELL_PANIC 255

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
 */
extern "C" int moshell_exec(const char *bytes, size_t byte_count);
