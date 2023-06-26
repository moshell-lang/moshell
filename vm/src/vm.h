#pragma once
#include <cstddef>

/**
 * The exit code of a Moshell program that did not able to spawn a child process.
 */
#define MOSHELL_COMMAND_NOT_RUNNABLE 127

/**
 * Executes the given Moshell bytecode.
 *
 * The bytecode will be immediately executed.
 * This function will not return until the interpreter halts.
 * Due to the C++ interop, this function might throw a C++ exception
 * if the given bytecode is invalid, which will be caught and printed to
 * stderr. The error itself cannot be obtained in any way by the caller
 * of this function.
 *
 * @param bytes The bytecode to execute.
 * @param byte_count The number of bytes in the bytecode.
 */
extern "C" void moshell_exec(const char *bytes, size_t byte_count);
