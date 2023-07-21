#pragma once

#include <algorithm>
#include <cstdint>
#include <errors.h>
#include <string>

/**
 * an access to an out of bound index has been made from the `Locals` area
 */
class LocalsOutOfBoundError : public MemoryError {
public:
    explicit LocalsOutOfBoundError(std::string msg) : MemoryError(std::move(msg)) {}
    const char *name() const noexcept override {
        return "LocalsOutOfBoundError";
    }
};

/**
 * Encapsulates the allocated locals area of a stack frame
 */
class Locals {
    /**
     * encapsulated bytes in the stack frame
     */
    char *const bytes;

    /**
     * number of bytes reserved
     */
    const size_t capacity;

public:
    explicit Locals(char *bytes, size_t capacity);

    /**
     * returns a reference to given byte
     * @throws LocalsOutOfBoundError if `at` is out of bound
     * */
    char &reference(size_t at);
};
