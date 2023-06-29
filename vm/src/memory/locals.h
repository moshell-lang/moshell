#pragma once

#include <algorithm>
#include <cstdint>
#include <errors.h>
#include <string>

/// The size of a local
class LocalsOutOfBoundError : public MemoryError {
public:
    explicit LocalsOutOfBoundError(std::string msg) : MemoryError(msg) {}
};

/**
 * Encapsulates the allocated locals area of a stack frame
 */
class Locals {
    /// encapsulated bytes in the stack frame
    const char *bytes;

    /// number of bytes reserved
    const size_t capacity;

public:
    explicit Locals(char *bytes, size_t capacity);

    /**
     * @throws LocalsOutOfBoundError if `at` is out of bound
     */
    int64_t get_q_word(size_t at) const;
    /**
     * @throws LocalsOutOfBoundError if `at` is out of bound
     */
    char get_byte(size_t at) const;
    /**
     * @throws LocalsOutOfBoundError if `at` is out of bound
     */
    uintptr_t get_ref(size_t at) const;

    /**
     * @throws LocalsOutOfBoundError if `at` is out of bound
     */
    void set_q_word(int64_t i, size_t at);
    /**
     * @throws LocalsOutOfBoundError if `at` is out of bound
     */
    void set_byte(char b, size_t at);
    /**
     * @throws LocalsOutOfBoundError if `at` is out of bound
     */
    void set_ref(uintptr_t s, size_t at);

private:
    template <typename T>
    T get(size_t at) const;

    template <typename T>
    void set(T t, size_t at);
};
