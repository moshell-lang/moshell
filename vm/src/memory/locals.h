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
    uint64_t get_ref(size_t at) const;

    /**
     * @throws LocalsOutOfBoundError if `at` + the size of `int64_i` is out of bound
     */
    void set_q_word(int64_t i, size_t at);
    /**
     * @throws LocalsOutOfBoundError if `at` + the size of `char` is out of bound
     */
    void set_byte(char b, size_t at);
    /**
     * @throws LocalsOutOfBoundError if `at` + the size of `uint64_t` is out of bound
     */
    void set_ref(uint64_t s, size_t at);

    /**
     * copies the given data from `at` to `at + size`
     * @throws LocalsOutOfBoundError if `at` + size is out of bound
     */
    void set_bytes(const char *data, size_t size, size_t at);

private:
    /**
     * ensures that the given space can fit at given position in this reserved locals space.
     * @param at start position
     * @param space_size the space needed
     * @param action kind of action being made
     */
    inline void check_capacity(size_t at, size_t space_size, std::string_view action) const;

    template <typename T>
    T get(size_t at) const;

    template <typename T>
    void set(T t, size_t at);
};
