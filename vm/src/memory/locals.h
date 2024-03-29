#pragma once

#include <cstdint>
#include <string_view>

#include "errors.h"

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
    std::byte *bytes;

    /**
     * number of bytes reserved
     */
    const size_t capacity;

public:
    Locals(std::byte *bytes, size_t capacity);

    /**
     * @returns a reference to given byte
     * @throws LocalsOutOfBoundError if `at` is out of bound
     */
    uint8_t &reference(size_t at);

    /**
     * @throws LocalsOutOfBoundError if `at` is out of bound
     */
    int64_t get_q_word(size_t at) const;
    /**
     * @throws LocalsOutOfBoundError if `at` is out of bound
     */
    uint8_t get_byte(size_t at) const;

    /**
     * @throws LocalsOutOfBoundError if `at` + the size of `int64_i` is out of bound
     */
    void set_q_word(int64_t i, size_t at);
    /**
     * @throws LocalsOutOfBoundError if `at` + the size of `char` is out of bound
     */
    void set_byte(uint8_t b, size_t at);

    template <typename T>
    T *get(size_t at) const {
        check_capacity(at, sizeof(T), "accessing");
        return (T *)(bytes + at);
    }

    template <typename T>
    void set(T t, size_t at) {
        check_capacity(at, sizeof(T), "updating");
        *(T *)(bytes + at) = t;
    }

private:
    /**
     * ensures that the given space can fit at given position in this reserved locals space.
     * @param at start position
     * @param space_size the space needed
     * @param action kind of action being made
     */
    inline void check_capacity(size_t at, size_t space_size, std::string_view action) const {
        if (at + space_size > capacity) {
            throw LocalsOutOfBoundError("locals out of bound when " + std::string(action) + " value at index " + std::to_string(at));
        }
    }
};
