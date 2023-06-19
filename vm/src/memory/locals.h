#pragma once

#include "errors.h"
#include <cstddef>
#include <cstdint>
#include <string>

class LocalsOutOfBoundError : public MemoryError {
public:
    explicit LocalsOutOfBoundError(const char *msg) : MemoryError(msg) {}
};

class Locals {
    char *bytes;
    size_t capacity;

public:
    explicit Locals(char *bytes, size_t capacity);

    [[nodiscard]] int64_t get_int64(size_t at) const;
    [[nodiscard]] double get_double(size_t at) const;
    [[nodiscard]] char get_byte(size_t at) const;
    [[nodiscard]] size_t get_ref(size_t at) const;

    void set_int64(int64_t i, size_t at);
    void set_double(double d, size_t at);
    void set_byte(char b, size_t at);
    void set_ref(size_t r, size_t at);

private:
    template <typename T>
    T get(size_t at) const;

    template <typename T>
    void set(T t, size_t at);
};
