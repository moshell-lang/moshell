#pragma once

#include "errors.h"
#include <cstddef>
#include <cstdint>
#include <exception>
#include <memory>
#include <stdexcept>

struct OperandStackOutOfBoundError : public MemoryError {

public:
    explicit OperandStackOutOfBoundError(std::string message) : MemoryError{message} {}
};

class OperandStack {
private:
    std::unique_ptr<char[]> bytes;
    size_t capacity;
    size_t current_pos;

public:
    explicit OperandStack(size_t len);

    void push_int(int64_t i);

    void push_byte(char b);

    void push_double(double d);

    void push_reference(uintptr_t r);

    int64_t pop_int();

    char pop_byte();

    double pop_double();

    uintptr_t pop_reference();

    void pop_bytes(size_t size);

private:
    template <typename T>
    void push(T t);

    template <typename T>
    T pop();
};
