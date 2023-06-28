#pragma once

#include "constant_pool.h"
#include "errors.h"
#include <cstddef>
#include <cstdint>
#include <exception>
#include <memory>
#include <stdexcept>

struct OperandStackUnderflowError : public MemoryError {

public:
    explicit OperandStackUnderflowError(std::string message) : MemoryError{std::move(message)} {}
};

class OperandStack {
private:
    char *bytes;
    size_t &current_pos;
    const size_t stack_capacity;

public:
    explicit OperandStack(char *buff, size_t &initial_pos, size_t stack_capacity);

    size_t size() const;

    size_t get_capacity() const;

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
