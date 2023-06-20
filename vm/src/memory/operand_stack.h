#pragma once

#include <cstddef>
#include <cstdint>
#include <exception>
#include <memory>
#include <stdexcept>

struct OperandStackOutOfBoundError : public std::out_of_range {

public:
    explicit OperandStackOutOfBoundError(const char *message) : std::out_of_range{message} {}
};

class OperandStack {
private:
    char *bytes;
    size_t &current_pos;
    const size_t capacity;

public:
    explicit OperandStack(char *buff, size_t &initial_pos, size_t capacity);

    size_t size() const;
    size_t get_capacity() const;

    void push_int(int64_t i);

    void push_byte(char b);

    void push_double(double d);

    void push_string_constant_ref(int64_t s);

    int64_t pop_int();

    char pop_byte();

    double pop_double();

    int64_t pop_string_constant_ref();

    void pop_bytes(size_t size);

private:
    template <typename T>
    void push(T t);

    template <typename T>
    T pop();
};
