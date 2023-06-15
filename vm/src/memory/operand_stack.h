#pragma once

#include <cstddef>
#include <cstdint>
#include <exception>
#include <memory>

class OperandStack {
private:
    char *bytes;
    size_t capacity;
    size_t current_pos;

public:
    explicit OperandStack(size_t len);

    void push_int(int64_t i);

    void push_double(double d);

    void push_string_constant_ref(int64_t s);

    int64_t pop_int();

    double pop_double();

    int64_t pop_string_constant_ref();

    void pop_bytes(size_t size);

    ~OperandStack();
};

struct OperandStackError : public std::exception {

private:
    const char *message;

public:
    explicit OperandStackError(const char *message) : message{message} {}

    [[nodiscard]] const char *what() const noexcept override {
        return message;
    }
};
