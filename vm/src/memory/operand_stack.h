#pragma once

#include <cstddef>
#include <cstdint>
#include <exception>
#include <memory>

struct OperandStackError : public std::exception {

private:
    const char *message;

public:
    explicit OperandStackError(const char *message) : message{message} {}

    [[nodiscard]] const char *what() const noexcept override {
        return message;
    }
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

    void push_string_constant_ref(int64_t s);

    int64_t pop_int();

    char pop_byte();

    double pop_double();

    int64_t pop_string_constant_ref();

    void pop_bytes(size_t size);

private:
    template <typename T>
    void push(T t) {
        if (current_pos + sizeof(T) >= capacity) {
            throw OperandStackError("exceeded operand stack capacity");
        }
        *(T *)(bytes.get() + current_pos) = t;
        current_pos += sizeof(T);
    }

    template <typename T>
    T pop() {
        if (current_pos < sizeof(T)) {
            throw OperandStackError("operand stack is empty");
        }
        current_pos -= sizeof(T);
        return *(T *)(bytes.get() + current_pos);
    }
};
