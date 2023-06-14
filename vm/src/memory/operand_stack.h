//
// Created by maxime on 6/14/23.
//

#ifndef VM_OPERAND_STACK_H
#define VM_OPERAND_STACK_H

#include <exception>
#include <cstdint>
#include <cstddef>
#include <memory>

class OperandStack {
private:
    char* bytes;
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

    ~OperandStack();
};

struct OperandStackError : public std::exception {

private:
    const char* message;

public:
    explicit OperandStackError(const char* message) : message{message} {}

    [[nodiscard]]
    const char* what() const noexcept override {
        return message;
    }
};


#endif //VM_OPERAND_STACK_H
