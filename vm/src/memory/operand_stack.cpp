#include "operand_stack.h"

OperandStack::OperandStack(size_t len)
    : bytes{std::make_unique<char[]>(len)},
      capacity{len},
      current_pos{0} {}

void OperandStack::push_int(int64_t i) {
    push(i);
}

void OperandStack::push_string_constant_ref(int64_t s) {
    push(s);
}

void OperandStack::push_byte(char b) {
    push(b);
}

void OperandStack::push_double(double d) {
    push(d);
}

int64_t OperandStack::pop_int() {
    return pop<int64_t>();
}

char OperandStack::pop_byte() {
    return pop<char>();
}

int64_t OperandStack::pop_string_constant_ref() {
    return pop<int64_t>();
}

double OperandStack::pop_double() {
    return pop<double>();
}

void OperandStack::pop_bytes(size_t size) {
    if (current_pos < size) {
        throw OperandStackError("operand stack is empty");
    }
    current_pos -= size;
}
