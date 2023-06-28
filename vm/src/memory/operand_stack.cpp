#include "operand_stack.h"

OperandStack::OperandStack(size_t len)
    : bytes{std::make_unique<char[]>(len)},
      capacity{len},
      current_pos{0} {}

void OperandStack::push_int(int64_t i) {
    push(i);
}

void OperandStack::push_reference(uintptr_t r) {
    push(r);
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

uintptr_t OperandStack::pop_reference() {
    return pop<uintptr_t>();
}

double OperandStack::pop_double() {
    return pop<double>();
}

template <typename T>
void OperandStack::push(T t) {
    if (current_pos + sizeof(T) >= capacity) {
        throw OperandStackOutOfBoundError("exceeded operand stack capacity");
    }
    *(T *)(bytes.get() + current_pos) = t;
    current_pos += sizeof(T);
}

template <typename T>
T OperandStack::pop() {
    if (current_pos < sizeof(T)) {
        throw OperandStackOutOfBoundError("operand stack is empty");
    }
    current_pos -= sizeof(T);
    return *(T *)(bytes.get() + current_pos);
}

void OperandStack::pop_bytes(size_t size) {
    if (current_pos < size) {
        throw OperandStackOutOfBoundError("operand stack is empty");
    }
    current_pos -= size;
}
