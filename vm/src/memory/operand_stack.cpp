#include "operand_stack.h"

OperandStack::OperandStack(char* buff, size_t &position, size_t capacity)
    : bytes{buff},
      current_pos{position},
      capacity{capacity} {}

size_t OperandStack::size() const {
    return current_pos;
}

size_t OperandStack::get_capacity() const {
    return capacity;
}

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

template <typename T>
void OperandStack::push(T t) {
    if (current_pos + sizeof(T) > capacity) {
        throw OperandStackOutOfBoundError("exceeded operand stack capacity");
    }
    *(T *)(bytes + current_pos) = t;
    current_pos += sizeof(T);
}

template <typename T>
T OperandStack::pop() {
    if (current_pos < sizeof(T)) {
        throw OperandStackOutOfBoundError("operand stack is empty");
    }
    current_pos -= sizeof(T);
    return *(T *)(bytes + current_pos);
}

void OperandStack::pop_bytes(size_t size) {
    if (current_pos < size) {
        throw OperandStackOutOfBoundError("operand stack is empty");
    }
    current_pos -= size;
}
