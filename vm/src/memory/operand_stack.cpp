#include "operand_stack.h"
#include "call_stack.h"

OperandStack::OperandStack(const char *buff, size_t &position, size_t stack_capacity)
    : bytes{buff},
      current_pos{position},
      stack_capacity{stack_capacity} {}

size_t OperandStack::size() const {
    return current_pos;
}

size_t OperandStack::get_capacity() const {
    return stack_capacity;
}

void OperandStack::push_int(int64_t i) {
    push(i);
}

void OperandStack::push_reference(uint64_t r) {
    push(r);
}

void OperandStack::push_byte(int8_t b) {
    push(b);
}

void OperandStack::push_double(double d) {
    push(d);
}

int64_t OperandStack::pop_int() {
    return pop<int64_t>();
}

int8_t OperandStack::pop_byte() {
    return pop<int8_t>();
}

uint64_t OperandStack::pop_reference() {
    return pop<uint64_t>();
}

double OperandStack::pop_double() {
    return pop<double>();
}

const char *OperandStack::pop_bytes(size_t n) {
    if (current_pos < n) {
        throw OperandStackUnderflowError("operand stack is empty");
    }
    current_pos -= n;
    return bytes + current_pos;
}

void OperandStack::advance_unchecked(size_t size) {
    current_pos += size;
}

template <typename T>
void OperandStack::push(T t) {
    if (current_pos + sizeof(T) > stack_capacity) {
        throw StackOverflowError("exceeded stack stack_capacity via operand stack");
    }
    *(T *)(bytes + current_pos) = t;
    current_pos += sizeof(T);
}

template <typename T>
T OperandStack::pop() {
    pop_bytes(sizeof(T));
    return *(T *)(bytes + current_pos);
}
