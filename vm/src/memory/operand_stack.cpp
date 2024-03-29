#include "operand_stack.h"
#include <cstring>

OperandStack::OperandStack(std::byte *bytes, size_t current_pos, size_t stack_capacity, std::vector<bool> &operands_refs)
    : bytes{bytes},
      current_pos{current_pos},
      stack_capacity{stack_capacity},
      operands_refs{operands_refs} {}

size_t OperandStack::size() const {
    return current_pos;
}

size_t OperandStack::get_capacity() const {
    return stack_capacity;
}

void OperandStack::push_int(int64_t i) {
    push(i);
}

void OperandStack::push_reference(msh::obj &r) {
    push(&r);
}

void OperandStack::push_unchecked_reference(void *r) {
    push((uint64_t)r);
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

msh::obj &OperandStack::pop_reference() {
    return *pop<msh::obj *>();
}

double OperandStack::pop_double() {
    return pop<double>();
}

const std::byte *OperandStack::pop_bytes(size_t n) {
    if (current_pos < n) {
        throw OperandStackUnderflowError("operand stack is empty");
    }
    current_pos -= n;
    return bytes + current_pos;
}

void OperandStack::transfer(OperandStack &callee_stack, size_t n) {
#ifndef NDEBUG
    if (n > callee_stack.size())
        throw std::out_of_range("cannot transfer more bytes than contained in the source operand stack");
#endif
    memcpy(this->bytes + current_pos, callee_stack.bytes + (callee_stack.size() - n), n);
    std::copy(callee_stack.operands_refs.begin() + (callee_stack.size() - n), callee_stack.operands_refs.begin() + callee_stack.size(), operands_refs.begin() + current_pos);
    this->current_pos += n;
}

void OperandStack::memmove(size_t dest, size_t src, size_t size) {
    if (dest + size > stack_capacity) {
        throw StackOverflowError("exceeded stack capacity via operand stack");
    }
    memcpy(bytes + dest, bytes + src, size);
    operands_refs[dest] = operands_refs[src];
    current_pos = dest + size;
}

void OperandStack::dup_qword() {
    if (current_pos + sizeof(int64_t) > stack_capacity) {
        throw StackOverflowError("exceeded stack capacity via operand stack");
    }
    memcpy(this->bytes + current_pos, this->bytes + current_pos - sizeof(int64_t), sizeof(int64_t));
    std::copy(operands_refs.begin() + current_pos - sizeof(int64_t), operands_refs.begin() + current_pos, operands_refs.begin() + current_pos);
    current_pos += sizeof(int64_t);
}

void OperandStack::swap_upper_qwords() {
    if (current_pos < sizeof(int64_t) * 2) {
        throw OperandStackUnderflowError("operand stack is empty");
    }
    std::swap(*(int64_t *)(bytes + current_pos - sizeof(int64_t)), *(int64_t *)(bytes + current_pos - sizeof(int64_t) * 2));
    std::vector<bool>::swap(operands_refs[current_pos - sizeof(int64_t)], operands_refs[current_pos - sizeof(int64_t) * 2]);
}

void OperandStack::swap_upper_three_qwords() {
    if (current_pos < sizeof(int64_t) * 3) {
        throw OperandStackUnderflowError("operand stack is empty");
    }
    std::swap(*(int64_t *)(bytes + current_pos - sizeof(int64_t)), *(int64_t *)(bytes + current_pos - sizeof(int64_t) * 3));
    std::swap(*(int64_t *)(bytes + current_pos - sizeof(int64_t) * 2), *(int64_t *)(bytes + current_pos - sizeof(int64_t) * 3));
    std::vector<bool>::swap(operands_refs[current_pos - sizeof(int64_t)], operands_refs[current_pos - sizeof(int64_t) * 3]);
    std::vector<bool>::swap(operands_refs[current_pos - sizeof(int64_t) * 2], operands_refs[current_pos - sizeof(int64_t) * 3]);
}
