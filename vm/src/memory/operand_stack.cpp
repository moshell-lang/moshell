#include "operand_stack.h"
#include <cstring>

OperandStack::OperandStack(char *buff, size_t &position, size_t stack_capacity, std::vector<bool> &operands_refs)
    : bytes{reinterpret_cast<std::byte *>(buff)},
      current_pos{position},
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
    return *(msh::obj *)(pop<uint64_t>());
}

double OperandStack::pop_double() {
    return pop<double>();
}

const std::byte *OperandStack::pop_bytes(size_t n) {
    if (current_pos < n) {
        throw OperandStackUnderflowError("operand stack is empty");
    }
    operands_refs.resize(operands_refs.size() - n);
    current_pos -= n;
    return bytes + current_pos;
}

void OperandStack::transfer(OperandStack &callee_stack, size_t n) {
#ifndef NDEBUG
    if (n > callee_stack.size())
        throw std::out_of_range("cannot transfer more bytes than contained in the source operand stack");
#endif
    memcpy(this->bytes + current_pos, callee_stack.bytes + (callee_stack.size() - n), n);
    this->current_pos += n;
    // Do not transfer the references here, as most of the stack instances share the same vector
}

void OperandStack::memmove(size_t dest, size_t src, size_t size) {
    if (dest + size > stack_capacity) {
        throw StackOverflowError("exceeded stack capacity via operand stack");
    }
    memcpy(bytes + dest, bytes + src, size);
    operands_refs.insert(operands_refs.begin() + dest, operands_refs.begin() + src, operands_refs.begin() + src + size);
    current_pos = dest + size;
}

void OperandStack::dup_qword() {
    if (current_pos + sizeof(int64_t) > stack_capacity) {
        throw StackOverflowError("exceeded stack capacity via operand stack");
    }
    memcpy(this->bytes + current_pos, this->bytes + current_pos - sizeof(int64_t), sizeof(int64_t));
    operands_refs.resize(operands_refs.size() + sizeof(int64_t));
    operands_refs[operands_refs.size() - sizeof(int64_t)] = operands_refs[operands_refs.size() - sizeof(int64_t) * 2];
    current_pos += sizeof(int64_t);
}

void OperandStack::swap_upper_qwords() {
    if (current_pos < sizeof(int64_t) * 2) {
        throw OperandStackUnderflowError("operand stack is empty");
    }
    std::swap(*(int64_t *)(bytes + current_pos - sizeof(int64_t)), *(int64_t *)(bytes + current_pos - sizeof(int64_t) * 2));
    std::vector<bool>::swap(operands_refs[operands_refs.size() - sizeof(int64_t)], operands_refs[operands_refs.size() - sizeof(int64_t) * 2]);
}

void OperandStack::swap_upper_three_qwords() {
    if (current_pos < sizeof(int64_t) * 3) {
        throw OperandStackUnderflowError("operand stack is empty");
    }
    std::swap(*(int64_t *)(bytes + current_pos - sizeof(int64_t)), *(int64_t *)(bytes + current_pos - sizeof(int64_t) * 3));
    std::swap(*(int64_t *)(bytes + current_pos - sizeof(int64_t) * 2), *(int64_t *)(bytes + current_pos - sizeof(int64_t) * 3));
    std::vector<bool>::swap(operands_refs[operands_refs.size() - sizeof(int64_t)], operands_refs[operands_refs.size() - sizeof(int64_t) * 3]);
    std::vector<bool>::swap(operands_refs[operands_refs.size() - sizeof(int64_t) * 2], operands_refs[operands_refs.size() - sizeof(int64_t) * 3]);
}
