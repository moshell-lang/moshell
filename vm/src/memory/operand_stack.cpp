//
// Created by maxime on 6/14/23.
//

#include "operand_stack.h"

OperandStack::OperandStack(size_t len)
    : bytes{new char[len]},
      capacity{len},
      current_pos{0} {}

void OperandStack::push_int(int64_t i) {
    if (current_pos + sizeof(int64_t) >= capacity) {
        throw OperandStackError("exceeded operand stack capacity");
    }
    *(int64_t *)(bytes + current_pos) = i;
    current_pos += sizeof(int64_t);
}

void OperandStack::push_double(double d) {
    if (current_pos + sizeof(double) >= capacity) {
        throw OperandStackError("exceeded operand stack capacity");
    }
    *(double *)(bytes + current_pos) = d;
    current_pos += sizeof(double);
}

void OperandStack::push_string_constant_ref(int64_t s) {
    if (current_pos + sizeof(int64_t) >= capacity) {
        throw OperandStackError("exceeded operand stack capacity");
    }
    *(int64_t *)(bytes + current_pos) = s;
    current_pos += sizeof(int64_t);
}

int64_t OperandStack::pop_int() {
    pop_bytes(sizeof(int64_t));
    return *(int64_t *)(bytes + current_pos);
}

int64_t OperandStack::pop_string_constant_ref() {
    pop_bytes(sizeof(int64_t));
    return *(int64_t *)(bytes + current_pos);
}

double OperandStack::pop_double() {
    pop_bytes(sizeof(double));
    return *(double *)(bytes + current_pos);
}

inline void OperandStack::pop_bytes(size_t size) {
    if (current_pos < size) {
        throw OperandStackError("operand stack is empty");
    }
    current_pos -= size;
}

OperandStack::~OperandStack() {
    delete[] bytes;
}
