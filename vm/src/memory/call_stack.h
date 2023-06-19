#pragma once

#include "locals.h"
#include "operand_stack.h"
#include <cstddef>
#include <tuple>

/// A thread callstack
class CallStack {
    char* block;
    size_t capacity;

    size_t current_frame_start;
    size_t current_frame_end;
    explicit CallStack(size_t capacity);

public:
    static std::tuple<CallStack, OperandStack, Locals> create(size_t capacity, size_t root_frame_operand_size, size_t root_frame_locals_size);


    std::tuple<OperandStack, Locals> push_frame(size_t operand_size, size_t locals_size);

    char* pop_frame();
};
