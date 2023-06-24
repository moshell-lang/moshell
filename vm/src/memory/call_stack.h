#pragma once

#include "definitions/function_definition.h"
#include "interpreter.h"
#include "locals.h"
#include "operand_stack.h"
#include <cstddef>
#include <tuple>

class StackOverflowError : public MemoryError {

public:
    explicit StackOverflowError(const char *message) : MemoryError{message} {}
};

struct stack_frame {
    constant_index function_signature_idx;
    size_t *instruction_pointer;
    OperandStack operands;
    Locals locals;
};

/// A thread callstack
class CallStack {
    std::unique_ptr<char[]> block;

    size_t frame_headers_pos;
    size_t capacity;
    size_t pos;

    /// creates an empty call stack
    explicit CallStack(size_t capacity);
public:

    size_t size() const;

    static CallStack create(size_t capacity, const function_definition& root, constant_index root_ref);

    void push_frame(const function_definition &callee, constant_index callee_ref);

    void pop_frame();

    stack_frame peek_frame() const;

    bool is_empty() const;
};
