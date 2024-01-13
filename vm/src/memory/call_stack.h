#pragma once

#include <cstddef>
#include <vector>

#include "definitions/function_definition.h"
#include "memory/locals.h"
#include "memory/operand_stack.h"

/**
 * The information about a stack frame.
 * Each function invocation implies a stack frame, that contains the functions' locals and operands stack
 */
struct stack_frame {
    const function_definition &function;
    size_t instruction_pointer;
    OperandStack operands;
    Locals locals;
};

/**
 * A thread callstack, with fixed capacity
 */
class CallStack {
    std::vector<stack_frame> blocks;
    std::vector<std::byte> tape;
    std::vector<bool> operands_refs_offsets;
    friend msh::gc;

public:
    /**
     * creates an empty call stack
     */
    explicit CallStack(size_t capacity);

    /**
     * Pushes a new frame inside this call stack.
     * @param callee the function definition of the new frame to create and push
     */
    void push_frame(const function_definition &callee);

    /**
     * pops last frame from the call_stack.
     * this action does not writes in the popped frame
     */
    void pop_frame();

    /**
     * peeks last frame, returning a `stack_frame` structure representing the frame
     * note that the structure holds references into the stack, thus the caller must ensure that
     * the returned stack_frame is manipulated only if the frame is still present in this call stack (not popped)
     */
    stack_frame &peek_frame();

    /**
     * @return true if the size is equal to 0
     */
    bool is_empty() const;

    /**
     * clears the call stack
     */
    void clear();

    std::vector<stack_frame>::iterator begin();
    std::vector<stack_frame>::iterator end();
};
