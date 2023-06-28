#pragma once

#include "definitions/function_definition.h"
#include "locals.h"
#include "operand_stack.h"

/// Thrown when the memory allocated for a thread call stack
/// is exceeded
class StackOverflowError : public MemoryError {
public:
    explicit StackOverflowError(std::string message) : MemoryError{message} {}
};

/// The information about a stack frame.
/// Each function invocation implies a stack frame, that contains the functions' locals and operands stack
struct stack_frame {
    constant_index function_signature_idx;
    size_t *instruction_pointer;
    OperandStack operands;
    Locals locals;
};

/// A thread callstack, with fixed capacity
class CallStack {
    std::unique_ptr<char[]> block;

    size_t frame_headers_pos;
    size_t capacity;
    size_t pos;

    /// creates an empty call stack
    explicit CallStack(size_t capacity);

public:
    /**
     * Creates a new CallStack, with root function set on top of the stack
     * @throws StackOverflowError if the root's frame size exceeds given capacity
     * */
    static CallStack create(size_t capacity, const function_definition &root, constant_index root_ref);

    /**
     * Pushes a new frame inside this call stack.
     * @callee the function definition of the new frame to create and push
     * @callee_ref the ref in bound constant pool of the callee's function signature
     * */
    void push_frame(const function_definition &callee, constant_index callee_signature);

    /**
     * pops last frame from the call_stack.
     * */
    void pop_frame();

    /**
     * peeks last frame, returning a `stack_frame` structure representing the frame
     * note that the structure holds references into the stack, thus the caller must ensure that
     * the returned stack_frame is manipulated only if the frame is still present in this call stack (not popped)
     * */
    stack_frame peek_frame() const;

    /**
     * @return the capacity in bytes of this call stack
     * */
    size_t get_capacity() const;

    /**
     * @return currently occupied bytes of this call stack
     * */
    size_t size() const;

    /**
     * @return true if the size is equal to 0
     * */
    inline bool is_empty() const;
};
