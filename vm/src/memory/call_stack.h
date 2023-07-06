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
    const std::string *function_identifier;
    size_t *instruction_pointer;
    OperandStack operands;
    Locals locals;
};

/// A thread callstack, with fixed capacity
class CallStack {
    std::unique_ptr<char[]> block;

    /**
     * position of the current frame headers
     * is 0 if this call stack is empty
     */
    size_t frame_headers_pos;
    /**
     * Total amount of bytes allocated for this stack.
     */
    size_t capacity;
    /**
     * amount of frames contained in the call stack
     */
    size_t frame_count;

    /// creates an empty call stack
    explicit CallStack(size_t capacity);

public:
    /**
     * Creates a new CallStack, with root function set on top of the stack
     * @throws StackOverflowError if the root's frame size exceeds given capacity
     */
    static CallStack create(size_t capacity, const function_definition &root, const std::string *root_identifier);

    /**
     * Pushes a new frame inside this call stack.
     * @callee the function definition of the new frame to create and push
     * @callee_ref the ref in bound constant pool of the callee's function function_identifier
     */
    void push_frame(const function_definition &callee, const std::string *callee_identifier);

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
    stack_frame peek_frame() const;

    /**
     * @return the capacity in bytes of this call stack
     */
    size_t get_capacity() const;

    /**
     * @return the number of frames present in this calls tack
     */
    size_t size() const;

    /**
     * @return true if the size is equal to 0
     */
    bool is_empty() const;
};
