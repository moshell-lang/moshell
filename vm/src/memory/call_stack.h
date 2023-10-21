#pragma once

#include "definitions/function_definition.h"
#include "memory/locals.h"
#include "memory/operand_stack.h"

/**
 * The information about a stack frame.
 * Each function invocation implies a stack frame, that contains the functions' locals and operands stack
 */
struct stack_frame {
    const function_definition &function;
    size_t *instruction_pointer;
    OperandStack operands;
    Locals locals;
};

class call_stack_iterator;

/**
 * A thread callstack, with fixed capacity
 */
class CallStack {
    std::unique_ptr<char[]> block;
    std::vector<bool> operands_refs_offsets;

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

    friend msh::gc;

public:
    /**
     * creates an empty call stack
     */
    explicit CallStack(size_t capacity);

    friend call_stack_iterator;

public:
    /**
     * Creates a new CallStack, with root function set on top of the stack
     * @throws StackOverflowError if the root's frame size exceeds given capacity
     */
    static CallStack create(size_t capacity, const function_definition &root);

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
    stack_frame peek_frame();

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

    /**
     * clears the call stack
     */
    void clear();

    call_stack_iterator begin();
    call_stack_iterator end();
};

// concurrent modification of call_stack is UB
class call_stack_iterator {
    CallStack *call_stack;
    size_t pos;
    size_t frame_ord;

public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = const stack_frame;
    using difference_type = size_t;
    using pointer = const stack_frame *;
    using reference = const stack_frame &;

    call_stack_iterator(CallStack *call_stack, size_t pos, size_t frame_ord);
    call_stack_iterator &operator++();
    bool operator==(const call_stack_iterator &other) const;
    bool operator!=(const call_stack_iterator &other) const;
    stack_frame operator*();
};
