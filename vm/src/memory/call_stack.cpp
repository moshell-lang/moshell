#include "call_stack.h"

/**
 * Contains all the runtime information about a frame
 */
struct frame_headers {
    /**
     * position of the previous frame header.
     * 0 if this frame is the root headers
     */
    size_t previous_frame_headers_pos;
    /**
     * Address of this frame's function identifier
     */
    const std::string *function_identifier;

    /**
     * Current position of the instruction being executed
     */
    size_t instruction_pointer;

    /**
     * The position of the frame's operands stack
     */
    size_t operands_pos;
    /**
     * The amount of bytes allocated for the locals area of this frame
     */
    size_t locals_capacity;
};

CallStack::CallStack(size_t capacity)
    : block{std::make_unique<char[]>(capacity)},
      frame_headers_pos{0},
      capacity{capacity},
      frame_count{0} {}

inline void check_overflow(size_t capacity, size_t current_pos, const function_definition &callee) {
    // as the operand stack stack_capacity is the end of the call stack, we do not include it in this check
    size_t total_frame_size = callee.locals_size + sizeof(frame_headers);
    if (current_pos + total_frame_size >= capacity) {
        throw StackOverflowError("Call stack exceeded stack_capacity");
    }
}

CallStack CallStack::create(size_t capacity, const function_definition &root, const std::string *root_identifier) {
    CallStack stack(capacity);
    stack.push_frame(root, root_identifier);
    return stack;
}

void CallStack::push_frame(const function_definition &callee, const std::string *callee_identifier) {
    char *block = this->block.get();

    size_t pos = 0;

    // if the stack is empty, we start at position 0
    if (!is_empty()) {
        // if the stack is not empty, go after the current frame's operands position
        frame_headers *previous_frame = (frame_headers *)(block + frame_headers_pos);
        // go after frame's headers, and add its operands length
        pos = frame_headers_pos + sizeof(frame_headers) + previous_frame->operands_pos;
    }

    // check potential stack overflow
    check_overflow(capacity, pos, callee);

    // reserve locals
    pos += callee.locals_size;

    // write default headers,
    *(frame_headers *)(block + pos) = {
        frame_headers_pos,
        callee_identifier,
        0,
        0,
        callee.locals_size,
    };
    frame_headers_pos = pos;
    frame_count++;
}

void CallStack::pop_frame() {
    if (is_empty()) {
        throw MemoryError("Could not pop call stack: stack is already empty.");
    }
    size_t pos = frame_headers_pos; // go to headers position, this also skips operands according to frame layout

    // retrieve headers
    const frame_headers *headers = (frame_headers *)(block.get() + pos);

    // place current frame_headers position to the previous frame
    frame_headers_pos = headers->previous_frame_headers_pos;

    frame_count--;
}

stack_frame CallStack::peek_frame() const {
    char *block = this->block.get();

    frame_headers *headers = (frame_headers *)(block + frame_headers_pos);

    // first byte position of operands
    size_t operands_first_byte = frame_headers_pos + sizeof(frame_headers);
    OperandStack frame_operands(block + operands_first_byte, headers->operands_pos, this->capacity - operands_first_byte);

    Locals frame_locals(block + (frame_headers_pos - headers->locals_capacity), headers->locals_capacity);
    return stack_frame{headers->function_identifier, &headers->instruction_pointer, std::move(frame_operands), std::move(frame_locals)};
}

size_t CallStack::get_capacity() const {
    return capacity;
}

size_t CallStack::size() const {
    return frame_count;
}

bool CallStack::is_empty() const {
    return size() == 0;
}
