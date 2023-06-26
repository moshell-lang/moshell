#include "call_stack.h"

/// Contains all the additional information of a running frame
struct frame_headers {
    size_t previous_frame_headers_pos;
    constant_index signature_index;
    size_t instruction_pointer;

    size_t operands_pos;
    size_t locals_capacity;
};

CallStack::CallStack(size_t capacity)
    : block{std::make_unique<char[]>(capacity)},
      frame_headers_pos{0},
      capacity{capacity},
      pos{0} {
}

inline void check_overflow(size_t capacity, size_t current_pos, const function_definition &callee) {
    //as the operand stack stack_capacity is the end of the call stack, we do not include it in this check
    size_t total_frame_size = callee.locals_size + sizeof(frame_headers);
    if (current_pos + total_frame_size >= capacity) {
        throw StackOverflowError("Call stack exceeded stack_capacity");
    }
}

CallStack CallStack::create(size_t capacity, const function_definition &root, constant_index root_ref) {
    CallStack stack(capacity);
    stack.push_frame(root, root_ref);
    return stack;
}

void CallStack::push_frame(const function_definition &callee, constant_index callee_signature) {
    check_overflow(capacity, pos, callee);

    char *block = this->block.get();

    if (!is_empty()) {
        frame_headers* previous_frame = (frame_headers *)(block + frame_headers_pos);
        pos += previous_frame->operands_pos;
    }

    // reserve locals
    pos += callee.locals_size;

    *(frame_headers *)(block + pos) = {
        frame_headers_pos,
        callee_signature,
        0,
        0,
        callee.locals_size,
    };
    frame_headers_pos = pos;

    pos += sizeof(frame_headers);

}

void CallStack::pop_frame() {
    if (is_empty()) {
        throw MemoryError("Could not pop call stack: stack is already empty.");
    }
    pos = frame_headers_pos; // go to headers position, this also skips operands according to frame layout

    //retrieve headers
    const frame_headers* headers = (frame_headers *)(block.get() + pos);

    frame_headers_pos = headers->previous_frame_headers_pos;
    // pop locals
    pos -= headers->locals_capacity;
}

stack_frame CallStack::peek_frame() const {
    char *block = this->block.get();

    frame_headers* headers = (frame_headers *)(block + frame_headers_pos);

    //first byte position of operands
    size_t operands_first_byte = frame_headers_pos + sizeof(frame_headers);
    OperandStack frame_operands(block + operands_first_byte, headers->operands_pos, this->capacity - operands_first_byte);

    Locals frame_locals(block + (frame_headers_pos - headers->locals_capacity), headers->locals_capacity);
    return {headers->signature_index, &headers->instruction_pointer, frame_operands, frame_locals};
}

size_t CallStack::get_capacity() const {
    return capacity;
}

size_t CallStack::size() const {
    return pos;
}

inline bool CallStack::is_empty() const {
    return size() == 0;
}
