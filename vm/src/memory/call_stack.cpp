#include "call_stack.h"

struct frame_headers {
    size_t previous_frame_headers_pos;
    constant_index signature_index;
    size_t instruction_pointer;

    size_t operands_pos;
    size_t operands_capacity;
    size_t locals_capacity;
};

CallStack::CallStack(size_t capacity)
    : block{std::make_unique<char[]>(capacity)},
      frame_headers_pos{0},
      capacity{capacity},
      pos{0} {
}

inline void check_overflow(size_t capacity, size_t current_pos, const function_definition &callee) {
    size_t total_frame_size = callee.locals_size + sizeof(frame_headers) + callee.operand_stack_capacity;
    if (current_pos + total_frame_size >= capacity) {
        throw StackOverflowError("Call stack exceeded capacity");
    }
}

size_t CallStack::size() const {
    return pos;
}

CallStack CallStack::create(size_t capacity, const function_definition &root, constant_index root_ref) {
    CallStack stack(capacity);
    stack.push_frame_with_overlap(root, root_ref, 0);
    return stack;
}

void CallStack::push_frame(const function_definition &callee, constant_index callee_ref, const OperandStack &caller_operands) {
    size_t end_offset = (caller_operands.get_capacity() - caller_operands.size());
    push_frame_with_overlap(callee, callee_ref, end_offset);
}

void CallStack::push_frame_with_overlap(const function_definition &callee, constant_index callee_ref, size_t start_offset) {
    check_overflow(capacity, pos - start_offset, callee);

    char *block = this->block.get();

    // reserve locals, left-shifting with given offset to make upper frame's operand be this frame locals first part
    pos += callee.locals_size - start_offset;

    *(frame_headers *)(block + pos) = {
        frame_headers_pos,
        callee_ref,
        0,
        0,
        callee.operand_stack_capacity,
        callee.locals_size,
    };
    frame_headers_pos = pos;

    pos += sizeof(frame_headers);

    // then reserve operands
    pos += callee.operand_stack_capacity;
}

void CallStack::pop_frame() {
    pos = frame_headers_pos; // go to headers position, this skips operands according to frame layout

    //retrieve headers
    const frame_headers* headers = (frame_headers *)(block.get() + pos);

    frame_headers_pos = headers->previous_frame_headers_pos;
    // pop locals
    pos -= headers->locals_capacity;
}

stack_frame CallStack::peek_frame() {
    char *block = this->block.get();

    size_t pos = frame_headers_pos; //go to headers
    frame_headers* headers = (frame_headers *)(block + pos);

    OperandStack frame_operands(block + this->pos - headers->operands_capacity, headers->operands_pos, headers->operands_capacity);

    Locals frame_locals(block + (pos - headers->locals_capacity), headers->locals_capacity);
    return {headers->signature_index, &headers->instruction_pointer, frame_operands, frame_locals};
}

bool CallStack::is_empty() {
    return pos == 0;
}
