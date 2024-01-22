#include "call_stack.h"

#include <cstring>

CallStack::CallStack(size_t capacity)
    : tape(capacity), operands_refs_offsets(capacity, false) {}

void CallStack::push_frame(const function_definition &callee) {
    size_t values_start = callee.locals_size;
    size_t locals_start = 0;
    if (!blocks.empty()) {
        stack_frame &caller = blocks.back();
        caller.operands.pop_bytes(callee.parameters_byte_count);
        locals_start = caller.operands.size();
        values_start = locals_start + callee.locals_size;
    }
    if (values_start > tape.size() || blocks.size() > tape.size()) {
        throw StackOverflowError("exceeded stack capacity via operand stack");
    }
    std::fill(operands_refs_offsets.begin() + locals_start, operands_refs_offsets.begin() + values_start, false);

    // zeroing non-parameter locals
    memset(tape.data() + locals_start + callee.parameters_byte_count, 0, callee.locals_size - callee.parameters_byte_count);
    blocks.push_back(stack_frame{
        callee,
        0,
        OperandStack(tape.data(), values_start, tape.size(), operands_refs_offsets),
        Locals(tape.data() + locals_start, callee.locals_size),
    });
}

void CallStack::pop_frame() {
    blocks.pop_back();
}

stack_frame &CallStack::peek_frame() {
    return blocks.back();
}

bool CallStack::is_empty() const {
    return blocks.empty();
}

void CallStack::clear() {
    blocks.clear();
    std::fill(operands_refs_offsets.begin(), operands_refs_offsets.end(), false);
}

std::vector<stack_frame>::iterator CallStack::begin() {
    return blocks.begin();
}

std::vector<stack_frame>::iterator CallStack::end() {
    return blocks.end();
}
