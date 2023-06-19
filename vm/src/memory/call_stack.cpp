#include "call_stack.h"

CallStack::CallStack(size_t capacity)
    : block{(char *)malloc(capacity)},
      capacity{capacity},
      current_frame_start{0},
      current_frame_end{0} {
    if (block == nullptr) {
        throw MemoryError("Could not malloc call stack");
    }
}

std::tuple<CallStack, OperandStack, Locals> CallStack::create(size_t capacity, size_t root_frame_operand_size, size_t root_frame_locals_size) {
    CallStack stack(capacity);
    Locals locals(stack.block, root_frame_locals_size);
    OperandStack operands(stack.block + root_frame_locals_size, root_frame_operand_size);
    stack.current_frame_end = root_frame_operand_size + root_frame_locals_size;
    return {stack, operands, locals};
}

std::tuple<OperandStack, Locals> CallStack::push_frame(size_t operand_size, size_t locals_size) {
}

char *CallStack::pop_frame() {
}