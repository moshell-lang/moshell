#include "stdlib_natives.h"

void int_to_string(OperandStack &caller_stack, runtime_state &state) {
    int64_t value = caller_stack.pop_int();

    const std::string &str = state.strings.insert(std::to_string(value));
    caller_stack.push_reference((uint64_t)&str);
}

void float_to_string(OperandStack &caller_stack, runtime_state &state) {
    double value = caller_stack.pop_double();

    const std::string &str = state.strings.insert(std::to_string(value));
    caller_stack.push_reference((uint64_t)&str);
}

void str_concat(OperandStack &caller_stack, runtime_state &state) {
    auto right = (const std::string *)caller_stack.pop_reference();
    auto left = (const std::string *)caller_stack.pop_reference();

    std::string result = *left + *right;

    const std::string &str = state.strings.insert(std::move(result));
    caller_stack.push_reference((uint64_t)&str);
}

void str_eq(OperandStack &caller_stack, runtime_state &state) {
    const std::string &b = *(const std::string *)caller_stack.pop_reference();
    const std::string &a = *(const std::string *)caller_stack.pop_reference();
    caller_stack.push_byte(a == b);
}

std::unordered_map<const std::string *, void (*)(OperandStack &, runtime_state &)>
load_natives(StringsHeap &strings) {
    std::unordered_map<const std::string *, void (*)(OperandStack &, runtime_state &)> map;
    map[&strings.insert("std::Int::to_string")] = int_to_string;
    map[&strings.insert("std::Float::to_string")] = float_to_string;
    map[&strings.insert("std::String::concat")] = str_concat;
    map[&strings.insert("std::String::eq")] = str_eq;

    return map;
}
