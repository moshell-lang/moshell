#include "stdlib_natives.h"
#include "interpreter.h"

void int_to_string(OperandStack &caller_stack, StringsHeap &strings) {
    int64_t value = caller_stack.pop_int();

    const std::string &str = strings.insert(std::to_string(value));
    caller_stack.push_reference((uint64_t)&str);
}

void float_to_string(OperandStack &caller_stack, StringsHeap &strings) {
    double value = caller_stack.pop_double();

    const std::string &str = strings.insert(std::to_string(value));
    caller_stack.push_reference((uint64_t)&str);
}

void str_concat(OperandStack &caller_stack, StringsHeap &strings) {
    auto right = (const std::string *)caller_stack.pop_reference();
    auto left = (const std::string *)caller_stack.pop_reference();

    std::string result = *left + *right;

    const std::string &str = strings.insert(std::move(result));
    caller_stack.push_reference((uint64_t)&str);
}

void str_eq(OperandStack &caller_stack, StringsHeap &) {
    const std::string &b = *(const std::string *)caller_stack.pop_reference();
    const std::string &a = *(const std::string *)caller_stack.pop_reference();
    caller_stack.push_byte(a == b);
}

natives_functions_t
load_natives(StringsHeap &strings) {
    natives_functions_t map;
    map[&strings.insert("std::Int::to_string")] = int_to_string;
    map[&strings.insert("std::Float::to_string")] = float_to_string;
    map[&strings.insert("std::String::concat")] = str_concat;
    map[&strings.insert("std::String::eq")] = str_eq;
    return map;
}
