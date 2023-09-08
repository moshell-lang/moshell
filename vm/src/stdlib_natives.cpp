#include "stdlib_natives.h"
#include "interpreter.h"
#include <cmath>
#include <iostream>
#include <limits>

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

void get_env(OperandStack &caller_stack, StringsHeap &strings) {
    const std::string &var_name = *(const std::string *)caller_stack.pop_reference();
    const std::string value = getenv(var_name.c_str());
    caller_stack.push_reference((uint64_t)&strings.insert(value));
}

void set_env(OperandStack &caller_stack, StringsHeap &) {
    const std::string &value = *(const std::string *)caller_stack.pop_reference();
    const std::string &var_name = *(const std::string *)caller_stack.pop_reference();
    setenv(var_name.c_str(), value.c_str(), true);
}

void is_env_def(OperandStack &caller_stack, StringsHeap &) {
    const std::string &var_name = *(const std::string *)caller_stack.pop_reference();
    caller_stack.push(getenv(var_name.c_str()) != NULL);
}

void panic(OperandStack &caller_stack, StringsHeap &) {
    const std::string &message = *(const std::string *)caller_stack.pop_reference();
    throw RuntimeException(message);
}

void exit(OperandStack &caller_stack, StringsHeap &) {
    uint8_t code = caller_stack.pop_byte();
    exit(code);
}

void read_line(OperandStack &caller_stack, StringsHeap &strings) {
    std::string line;
    std::getline(std::cin, line);

    caller_stack.push_reference((uint64_t)&strings.insert(line));
}

void to_exitcode(OperandStack &caller_stack, StringsHeap &) {
    int64_t i = caller_stack.pop_int();
    if (i < 0 || i > std::numeric_limits<uint8_t>::max()) {
        throw RuntimeException("cannot cast int to exitcode: " + std::to_string(i) + " is out of bounds.");
    }

    caller_stack.push_byte((uint8_t)i);
}

void truncate(OperandStack &caller_stack, StringsHeap &) {
    double d = caller_stack.pop_double();

    caller_stack.push_int((int64_t)std::trunc(d));
}

void ceil(OperandStack &caller_stack, StringsHeap &) {
    double d = caller_stack.pop_double();

    caller_stack.push_int((int64_t)std::ceil(d));
}

void round(OperandStack &caller_stack, StringsHeap &) {
    double d = caller_stack.pop_double();

    caller_stack.push_int((int64_t)std::round(d));
}

natives_functions_t
load_natives(StringsHeap &strings) {
    natives_functions_t map;
    map[&strings.insert("lang::Int::to_string")] = int_to_string;
    map[&strings.insert("lang::Float::to_string")] = float_to_string;
    map[&strings.insert("lang::String::concat")] = str_concat;
    map[&strings.insert("lang::String::eq")] = str_eq;

    map[&strings.insert("std::env")] = get_env;
    map[&strings.insert("std::set_env")] = set_env;
    map[&strings.insert("std::is_env_def")] = is_env_def;
    map[&strings.insert("std::panic")] = panic;
    map[&strings.insert("std::exit")] = exit;
    map[&strings.insert("std::read_line")] = read_line;

    map[&strings.insert("std::convert::to_exitcode")] = to_exitcode;
    map[&strings.insert("std::convert::trunc")] = truncate;
    map[&strings.insert("std::convert::ceil")] = ceil;
    map[&strings.insert("std::convert::round")] = round;
    return map;
}
