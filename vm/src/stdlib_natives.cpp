#include "stdlib_natives.h"
#include "interpreter.h"
#include "memory/object.h"
#include <cmath>
#include <iostream>

static void int_to_string(OperandStack &caller_stack, msh::heap &heap) {
    int64_t value = caller_stack.pop_int();
    msh::obj &str = heap.insert(std::to_string(value));
    caller_stack.push_reference(str);
}

static void float_to_string(OperandStack &caller_stack, msh::heap &heap) {
    double value = caller_stack.pop_double();
    msh::obj &str = heap.insert(std::to_string(value));
    caller_stack.push_reference(str);
}

static void str_concat(OperandStack &caller_stack, msh::heap &heap) {
    const std::string &right = std::get<const std::string>(caller_stack.pop_reference());
    const std::string &left = std::get<const std::string>(caller_stack.pop_reference());

    std::string result = left + right;

    msh::obj &str = heap.insert(std::move(result));
    caller_stack.push_reference(str);
}

static void str_eq(OperandStack &caller_stack, msh::heap &) {
    const std::string &right = std::get<const std::string>(caller_stack.pop_reference());
    const std::string &left = std::get<const std::string>(caller_stack.pop_reference());
    caller_stack.push_byte(static_cast<int8_t>(right == left));
}

void get_env(OperandStack &caller_stack, msh::heap &heap) {
    const std::string &var_name = std::get<const std::string>(caller_stack.pop_reference());
    char *value = getenv(var_name.c_str());
    if (value == nullptr) {
        throw RuntimeException("Environment variable " + var_name + " isn't defined.");
    }
    caller_stack.push_reference(heap.insert(value));
}

void set_env(OperandStack &caller_stack, msh::heap &) {
    const std::string &value = std::get<const std::string>(caller_stack.pop_reference());
    const std::string &var_name = std::get<const std::string>(caller_stack.pop_reference());
    setenv(var_name.c_str(), value.c_str(), true);
}

void is_env_def(OperandStack &caller_stack, msh::heap &) {
    const std::string &var_name = std::get<const std::string>(caller_stack.pop_reference());
    caller_stack.push(getenv(var_name.c_str()) != nullptr);
}

void panic(OperandStack &caller_stack, msh::heap &) {
    const std::string &message = std::get<const std::string>(caller_stack.pop_reference());
    throw RuntimeException(message);
}

void exit(OperandStack &caller_stack, msh::heap &) {
    uint8_t code = caller_stack.pop_byte();
    exit(code);
}

void read_line(OperandStack &caller_stack, msh::heap &heap) {
    std::string line;
    std::getline(std::cin, line);

    msh::obj &obj = heap.insert(msh::obj{line});
    caller_stack.push_reference(obj);
}

void to_exitcode(OperandStack &caller_stack, msh::heap &) {
    int64_t i = caller_stack.pop_int();
    uint8_t exitcode = static_cast<uint8_t>(i);
    if (exitcode != i) {
        throw RuntimeException("cannot cast int to exitcode: " + std::to_string(i) + " is out of bounds.");
    }

    caller_stack.push_byte(static_cast<int8_t>(exitcode));
}

void floor(OperandStack &caller_stack, msh::heap &) {
    double d = caller_stack.pop_double();

    caller_stack.push_int(static_cast<int64_t>(std::floor(d)));
}

void ceil(OperandStack &caller_stack, msh::heap &) {
    double d = caller_stack.pop_double();

    caller_stack.push_int(static_cast<int64_t>(std::ceil(d)));
}

void round(OperandStack &caller_stack, msh::heap &) {
    double d = caller_stack.pop_double();

    caller_stack.push_int(static_cast<int64_t>(std::round(d)));
}

static void str_split(OperandStack &caller_stack, msh::heap &heap) {
    const std::string &delim = std::get<const std::string>(caller_stack.pop_reference());
    const std::string &str = std::get<const std::string>(caller_stack.pop_reference());
    msh::obj_vector res;
    if (delim.empty()) {
        throw RuntimeException("The delimiter is empty.");
    }

    std::string word;
    size_t start = 0, end, delim_len = delim.length();
    while ((end = str.find(delim, start)) != std::string::npos) {
        word = str.substr(start, end - start);
        start = end + delim_len;
        res.push_back(&heap.insert(word));
    }
    res.push_back(&heap.insert(str.substr(start)));

    caller_stack.push_reference(heap.insert(std::move(res)));
}

static void str_bytes(OperandStack &caller_stack, msh::heap &heap) {
    const std::string &str = std::get<const std::string>(caller_stack.pop_reference());
    msh::obj_vector res;
    res.reserve(str.length());
    for (char c : str) {
        res.push_back(&heap.insert(static_cast<int64_t>(c)));
    }
    caller_stack.push_reference(heap.insert(std::move(res)));
}

static void vec_len(OperandStack &caller_stack, msh::heap &) {
    const msh::obj_vector &vec = std::get<msh::obj_vector>(caller_stack.pop_reference());
    caller_stack.push_int(static_cast<int64_t>(vec.size()));
}

static void vec_pop(OperandStack &caller_stack, msh::heap &) {
    msh::obj_vector &vec = std::get<msh::obj_vector>(caller_stack.pop_reference());
    msh::obj &last_element = *vec.back();
    vec.pop_back();
    caller_stack.push_reference(last_element);
}

static void vec_push(OperandStack &caller_stack, msh::heap &) {
    msh::obj &ref = caller_stack.pop_reference();
    msh::obj_vector &vec = std::get<msh::obj_vector>(caller_stack.pop_reference());
    vec.push_back(&ref);
}

static void vec_index(OperandStack &caller_stack, msh::heap &) {
    int64_t n = caller_stack.pop_int();
    size_t index = static_cast<size_t>(n);
    msh::obj_vector &vec = std::get<msh::obj_vector>(caller_stack.pop_reference());
    if (index >= vec.size()) {
        throw RuntimeException("Index " + std::to_string(n) + " is out of range, the length is " + std::to_string(vec.size()) + ".");
    }
    caller_stack.push_reference(*vec[index]);
}

natives_functions_t load_natives(msh::heap &) {
    return natives_functions_t{
        {"lang::Int::to_string", int_to_string},
        {"lang::Float::to_string", float_to_string},

        {"lang::String::concat", str_concat},
        {"lang::String::eq", str_eq},
        {"lang::String::split", str_split},
        {"lang::String::bytes", str_bytes},

        {"lang::Vec::pop", vec_pop},
        {"lang::Vec::len", vec_len},
        {"lang::Vec::push", vec_push},
        {"lang::Vec::[]", vec_index},

        {"std::panic", panic},
        {"std::exit", exit},
        {"std::env", get_env},
        {"std::set_env", set_env},
        {"std::is_env_def", is_env_def},
        {"std::read_line", read_line},
        
        {"std::convert::to_exitcode", to_exitcode},
        {"std::convert::ceil", ceil},
        {"std::convert::floor", floor},
        {"std::convert::round", round},
    };
}
