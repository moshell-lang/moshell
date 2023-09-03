#include "stdlib_natives.h"
#include "interpreter.h"
#include "memory/object.h"

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

static void vec_len(OperandStack &caller_stack, msh::heap &) {
    const msh::obj_vector &vec = std::get<msh::obj_vector>(caller_stack.pop_reference());
    caller_stack.push_int(static_cast<int64_t>(vec.size()));
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
        {"lang::Vec::len", vec_len},
        {"lang::Vec::push", vec_push},
        {"lang::Vec::[]", vec_index},
    };
}
