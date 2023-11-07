#include "stdlib_natives.h"
#include "interpreter.h"
#include "memory/heap.h"
#include <charconv>
#include <cmath>
#include <cstring>
#include <filesystem>
#include <iostream>
#include <pwd.h>
#include <unistd.h>

static void int_to_string(OperandStack &caller_stack, runtime_memory &mem) {
    int64_t value = caller_stack.pop_int();
    msh::obj &str = mem.emplace(std::to_string(value));
    caller_stack.push_reference(str);
}

static void float_to_string(OperandStack &caller_stack, runtime_memory &mem) {
    double value = caller_stack.pop_double();
    msh::obj &str = mem.emplace(std::to_string(value));
    caller_stack.push_reference(str);
}

static void str_concat(OperandStack &caller_stack, runtime_memory &mem) {
    const std::string &right = caller_stack.pop_reference().get<const std::string>();
    const std::string &left = caller_stack.pop_reference().get<const std::string>();

    std::string result = left + right;

    msh::obj &str = mem.emplace(std::move(result));
    caller_stack.push_reference(str);
}

static void str_eq(OperandStack &caller_stack, runtime_memory &) {
    const std::string &right = caller_stack.pop_reference().get<const std::string>();
    const std::string &left = caller_stack.pop_reference().get<const std::string>();
    int8_t test = static_cast<int8_t>(right == left);
    caller_stack.push_byte(test);
}

static void get_env(OperandStack &caller_stack, runtime_memory &mem) {
    const std::string &var_name = caller_stack.pop_reference().get<const std::string>();
    const char *value = getenv(var_name.c_str());
    if (value == nullptr) {
        caller_stack.push(nullptr);
    } else {
        caller_stack.push_reference(mem.emplace(value));
    }
}

static void set_env(OperandStack &caller_stack, runtime_memory &) {
    const std::string &value = caller_stack.pop_reference().get<const std::string>();
    const std::string &var_name = caller_stack.pop_reference().get<const std::string>();
    setenv(var_name.c_str(), value.c_str(), true);
}

static void panic(OperandStack &caller_stack, runtime_memory &) {
    const std::string &message = caller_stack.pop_reference().get<const std::string>();
    throw RuntimeException(message);
}

static void exit(OperandStack &caller_stack, runtime_memory &) {
    uint8_t code = caller_stack.pop_byte();
    exit(code);
}

static void read_line(OperandStack &caller_stack, runtime_memory &mem) {
    std::string line;
    std::getline(std::cin, line);

    msh::obj &obj = mem.emplace(line);
    caller_stack.push_reference(obj);
}

static void new_vec(OperandStack &caller_stack, runtime_memory &mem) {
    msh::obj &obj = mem.emplace(msh::obj_vector());
    caller_stack.push_reference(obj);
}

static void some(OperandStack &, runtime_memory &) {
    // the argument is the returned value
}

static void none(OperandStack &caller_stack, runtime_memory &) {
    caller_stack.push(nullptr);
}

static void cd(OperandStack &caller_stack, runtime_memory &) {
    const std::string &path = caller_stack.pop_reference().get<const std::string>();
    if (chdir(path.c_str()) == -1) {
        throw RuntimeException("Failed to change directory to " + path + ": " + strerror(errno) + ".");
    }
}

static void working_dir(OperandStack &caller_stack, runtime_memory &mem) {
    std::filesystem::path path = std::filesystem::current_path();
    msh::obj &obj = mem.emplace(path.string());
    caller_stack.push_reference(obj);
}

static void home_dir(OperandStack &caller_stack, runtime_memory &mem) {
    const std::string &username = caller_stack.pop_reference().get<const std::string>();
    struct passwd *pass = getpwnam(username.c_str());
    if (pass == nullptr) {
        caller_stack.push(nullptr);
    } else {
        msh::obj &obj = mem.emplace(pass->pw_dir);
        caller_stack.push_reference(obj);
    }
}

static void current_home_dir(OperandStack &caller_stack, runtime_memory &mem) {
    const char *homedir;
    if ((homedir = getenv("HOME")) == nullptr) {
        struct passwd *pass = getpwuid(getuid());
        if (pass == nullptr) {
            throw RuntimeException("Failed to get the current user's home directory: " + std::string(strerror(errno)) + ".");
        }
        homedir = pass->pw_dir;
    }
    msh::obj &obj = mem.emplace(homedir);
    caller_stack.push_reference(obj);
}

static void floor(OperandStack &caller_stack, runtime_memory &) {
    double d = caller_stack.pop_double();

    caller_stack.push_int(static_cast<int64_t>(std::floor(d)));
}

static void ceil(OperandStack &caller_stack, runtime_memory &) {
    double d = caller_stack.pop_double();

    caller_stack.push_int(static_cast<int64_t>(std::ceil(d)));
}

static void round(OperandStack &caller_stack, runtime_memory &) {
    double d = caller_stack.pop_double();

    caller_stack.push_int(static_cast<int64_t>(std::round(d)));
}

static void parse_int_radix(OperandStack &caller_stack, runtime_memory &mem) {
    int base = static_cast<int>(caller_stack.pop_int());
    const std::string &str = caller_stack.pop_reference().get<const std::string>();

    if (base < 2 || base > 36) {
        throw RuntimeException("Invalid base: " + std::to_string(base) + ".");
    }
    const char *first = str.data();
    if (!str.empty() && str.front() == '+') { // Allow leading '+'
        first += 1;
    }

    int64_t value = 0;
    const auto result = std::from_chars(first,
                                        str.data() + str.size(),
                                        value, base);

    // Ensure that the entire string was consumed and that the result is valid
    if (result.ec == std::errc() && result.ptr == &*str.end()) {
        caller_stack.push_reference(mem.emplace(value));
    } else {
        caller_stack.push(nullptr);
    }
}

static void str_split(OperandStack &caller_stack, runtime_memory &mem) {
    msh::native_procedure<msh::obj *> procedure(caller_stack);
    const std::string &delim = procedure.pop_reference().get<const std::string>();
    const std::string &str = procedure.pop_reference().get<const std::string>();

    msh::obj &res_obj = mem.emplace(msh::obj_vector());
    caller_stack.push_reference(res_obj);
    msh::obj_vector &res = res_obj.get<msh::obj_vector>();

    if (delim.empty()) {
        throw RuntimeException("The delimiter is empty.");
    }

    std::string word;
    size_t start = 0, end, delim_len = delim.length();
    while ((end = str.find(delim, start)) != std::string::npos) {
        word = str.substr(start, end - start);
        start = end + delim_len;
        res.push_back(&mem.emplace(word));
    }
    res.push_back(&mem.emplace(str.substr(start)));
}

static void str_bytes(OperandStack &caller_stack, runtime_memory &mem) {
    msh::native_procedure<msh::obj *> procedure(caller_stack);
    const std::string &str = procedure.pop_reference().get<const std::string>();
    msh::obj_vector res;
    res.reserve(str.length());

    msh::obj &heap_obj = mem.emplace(std::move(res));
    caller_stack.push_reference(heap_obj);
    msh::obj_vector &heap_res = heap_obj.get<msh::obj_vector>();

    for (char c : str) {
        heap_res.push_back(&mem.emplace(static_cast<int64_t>(c)));
    }
}

static void vec_len(OperandStack &caller_stack, runtime_memory &) {
    const msh::obj_vector &vec = caller_stack.pop_reference().get<msh::obj_vector>();
    caller_stack.push_int(static_cast<int64_t>(vec.size()));
}

static void vec_pop(OperandStack &caller_stack, runtime_memory &) {
    msh::obj_vector &vec = caller_stack.pop_reference().get<msh::obj_vector>();
    if (vec.empty()) {
        caller_stack.push(nullptr);
        return;
    }
    msh::obj &last_element = *vec.back();
    vec.pop_back();
    caller_stack.push_reference(last_element);
}

static void vec_pop_head(OperandStack &caller_stack, runtime_memory &) {
    msh::obj_vector &vec = caller_stack.pop_reference().get<msh::obj_vector>();
    if (vec.empty()) {
        caller_stack.push(nullptr);
        return;
    }
    msh::obj *first_element = *vec.begin();
    vec.erase(vec.begin());
    caller_stack.push_reference(*first_element);
}

static void vec_push(OperandStack &caller_stack, runtime_memory &) {
    msh::obj &ref = caller_stack.pop_reference();
    msh::obj_vector &vec = caller_stack.pop_reference().get<msh::obj_vector>();
    vec.push_back(&ref);
}

static void vec_index(OperandStack &caller_stack, runtime_memory &) {
    int64_t n = caller_stack.pop_int();
    size_t index = static_cast<size_t>(n);
    msh::obj_vector &vec = caller_stack.pop_reference().get<msh::obj_vector>();
    if (index >= vec.size()) {
        throw RuntimeException("Index " + std::to_string(n) + " is out of range, the length is " + std::to_string(vec.size()) + ".");
    }
    caller_stack.push_reference(*vec[index]);
}

static void vec_index_set(OperandStack &caller_stack, runtime_memory &) {
    msh::obj &ref = caller_stack.pop_reference();
    int64_t n = caller_stack.pop_int();
    size_t index = static_cast<size_t>(n);
    msh::obj_vector &vec = caller_stack.pop_reference().get<msh::obj_vector>();
    if (index >= vec.size()) {
        throw RuntimeException("Index " + std::to_string(n) + " is out of range, the length is " + std::to_string(vec.size()) + ".");
    }
    vec[index] = &ref;
}

static void gc(OperandStack &, runtime_memory &mem) {
    mem.run_gc();
}

static void is_operands_empty(OperandStack &os, runtime_memory &) {
    os.push(os.size() == 0);
}

static void program_arguments(OperandStack &os, runtime_memory &mem) {
    std::vector<std::string> &pargs = mem.program_arguments();
    msh::obj_vector vec;
    for (const std::string &arg : pargs) {
        vec.push_back(&mem.emplace(arg));
    }
    os.push_reference(mem.emplace(vec));
}

natives_functions_t load_natives() {
    return natives_functions_t{
        {"lang::Int::to_string", int_to_string},
        {"lang::Float::to_string", float_to_string},

        {"lang::String::concat", str_concat},
        {"lang::String::eq", str_eq},
        {"lang::String::split", str_split},
        {"lang::String::bytes", str_bytes},

        {"lang::Vec::pop", vec_pop},
        {"lang::Vec::pop_head", vec_pop_head},
        {"lang::Vec::len", vec_len},
        {"lang::Vec::push", vec_push},
        {"lang::Vec::[]", vec_index},
        {"lang::Vec::[]=", vec_index_set},

        {"std::panic", panic},
        {"std::exit", exit},
        {"std::env", get_env},
        {"std::set_env", set_env},
        {"std::read_line", read_line},
        {"std::new_vec", new_vec},
        {"std::some", some},
        {"std::none", none},
        {"std::cd", cd},
        {"std::working_dir", working_dir},
        {"std::home_dir", home_dir},
        {"std::current_home_dir", current_home_dir},

        {"std::memory::gc", gc},
        {"std::memory::empty_operands", is_operands_empty},
        {"std::memory::program_arguments", program_arguments},

        {"std::convert::ceil", ceil},
        {"std::convert::floor", floor},
        {"std::convert::round", round},
        {"std::convert::parse_int_radix", parse_int_radix},
    };
}
