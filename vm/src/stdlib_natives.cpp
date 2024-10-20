#include "stdlib_natives.h"
#include "interpreter.h"
#include "memory/heap.h"
#include <charconv>
#include <cmath>
#include <cstring>
#include <filesystem>
#include <glob.h>
#include <iostream>
#include <pwd.h>
#include <sys/wait.h>
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

static void str_len(OperandStack &caller_stack, runtime_memory &) {
    const std::string &str = caller_stack.pop_reference().get<const std::string>();
    caller_stack.push_int(static_cast<int64_t>(str.length()));
}

static void str_index(OperandStack &caller_stack, runtime_memory &mem) {
    // Tests if the index is at a UTF-8 char boundary
    auto is_char_boundary = [](const std::string &s, size_t index) {
        return index == 0 || index == s.length() || static_cast<signed char>(s[index]) >= -0x40;
    };

    int64_t n = caller_stack.pop_int();
    size_t index = static_cast<size_t>(n);
    const std::string &str = caller_stack.pop_reference().get<const std::string>();
    if (n < 0 || index >= str.length()) {
        throw RuntimeException("Index " + std::to_string(n) + " is out of range, the length is " + std::to_string(str.length()) + ".");
    }
    if (!is_char_boundary(str, index)) {
        throw RuntimeException("Index " + std::to_string(n) + " is not a char boundary.");
    }
    char c = str[index];
    int codepoint_len = 1;
    if ((c & 0xf8) == 0xf0) {
        codepoint_len = 4;
    } else if ((c & 0xf0) == 0xe0) {
        codepoint_len = 3;
    } else if ((c & 0xe0) == 0xc0) {
        codepoint_len = 2;
    }
    if ((index + codepoint_len) > str.length()) {
        codepoint_len = 1;
    }
    std::string codepoint = str.substr(index, codepoint_len);
    msh::obj &obj = mem.emplace(codepoint);
    caller_stack.push_reference(obj);
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

static void vec_extend(OperandStack &caller_stack, runtime_memory &) {
    msh::obj_vector &right = caller_stack.pop_reference().get<msh::obj_vector>();
    msh::obj_vector &left = caller_stack.pop_reference().get<msh::obj_vector>();
    left.insert(left.end(), right.begin(), right.end());
}

static void vec_index(OperandStack &caller_stack, runtime_memory &) {
    int64_t n = caller_stack.pop_int();
    size_t index = static_cast<size_t>(n);
    msh::obj_vector &vec = caller_stack.pop_reference().get<msh::obj_vector>();
    if (index >= vec.size()) {
        throw RuntimeException("Index " + std::to_string(n) + " is out of range, the length is " + std::to_string(vec.size()) + ".");
    }
    msh::obj& ref = *vec[index];
    caller_stack.push_reference(ref);
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

static void expand_glob(OperandStack &caller_stack, runtime_memory &mem) {
    glob_t glob_result;
    memset(&glob_result, 0, sizeof(glob_result));
    const std::string &pattern = caller_stack.pop_reference().get<const std::string>();
    int ret = glob(pattern.c_str(), GLOB_TILDE, nullptr, &glob_result);
    if (ret != 0 && ret != GLOB_NOMATCH) {
        globfree(&glob_result);
        switch (ret) {
        case GLOB_NOSPACE:
            throw RuntimeException("Failed to glob files: out of memory.");
        case GLOB_ABORTED:
            throw RuntimeException("Failed to glob files: read error.");
        default:
            throw RuntimeException("Failed to glob files: unknown error.");
        }
    }
    msh::obj_vector res;
    res.reserve(glob_result.gl_pathc);
    msh::obj &heap_obj = mem.emplace(std::move(res));
    caller_stack.push_reference(heap_obj);
    msh::obj_vector &vec = heap_obj.get<msh::obj_vector>();
    for (size_t i = 0; i < glob_result.gl_pathc; ++i) {
        vec.push_back(&mem.emplace(glob_result.gl_pathv[i]));
    }
    globfree(&glob_result);
}

static void gc(OperandStack &, runtime_memory &mem) {
    mem.run_gc();
}

static void is_operands_empty(OperandStack &os, runtime_memory &) {
    os.push<bool>(os.size() == 0);
}

static void program_arguments(OperandStack &caller_stack, runtime_memory &mem) {
    const std::vector<std::string> &pargs = mem.program_arguments();
    msh::obj &obj = mem.emplace(msh::obj_vector());
    caller_stack.push_reference(obj);
    msh::obj_vector &vec = obj.get<msh::obj_vector>();
    vec.reserve(pargs.size());
    for (const std::string &arg : pargs) {
        vec.push_back(&mem.emplace(arg));
    }
}

static void get_fd_path(OperandStack &caller_stack, runtime_memory &mem) {
    int fd = caller_stack.pop_int();
    std::string path = "/dev/fd/";
    path += std::to_string(fd);
    msh::obj &str = mem.emplace(std::move(path));
    caller_stack.push_reference(str);
}

static void process_wait(OperandStack &caller_stack, runtime_memory &) {
    pid_t pid = static_cast<pid_t>(caller_stack.pop_int());
    int status;
    if (waitpid(pid, &status, 0) == -1) {
        throw RuntimeException("Failed to wait for process " + std::to_string(pid) + ": " + strerror(errno) + ".");
    }
}

static void process_wait_all(OperandStack &, runtime_memory &) {
    int status;
    while (wait(&status) > 0) {
    }
}

natives_functions_t load_natives() {
    return natives_functions_t{
        {"lang::Int::to_string", int_to_string},
        {"lang::Float::to_string", float_to_string},

        {"lang::String::concat", str_concat},
        {"lang::String::eq", str_eq},
        {"lang::String::split", str_split},
        {"lang::String::bytes", str_bytes},
        {"lang::String::len", str_len},
        {"lang::String::[]", str_index},

        {"lang::Vec::pop", vec_pop},
        {"lang::Vec::pop_head", vec_pop_head},
        {"lang::Vec::len", vec_len},
        {"lang::Vec::push", vec_push},
        {"lang::Vec::extend", vec_extend},
        {"lang::Vec::[]", vec_index},
        {"lang::Vec::[]=", vec_index_set},

        {"lang::glob::expand", expand_glob},

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

        {"std::convert::parse_int_radix", parse_int_radix},

        {"std::memory::gc", gc},
        {"std::memory::empty_operands", is_operands_empty},
        {"std::memory::program_arguments", program_arguments},

        {"std::math::ceil", ceil},
        {"std::math::floor", floor},
        {"std::math::round", round},

        {"std::process::get_fd_path", get_fd_path},
        {"std::process::wait", process_wait},
        {"std::process::wait_all", process_wait_all},
    };
}
