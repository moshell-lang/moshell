#include "definitions/module_definition.h"
#include "interpreter.h"
#include <cxxabi.h>
#include <iostream>

extern "C" void moshell_exec(const char *bytes, size_t byte_count) {
    strings_t strings;
    try {

        // read function definitions
        auto module_def = load_module(bytes, strings);

        run_module(module_def, strings);
    } catch (std::exception &e) {
        int status;
        char *exception_type = abi::__cxa_demangle(typeid(e).name(), nullptr, 0, &status);
        std::cerr << exception_type << ": " << e.what() << std::endl;
        free(exception_type);
    }
}
