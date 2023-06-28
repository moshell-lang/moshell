#include "byte_reader.h"
#include "definitions/module_definition.h"
#include "interpreter.h"
#include <cxxabi.h>

#include <iostream>

extern "C" void moshell_exec(const char *bytes, size_t byte_count) {
    strings_t strings;
    ByteReader reader(bytes, byte_count);
    try {
        // read function definitions
        auto module_def = load_module(reader, strings);

        run_module(module_def, strings);
    } catch (const std::exception &e) {
        int status;
        char *exception_type = abi::__cxa_demangle(typeid(e).name(), nullptr, 0, &status);
        std::cerr << exception_type << ": " << e.what() << std::endl;
        free(exception_type);
    }
}
