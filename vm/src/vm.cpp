#include "definitions/module_definition.h"
#include "interpreter.h"
#include <iostream>
#include <unordered_map>




extern "C" int exec(const char *bytes, size_t byte_count) {
    strings_t strings;
    try {

        // read function definitions
        auto module_def = load_module(bytes, strings);

        return run_module(module_def, strings);
    } catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
    }
}
