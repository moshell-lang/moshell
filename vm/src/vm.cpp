#include "definitions/module_definition.h"
#include "interpreter.h"
#include <iostream>
#include <unordered_map>




extern "C" void exec(const char *bytes, size_t byte_count) {
    unsigned int constant_pool_bytes = 0;

    // read function definitions
    auto module_def = load_module(bytes, constant_pool_bytes);

    std::vector<std::string> strings;
    try {
        run(module_def.pool, bytes + constant_pool_bytes, byte_count - constant_pool_bytes, strings);
    } catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
    }
}
