#pragma once

#include "memory/object.h"
#include "stdlib_natives.h"

namespace msh {
    class loader;
    class pager;
    struct memory_page;
}

class RuntimeException : public std::runtime_error {
public:
    explicit RuntimeException(std::string msg);
};

/**
 * Will run given bytecode's main method.
 * @throws InvalidBytecodeError if an interpreted instruction set contains invalid instructions
 * @return true if the run did not abort
 */
bool run_unit(const msh::loader &loader, msh::pager &pager, const msh::memory_page &current_page, msh::heap &heap, const natives_functions_t &natives);
