#pragma once

#include "memory/strings.h"
#include "stdlib_natives.h"

namespace msh {
    class loader;
    class pager;
    struct memory_page;
}

/**
 * used to report runtime exceptions.
 * When caught by the interpreter, the interpreter starts to panic and
 * uses the exception's message as the panic message
 * */
class RuntimeException : public std::runtime_error {
public:
    explicit RuntimeException(std::string msg);
};

/**
 * Will run given bytecode's main method.
 * @throws InvalidBytecodeError if an interpreted instruction set contains invalid instructions
 * @return true if the run did not abort
 */
bool run_unit(const msh::loader &loader, msh::pager &pager, const msh::memory_page &current_page, StringsHeap &strings, const natives_functions_t &natives);
