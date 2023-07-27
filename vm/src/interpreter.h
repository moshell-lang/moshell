#pragma once

#include "memory/strings.h"
#include "stdlib_natives.h"

namespace msh {
    class loader;
    class pager;
    struct memory_page;
}

/**
 * Will run given bytecode's main method.
 * @throws InvalidBytecodeStructure if the given bytecode does not defines a <main>() function
 * @throws InvalidBytecodeError if an interpreted instruction set contains invalid instructions
 */
void run_unit(const msh::loader &loader, msh::pager &pager, const msh::memory_page &current_page, StringsHeap &strings, const natives_functions_t &natives);
