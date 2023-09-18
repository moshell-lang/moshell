#pragma once

#include "memory/heap.h"
#include "stdlib_natives.h"

namespace msh {
    class loader;
    class pager;
    class gc;
    struct memory_page;
}

class runtime_memory {
    msh::heap &heap;
    msh::gc &gc;

    size_t last_gc_heap_size;

public:
    runtime_memory(msh::heap &heap, msh::gc &gc);

    void run_gc();

    msh::obj &emplace(msh::obj_data &&data);
};

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
bool run_unit(const msh::loader &loader, msh::pager &pager, const msh::memory_page &current_page, msh::heap &heap, const natives_functions_t &natives);
