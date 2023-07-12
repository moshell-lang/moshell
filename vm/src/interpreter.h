#pragma once

#include "definitions/bytecode_unit.h"
#include "memory/operand_stack.h"
#include "memory/strings.h"

/**
 * contains values needed during runtime interpretation
 */
struct runtime_state {
    /**
     * strings heap space
     */
    StringsHeap &strings;

    /**
     * loaded function definitions, bound with their string identifier
     */
    const std::unordered_map<const std::string *, function_definition> &functions;
    /**
     * native functions pointers, bound with their string identifier
     */
    const std::unordered_map<const std::string *, void (*)(OperandStack &, runtime_state &)> &native_functions;

    /**
     * The used constant pool
     */
    const ConstantPool &pool;
};

/**
 * Will run given bytecode's main method.
 * @throws InvalidBytecodeStructure if the given bytecode does not defines a <main>() function
 * @throws InvalidBytecodeError if an interpreted instruction set contains invalid instructions
 */
void run_unit(const bytecode_unit &module_def, StringsHeap &strings);
