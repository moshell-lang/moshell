#pragma once

#include "errors.h"
#include "function_definition.h"
#include "memory/constant_pool.h"
#include <unordered_map>

class ByteReader;

/**
 * Thrown when a module gets invalidated during the loading phase
 */
class InvalidModuleDescription : public VirtualMachineError {
public:
    explicit InvalidModuleDescription(std::string msg) : VirtualMachineError(msg) {}
};

struct module_definition {
    /// all the constants used by this module.
    /// The functions instructions of this module always references to the indexes of this pool
    const ConstantPool pool;
    /// contains the functions declared in this module
    const std::unordered_map<const std::string *, function_definition> functions;
};

/**
 * Loads a module, placing all string constants in the `strings` argument
 * @param reader the bytes to reader / load
 * @param strings the strings set where all the strings constants are interned.
 * @throws InvalidBytecodeDefinition if the reader ran out of bytes while reading the module
 * @throws InvalidModuleDefinition if the constant pool or a function isn't validated
 */
module_definition load_module(ByteReader &reader, strings_t &strings);