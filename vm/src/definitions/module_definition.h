#pragma once

#include "errors.h"
#include "function_definition.h"
#include "memory/constant_pool.h"
#include <unordered_map>

class ByteReader;

/**
 * Thrown when a module gets invalidated during the loading phase
 * */
class InvalidModuleDescription : public VirtualMachineError {
public:
    explicit InvalidModuleDescription(std::string msg) : VirtualMachineError(msg) {}
};

struct module_definition {
    /// all the constants used by this module.
    /// The functions instructions always references this pool
    const ConstantPool pool;
    /// contains the functions declared in this module
    const std::unordered_map<constant_index, function_definition> functions;
};

/**
 * Loads a module, assuming that given bytes
 * */
module_definition load_module(ByteReader &reader, strings_t &strings);