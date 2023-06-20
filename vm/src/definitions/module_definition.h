#pragma once

#include "memory/constant_pool.h"
#include <unordered_map>

class InvalidModuleDescription: public VirtualMachineError {
public:
    explicit InvalidModuleDescription(const char *msg): VirtualMachineError(msg) {}
};

struct module_definition {
    const ConstantPool pool;
    const std::unordered_map<constant_index, function_definition> functions;
};


module_definition load_module(const char *bytes);