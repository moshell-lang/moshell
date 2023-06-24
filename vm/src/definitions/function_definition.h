#pragma once

#include "types.h"
#include <cstddef>
#include <memory>
#include <string>
#include <vector>

struct function_signature {
    const std::string name;

    const std::vector<Type> params;

    const Type return_type;

    explicit function_signature(std::string name, std::vector<Type> params, Type return_type);
};

struct function_definition {
    const char *instructions;
    size_t instruction_count;

    size_t locals_size;
};

class InvalidFunctionDefinition : public VirtualMachineError {
public:
    explicit InvalidFunctionDefinition(const char *msg) : VirtualMachineError(msg) {}
};