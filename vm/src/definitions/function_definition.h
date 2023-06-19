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
    const char* instructions;
    const size_t instruction_count;
    explicit function_definition(const char* instructions, size_t instruction_count);
};