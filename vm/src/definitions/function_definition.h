#pragma once

#include "types.h"
#include <vector>

/**
 * the signature of a moshell function
 * Signatures are used to distinguish functions interfaces.
 * */
struct function_signature {
    /// fully qualified name of the function
    const std::string name;

    /// the function parameter types
    const std::vector<Type> params;

    /// the function return type
    const Type return_type;

    explicit function_signature(std::string name, std::vector<Type> params, Type return_type);
};

/**
 * Contains the instructions, and the locals size of a function instruction set.
 * */
struct function_definition {
    size_t locals_size;
    size_t instruction_count;
    const char *instructions;
};

class InvalidFunctionDefinition : public VirtualMachineError {
public:
    explicit InvalidFunctionDefinition(std::string msg) : VirtualMachineError(msg) {}
};

/**
 * validates given function signature
 * ensures that the signature does not contains any void parameter.
 * @throws InvalidFunctionDefinition if the signature is not validated
 * */
void validate_signature(const function_signature &signature);