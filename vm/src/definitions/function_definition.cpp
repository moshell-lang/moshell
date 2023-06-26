#include "function_definition.h"

#include <utility>


void validate_signature(const function_signature &signature) {
    for (Type param : signature.params) {
        if (param == Type::VOID) {
            throw InvalidFunctionDefinition("Function signature contains void parameters");
        }
    }
}

function_signature::function_signature(std::string name, std::vector<Type> params, Type return_type)
    : name{std::move(name)}, params{std::move(params)}, return_type{return_type} {}

