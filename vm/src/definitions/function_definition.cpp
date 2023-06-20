#include "function_definition.h"

#include <utility>

function_signature::function_signature(std::string name, std::vector<Type> params, Type return_type)
    : name{std::move(name)}, params{std::move(params)}, return_type{return_type} {}

