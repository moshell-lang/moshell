#pragma once

#include "errors.h"
#include <string>

class NoSuchTypeError : public VirtualMachineError {
public:
    explicit NoSuchTypeError(std::string msg) : VirtualMachineError(msg) {}
};

enum class Type {
    INT,
    FLOAT,
    BYTE,
    VOID,

    STRING
};

Type get_type(const std::string_view &name);