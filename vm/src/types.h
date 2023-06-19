#pragma once
#include "errors.h"
#include <string>


class NoSuchTypeError: public VirtualMachineError {
public:
    explicit NoSuchTypeError(const char *msg): VirtualMachineError(msg) {}
};

enum class Type {
    INT,
    FLOAT,
    BYTE,

    STRING
};

Type get_type(const std::string& name);