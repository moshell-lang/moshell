#include "types.h"

/**
 * maps name inputs to its `Type` variant
 * */
Type get_type(const std::string_view &name) {
    if (name == "int") {
        return Type::INT;
    }
    if (name == "void") {
        return Type::VOID;
    }
    if (name == "float") {
        return Type::FLOAT;
    }
    if (name == "byte") {
        return Type::BYTE;
    }
    if (name == "string") {
        return Type::STRING;
    }
    throw NoSuchTypeError("Unknown type " + std::string(name));
}