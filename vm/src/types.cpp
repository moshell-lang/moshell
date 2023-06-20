#include "types.h"


Type get_type(const std::string& name) {
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
    if (name == "String") {
        return Type::STRING;
    }
    throw NoSuchTypeError(("Unknown type " + name).c_str());
}