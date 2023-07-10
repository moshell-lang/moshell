#pragma once

#include "errors.h"
#include "function_definition.h"
#include "memory/constant_pool.h"
#include <unordered_map>

class ByteReader;

/**
 * Thrown when a bytecode gets invalidated during the loading phase
 */
class InvalidBytecodeStructure : public VirtualMachineError {
public:
    explicit InvalidBytecodeStructure(std::string msg) : VirtualMachineError(std::move(msg)) {}
    const char *name() const noexcept override {
        return "InvalidBytecodeStructure";
    }
};

struct bytecode_unit {
    /**
     * all the constants used by this bytecode unit.
     * The functions instructions of this unit always references to the indexes of this pool
     */
    const ConstantPool pool;
    /**
     * contains the functions declared in this unit
     */
    const std::unordered_map<const std::string *, function_definition> functions;
};

/**
 * Loads a bytecode unit, placing all string constants in the `strings` argument
 * @param reader the bytes to reader / load
 * @param strings the strings set where all the strings constants are interned.
 * @throws InvalidBytecodeError if the reader ran out of bytes while reading the bytecode
 * @throws InvalidBytecodeStructure if the constant pool or a function isn't validated
 */
bytecode_unit load_unit(ByteReader &reader, strings_t &strings);