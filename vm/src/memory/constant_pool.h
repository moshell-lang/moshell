#pragma once

#include "definitions/function_definition.h"
#include "errors.h"
#include "memory/strings.h"

#include <memory>

class ByteReader;

/**
 * defines the type for an index inside the constant pool.
 */
typedef uint32_t constant_index;

/**
 * Contains the constants defined in a module bytecode.
 */
class ConstantPool {
    std::unique_ptr<std::string const *[]> constants;
    const uint32_t size;

    explicit ConstantPool(uint32_t size);

    friend ConstantPool load_constant_pool(ByteReader &reader, strings_t &strings);

public:
    /**
     * get given string constant
     * @param at the constant's index to get
     * @throws BadConstantType if the value is not a string
     * @returns a string slice of the string inside the pool
     */
    const std::string &get_string(constant_index at) const;
};

/**
 * loads constant pool from given byte reader
 * @param reader the byte reader to read
 * @param strings all the read string constants are interned inside this `strings_t` argument
 * @throws InvalidBytecodeError if the reader reaches end of stream while reading the constant pool
 */
ConstantPool load_constant_pool(ByteReader &reader, strings_t &strings);