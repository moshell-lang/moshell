#pragma once

#include <memory>
#include <vector>

#include "memory/strings.h"

class ByteReader;

/**
 * defines the type for an index inside the constant pool.
 * */
typedef uint32_t constant_index;

/**
 * Contains the constants defined in a module bytecode.
 * */
class ConstantPool {
    /**
     * The constant pool isn't the owner of its strings, so
     * it is simply an array of string pointers.
     */
    std::unique_ptr<const std::string *[]> constants;
    uint32_t size;

    explicit ConstantPool(uint32_t size);

    friend ConstantPool load_constant_pool(ByteReader &reader, strings_t &strings);

public:
    /**
     * get given string reference
     * @param at the constant's index to get
     * @returns a string reference of the string inside the vm's `strings_t` set
     * */
    const std::string &get_string(constant_index at) const;
};

/**
 * loads constant pool from given byte reader
 * @param reader the byte reader to read
 * @param strings all the read string constants are interned inside this `strings_t` argument
 * @throws InvalidBytecodeError if the reader reaches end of stream while reading the constant pool
 * */
ConstantPool load_constant_pool(ByteReader &reader, strings_t &strings);
