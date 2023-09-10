#pragma once

#include "definitions/function_definition.h"
#include "errors.h"
#include "memory/object.h"

#include <memory>

class ByteReader;

/**
 * defines the type for an index inside the constant pool.
 */
typedef uint32_t constant_index;

/**
 * Contains the string constants defined in a bytecode unit.
 * As the pool isn't the owner of its strings, it contains pointers to the bound `heap` object.
 */
class ConstantPool {
    std::unique_ptr<const msh::obj *[]> constants;
    /**
     * Number of constants in the pool
     */
    const uint32_t size;

    explicit ConstantPool(uint32_t size);

    friend ConstantPool load_constant_pool(ByteReader &reader, msh::heap &heap);

public:
    /**
     * get given string reference
     * @param at the constant's index to get
     * @returns a string reference of the string inside the vm's heap
     * @throws std::out_of_range if the given index is out of range
     */
    const std::string &get_string(constant_index at) const;

    const msh::obj &get_ref(constant_index at) const;
};

/**
 * loads constant pool from given byte reader
 * @param reader the byte reader to read
 * @param heap the heap to allocate strings in
 * @throws InvalidBytecodeError if the reader reaches end of stream while reading the constant pool
 */
ConstantPool load_constant_pool(ByteReader &reader, msh::heap &heap);
