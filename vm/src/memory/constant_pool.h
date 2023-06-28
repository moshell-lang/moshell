#pragma once

#include "definitions/function_definition.h"
#include "errors.h"
#include "memory/strings.h"

#include <memory>

class ByteReader;

/**
 * defines the type for an index inside the constant pool.
 * */
typedef uint32_t constant_index;

/**
 * Kind of constant that can be contained in the constant pool
 * */
enum PoolConstantType {
    /// an utf-8 string
    C_STR,
    /// a function signature
    C_SIGNATURE
};

class PoolConstantValue {
public:
    PoolConstantType type;
    explicit PoolConstantValue(PoolConstantType type);
};

class BadConstantType : public MemoryError {
public:
    explicit BadConstantType(std::string msg) : MemoryError(msg) {}
};

/**
 * Contains the constants defined in a module bytecode.
 * */
class ConstantPool {
    const std::shared_ptr<std::unique_ptr<const PoolConstantValue>[]> constants;
    const uint32_t size;

    explicit ConstantPool(uint32_t size);

    friend ConstantPool load_constant_pool(ByteReader &reader, strings_t &strings);
    /**
     * get given constant assuming it is of type `T`
     * @param pos the constant's index to get
     * @param type the expected type of the constant
     * @param type_name the type name of the constant
     * @throws BadConstantType if the value defined at `pos` does not matches the expected type
     * @returns a reference to the expected value inside the pool
     * */
    template <typename T>
    const T &get(constant_index pos, PoolConstantType type, const char *type_name) const;

public:
    /**
     * get given function signature constant
     * @param at the constant's index to get
     * @throws BadConstantType if the value is not a function signature
     * @returns a constant reference of the function signature inside the pool
     * */
    const function_signature &get_signature(constant_index at) const;
    /**
     * get given string constant
     * @param at the constant's index to get
     * @throws BadConstantType if the value is not a string
     * @returns a string slice of the string inside the pool
     * */
    const std::string &get_string(constant_index at) const;
};

/**
 * loads constant pool from given byte reader
 * @param reader the byte reader to read
 * @param strings all the read string constants are interned inside this `strings_t` argument
 * @throws InvalidBytecodeError if the reader reaches end of stream while reading the constant pool
 */
ConstantPool load_constant_pool(ByteReader &reader, strings_t &strings);