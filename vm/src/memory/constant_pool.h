#pragma once

#include "errors.h"
#include "memory/strings.h"
#include <memory>
#include <vector>

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
};

class PoolConstantValue {
public:
    PoolConstantType type;
    explicit PoolConstantValue(PoolConstantType type);
};

class BadConstantType : public VirtualMachineError {
public:
    explicit BadConstantType(std::string msg): VirtualMachineError(msg) {}
};

/**
 * Contains the constants defined in a module bytecode.
 * */
class ConstantPool {
    std::vector<std::unique_ptr<const PoolConstantValue>> constants;

    explicit ConstantPool(uint32_t size);

    friend ConstantPool load_constant_pool(ByteReader& reader, strings_t& strings);
    /**
     * get given constant assuming it is of type `T`
     * @param pos the constant's index to get
     * @param type the expected type of the constant
     * @param type_name the type name of the constant
     * @throws BadConstantType if the value defined at `pos` does not matches the expected type
     * @returns a reference to the expected value inside the pool
     * */
    template <typename T>
    const T& get(constant_index pos, PoolConstantType type, const char *type_name) const;

public:

    /**
     * get given string reference
     * @param at the constant's index to get
     * @throws BadConstantType if the value is not a string
     * @returns a string reference of the string inside the vm's `strings_t` set
     * */
    const std::string& get_string(constant_index at) const;

};

/**
 * loads constant pool from given byte reader
 * @param reader the byte reader to read
 * @param strings all the read string constants are interned inside this `strings_t` argument
 * @throws InvalidBytecodeError if the reader reaches end of stream while reading the constant pool
 * */
ConstantPool load_constant_pool(ByteReader& reader, strings_t& strings);