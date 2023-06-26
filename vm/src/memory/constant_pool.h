#pragma once

#include "definitions/function_definition.h"
#include "errors.h"
#include "strings.h"
#include <memory>
#include <vector>

enum PoolConstantType {
    C_STR,
    C_SIGNATURE
};

class PoolConstantValue {
public:
    PoolConstantType type;
    explicit PoolConstantValue(PoolConstantType type) : type{type} {}
};

class BadConstantType : public MemoryError {
public:
    explicit BadConstantType(const char* msg): MemoryError(msg) {}
};


typedef uint32_t constant_index;

class ConstantPool {
    std::shared_ptr<std::unique_ptr<const PoolConstantValue>[]> constants;
    const uint32_t size;

    explicit ConstantPool(uint32_t size);

    friend ConstantPool load_constant_pool(const char *bytes, size_t &ip, strings_t& strings);

    template <typename T>
    [[nodiscard]] const T& get(constant_index at, PoolConstantType type, const char *type_name) const;

public:
    [[nodiscard]] const function_signature& get_signature(constant_index at) const;
    [[nodiscard]] const std::string& get_string(constant_index at) const;

    bool is_of_type(PoolConstantType type, constant_index at) const;

};

ConstantPool load_constant_pool(const char *bytes, size_t &ip, strings_t& strings);