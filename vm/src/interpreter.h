#pragma once

#include "conversions.h"
#include <memory>
#include <vector>

#include "memory/operand_stack.h"
#include "vm.h"

struct constant_pool {
    std::vector<std::unique_ptr<char[]>> strings;
    std::vector<size_t> sizes;

    explicit constant_pool(int capacity);
};

/// appends in pool given value converted to string,
/// And returns the new string constant identifier in pool
template <typename T>
int64_t append_str_value(T value, constant_pool &pool) {

    size_t str_len;
    std::unique_ptr<char[]> value_str = to_str(value, str_len);

    // replace stack's integer value with a string reference
    int64_t strings_local = pool.strings.size();
    // add the string in constant pools (burk)
    pool.strings.push_back(std::move(value_str));
    pool.sizes.push_back(str_len);
    return strings_local;
}

void run(constant_pool pool, const char *bytes, size_t size);
