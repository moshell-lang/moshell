#pragma once

#include <memory>
#include <vector>
#include "conversions.h"

#include "vm.h"

struct constant_pool {
    std::vector<std::unique_ptr<char[]>> strings;
    std::vector<size_t> sizes;

    explicit constant_pool(int capacity);
};


template<typename T>
void append_str_value(T value, int64_t *stack_local, constant_pool &pool) {

    size_t str_len;
    std::unique_ptr<char[]> value_str = to_str(value, str_len);

    // replace stack's integer value with a string reference
    *stack_local = (int64_t) pool.strings.size();
    // add the string in constant pools (burk)
    pool.strings.push_back(std::move(value_str));
    pool.sizes.push_back(str_len);
}

void run(constant_pool pool, int ip, const char *bytes, size_t size);
