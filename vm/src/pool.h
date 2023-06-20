#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "conversions.h"

/**
 * A string constant pool.
 */
struct constant_pool {
    std::vector<std::unique_ptr<char[]>> strings;
    std::vector<size_t> sizes;

    /**
     * Constructs a constant pool with the given capacity.
     *
     * @param capacity The number of strings the constant pool can hold.
     */
    explicit constant_pool(int capacity);

    /**
     * Concatenates two strings and adds the result to the constant pool.
     *
     * @param lhs The index of the first string in the constant pool.
     * @param rhs The index of the second string in the constant pool.
     * @return The index of the concatenated string in the constant pool.
     */
    int64_t concat(int64_t lhs, int64_t rhs);
};

/**
 * Converts a value to a string and adds it to the constant pool.
 *
 * @tparam T The type of the value.
 * @param value The value to convert.
 * @param pool The constant pool to add the string to.
 * @return The index of the string in the constant pool.
 */
template <typename T>
int64_t append_str_value(T value, constant_pool &pool) {
    size_t str_len;
    std::unique_ptr<char[]> value_str = to_str(value, str_len);

    // replace stack's integer value with a string reference
    size_t idx = pool.strings.size();
    // add the string in constant pools (burk)
    pool.strings.push_back(std::move(value_str));
    pool.sizes.push_back(str_len);
    return static_cast<int64_t>(idx);
}
