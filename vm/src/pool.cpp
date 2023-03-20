#include "pool.h"

#include <cstring>

constant_pool::constant_pool(int capacity) {
    strings.reserve(capacity);
    sizes.reserve(capacity);
}

int64_t constant_pool::concat(int64_t lhs, int64_t rhs) {
    size_t lhs_len = sizes[lhs];
    size_t rhs_len = sizes[rhs];
    size_t str_len = lhs_len + rhs_len;
    std::unique_ptr<char[]> str = std::make_unique<char[]>(str_len);
    memcpy(str.get(), strings[lhs].get(), lhs_len);
    memcpy(str.get() + lhs_len, strings[rhs].get(), rhs_len);
    size_t id = strings.size();
    strings.push_back(std::move(str));
    sizes.push_back(str_len);
    return static_cast<int64_t>(id);
}
