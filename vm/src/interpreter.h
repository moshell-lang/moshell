#pragma once

#include <cstddef>
#include <memory>
#include <vector>

struct constant_pool {
    std::vector<std::unique_ptr<char[]>> strings;
    std::vector<size_t> sizes;

    explicit constant_pool(int capacity);
};

constant_pool load_constant_pool(const char *bytes, int *ip);

void run(constant_pool pool, int ip, const char *bytes, size_t size);
