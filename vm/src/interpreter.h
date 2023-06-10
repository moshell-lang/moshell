#pragma once

#include <cstddef>
#include <memory>
#include <vector>

#include "vm.h"

struct constant_pool {
    std::vector<std::unique_ptr<char[]>> strings;
    std::vector<size_t> sizes;

    explicit constant_pool(int capacity);
};


void run(constant_pool pool, int ip, const char *bytes, size_t size);
