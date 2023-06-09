#pragma once

#include <cstddef>

struct constant_pool {
    char **strings;
    size_t *sizes;
};

constant_pool load_constant_pool(const char *bytes, int *ip);

void run(struct constant_pool pool, int ip, const char *bytes, size_t size);
