#pragma once

#include "memory/constant_pool.h"
#include <cstddef>
#include <cstdint>

void run(const ConstantPool &pool, const char *bytes, size_t size, strings_t &strings);
