#pragma once

#include "conversions.h"
#include <memory>
#include <vector>

#include "memory/constant_pool.h"
#include "memory/operand_stack.h"
#include "vm.h"

void run(const ConstantPool &pool, const char *bytes, size_t size, std::vector<std::string> &strings);
