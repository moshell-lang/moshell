#pragma once

#include <cstdint>

#include "definitions/module_definition.h"
#include "memory/call_stack.h"
#include "memory/constant_pool.h"
#include "memory/operand_stack.h"
#include "vm.h"


struct runtime_state {
    strings_t &strings;

    const std::unordered_map<constant_index, function_definition> &functions;
    const ConstantPool &pool;
};

int run_module(const module_definition &module_def, strings_t &strings);


