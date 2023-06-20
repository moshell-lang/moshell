#pragma once

#include "conversions.h"
#include <memory>
#include <vector>

#include "definitions/module_definition.h"
#include "memory/call_stack.h"
#include "memory/constant_pool.h"
#include "memory/operand_stack.h"
#include "vm.h"


struct runtime_state {
    std::vector<std::string> &strings;

    const std::unordered_map<constant_index, function_definition> &functions;
    const ConstantPool &pool;
};



int run_module(const module_definition &module_def, std::vector<std::string> &strings);


