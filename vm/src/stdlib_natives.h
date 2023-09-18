#pragma once

#include "memory/operand_stack.h"
#include <string_view>
#include <unordered_map>

class runtime_memory;

using natives_functions_t = std::unordered_map<std::string_view, void (*)(OperandStack &, runtime_memory &)>;

natives_functions_t
load_natives();
