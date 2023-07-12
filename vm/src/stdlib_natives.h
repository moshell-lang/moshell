#pragma once

#include "memory/operand_stack.h"
#include <string>
#include <unordered_map>

struct runtime_state;

typedef std::unordered_map<const std::string *, void (*)(OperandStack &, runtime_state &)> natives_functions_t;

natives_functions_t
load_natives(StringsHeap &strings);