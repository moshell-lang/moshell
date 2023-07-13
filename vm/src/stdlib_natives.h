#pragma once

#include "memory/operand_stack.h"
#include <string>
#include <unordered_map>

typedef std::unordered_map<const std::string *, void (*)(OperandStack &, StringsHeap &)> natives_functions_t;

natives_functions_t
load_natives(StringsHeap &strings);