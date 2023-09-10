#pragma once

#include "memory/operand_stack.h"
#include <string_view>
#include <unordered_map>

using natives_functions_t = std::unordered_map<std::string_view, void (*)(OperandStack &, msh::heap &)>;

natives_functions_t
load_natives(msh::heap &heap);
