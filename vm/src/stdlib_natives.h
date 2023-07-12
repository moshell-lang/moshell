#include "interpreter.h"
#include "memory/operand_stack.h"
#include <string>
#include <unordered_map>

std::unordered_map<const std::string *, void (*)(OperandStack &, runtime_state &)>
load_natives(StringsHeap &strings);