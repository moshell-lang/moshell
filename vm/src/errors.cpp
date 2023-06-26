#include "errors.h"

VirtualMachineError::VirtualMachineError(std::string msg) : msg{msg} {}
const char *VirtualMachineError::what() const noexcept {
    return msg.c_str();
}

