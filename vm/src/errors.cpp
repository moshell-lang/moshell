#include "errors.h"

VirtualMachineError::VirtualMachineError(const char *msg) : msg{msg} {}
const char *VirtualMachineError::what() const noexcept {
    return msg;
}

