#include "errors.h"

VirtualMachineError::VirtualMachineError(std::string msg) : msg{std::move(msg)} {}
const char *VirtualMachineError::what() const noexcept {
    return msg.c_str();
}
