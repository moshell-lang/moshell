#pragma once

#include <exception>

class VirtualMachineError: public std::exception {
    const char* msg;
public:
    explicit VirtualMachineError(const char *msg);

    const char *what() const _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_NOTHROW override;
};

class MemoryError: public VirtualMachineError {
public:
    explicit MemoryError(const char *msg): VirtualMachineError(msg) {}
};

