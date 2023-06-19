#pragma once

#include <exception>

class VirtualMachineError: public std::exception {
    const char* msg;
public:
    explicit VirtualMachineError(const char *msg);
};

class MemoryError: public VirtualMachineError {
public:
    explicit MemoryError(const char *msg): VirtualMachineError(msg) {}
};

class StackOverflowError: public MemoryError {
public:
    explicit StackOverflowError(const char *msg): MemoryError(msg) {}
};

