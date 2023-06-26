#pragma once

#include <exception>
#include <string>

/**
 * Base class for fatal errors that occurs inside the virtual machine
 * */
class VirtualMachineError: public std::exception {
    const std::string msg;
public:
    explicit VirtualMachineError(std::string msg);

    const char *what() const _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_NOTHROW override;
};

/**
 * VM Error subdivision for any error implied by bytecode interpretation
 * */
class InvalidBytecodeError : public VirtualMachineError {
public:
    explicit InvalidBytecodeError(std::string msg): VirtualMachineError(msg) {}
};

/**
 * VM Error subdivision for any error related to VM's memory
 * */
class MemoryError: public VirtualMachineError {
public:
    explicit MemoryError(std::string msg): VirtualMachineError(msg) {}
};

