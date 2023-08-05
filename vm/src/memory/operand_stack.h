#pragma once

#include "constant_pool.h"
#include "errors.h"
#include <cstddef>
#include <cstdint>
#include <exception>
#include <memory>
#include <stdexcept>

/**
 * thrown when the operand stack does not have enough data to pop requested value
 */
struct OperandStackUnderflowError : public MemoryError {

public:
    explicit OperandStackUnderflowError(std::string message) : MemoryError{std::move(message)} {}
    const char *name() const noexcept override {
        return "OperandStackUnderflowError";
    }
};

class OperandStack {
private:
    char *const bytes;
    size_t &current_pos;
    const size_t stack_capacity;

public:
    explicit OperandStack(char *const buff, size_t &initial_pos, size_t stack_capacity);

    /**
     * @return the size in bytes of the operand stack
     */
    size_t size() const;

    /**
     * @return the capacity in bytes of the operand stack
     */
    size_t get_capacity() const;

    /**
     * @throws StackOverflowError if the operand stack would overflow by pushing the quad-word
     */
    void push_int(int64_t i);

    /**
     * @throws StackOverflowError if the operand stack would overflow by pushing the byte
     */
    void push_byte(int8_t b);

    /**
     * @throws StackOverflowError if the operand stack would overflow by pushing the quad-word
     */
    void push_double(double d);

    /**
     * @throws StackOverflowError if the operand stack would overflow by pushing the reference
     */
    void push_reference(uint64_t r);

    /**
     * @return popped quad-word as an integer
     * @throws OperandStackUnderflowError if the operand stack does not have enough bytes to pop a quad-word
     */
    int64_t pop_int();

    /**
     * @return popped byte
     * @throws OperandStackUnderflowError if the operand stack does not have enough bytes to pop a byte
     */
    int8_t pop_byte();

    /**
     * @return popped quad-word as a double
     * @throws OperandStackUnderflowError if the operand stack does not have enough bytes to pop a quad-word
     */
    double pop_double();

    /**
     * @return popped reference
     * @throws OperandStackUnderflowError if the operand stack does not have enough bytes to pop a reference
     */
    uint64_t pop_reference();

    /**
     * pops `n` bytes
     * @throws OperandStackUnderflowError if the operand stack does not have enough bytes to pop
     */
    const char *pop_bytes(size_t n);

    void push(const char *bytes, size_t size);

    template <typename T>
    void push(T t) {
        if (current_pos + sizeof(T) > stack_capacity) {
            throw StackOverflowError("exceeded stack capacity via operand stack");
        }
        *(T *)(bytes + current_pos) = t;
        current_pos += sizeof(T);
    }

    template <typename T>
    T pop() {
        pop_bytes(sizeof(T));
        return *(T *)(bytes + current_pos);
    }
};
