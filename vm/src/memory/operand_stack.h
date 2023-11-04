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
    std::byte *bytes;
    size_t &current_pos;
    const size_t stack_capacity;
    std::vector<bool> &operands_refs;

    friend msh::gc;

public:
    OperandStack(char *buff, size_t &initial_pos, size_t stack_capacity, std::vector<bool> &operands_refs);

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
    void push_reference(msh::obj &r);

    void push_unchecked_reference(void *r);

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
    msh::obj &pop_reference();

    /**
     * pops `n` bytes
     * @throws OperandStackUnderflowError if the operand stack does not have enough bytes to pop
     */
    const std::byte *pop_bytes(size_t n);

    /**
     * transfer to this operand stack the n first bytes of the given caller stack.
     * The bytes are transferred without modifying the operands object refs vector
     * in order to keep the information over contained references
     *
     * The operand stacks must be adjacent in callstack (this = caller_stack, callee_stack the callee stack) otherwise this operation will result to undefined state of the callstack
     * */
    void transfer(OperandStack &callee_stack, size_t n);

    /**
     * Copies `size` bytes from the `src` position to the `dest` position.
     *
     * @param dest destination position to copy to
     * @param src source position to start the copy from
     * @param size amount of bytes to copy
     */
    void memmove(size_t dest, size_t src, size_t size);

    /**
     * Duplicates the quad-word on the top of the stack.
     */
    void dup_qword();

    /**
     * Swaps the two quad-words on the top of the stack.
     */
    void swap_upper_qwords();

    /**
     * Swaps the three quad-words on the top of the stack.
     */
    void swap_upper_three_qwords();

    template <typename T>
    void push(T t) {
        if (current_pos + sizeof(T) > stack_capacity) {
            throw StackOverflowError("exceeded stack capacity via operand stack");
        }

        size_t false_bits_count = sizeof(T);
        if constexpr (std::is_same_v<msh::obj, std::remove_cvref_t<std::remove_pointer_t<T>>>) {
            operands_refs.push_back(true);
            false_bits_count--;
        }
        operands_refs.resize(operands_refs.size() + false_bits_count, false);

        *(T *)(bytes + current_pos) = t;
        current_pos += sizeof(T);
    }

    template <typename T>
    T pop() {
        return *(T *)pop_bytes(sizeof(T));
    }

    template <typename T>
    T peek(size_t offset = 0) {
        return *(T *)(bytes + current_pos - offset);
    }
};

namespace msh {
    /**
     * RAII class to safely peek multiple values from the operand stack without compromising the reachability of the popped values.
     *
     * @tparam T the return type of the native procedure
     */
    template <typename T>
        requires std::is_same_v<T, std::byte> || std::is_same_v<T, int64_t> || std::is_same_v<T, double> || std::is_same_v<T, msh::obj *>
    class native_procedure {
        OperandStack &caller_stack;
        size_t offset{};

    public:
        explicit native_procedure(OperandStack &caller_stack)
            : caller_stack{caller_stack} {}

        msh::obj &pop_reference() {
            offset += sizeof(uint64_t);
            return *caller_stack.peek<msh::obj *>(offset);
        }

        ~native_procedure() {
            size_t size = sizeof(T);
            if constexpr (std::is_reference_v<T> || std::is_pointer_v<T>) {
                size = sizeof(uint64_t);
            }
            caller_stack.memmove(caller_stack.size() - offset, caller_stack.size() - size, size);
        }
    };
}
