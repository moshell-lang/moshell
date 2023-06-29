#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>

/**
 * Contains the instructions, and the locals size of a function instruction set.
 */
struct function_definition {
    size_t locals_size;
    size_t parameters_byte_count;
    uint8_t return_byte_count;
    size_t instruction_count;
    const char *instructions;
};
