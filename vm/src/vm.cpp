#include "conversions.h"
#include "interpreter.h"
#include <cstring>
#include <iostream>

constant_pool load_constant_pool(const char *bytes, unsigned int *ip) {
    // Read the number of strings on a single byte
    char count = *(bytes + *ip);
    (*ip)++;
    // Allocate the constant pool
    constant_pool pool(count);
    // Read each string and store it in the constant pool
    // A string is an 8-byte length big endian followed by the string data without a null byte
    for (int i = 0; i < count; i++) {
        // Read the length
        size_t length = ntohl(*(int64_t *)(bytes + *ip));
        (*ip) += 8;

        // Allocate the string
        pool.strings.push_back(std::make_unique<char[]>(length));
        pool.sizes.push_back(length);
        // Read the string data
        memcpy(pool.strings[i].get(), bytes + *ip, length);
        (*ip) += length;
    }
    return pool;
}

extern "C" void exec(const char *bytes, size_t byte_count) {
    unsigned int constant_pool_bytes = 0;
    constant_pool pool = load_constant_pool(bytes, &constant_pool_bytes);

    try {
        run(std::move(pool), bytes + constant_pool_bytes, byte_count - constant_pool_bytes);
    } catch (std::exception &e) {
        std::cerr << e.what() << std::endl;
    }
}