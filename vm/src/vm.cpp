#include <cstring>
#include "interpreter.h"

int64_t ntohl(int64_t const net) {
    int64_t host = 0;
    for (int i = 0; i < 8; i++) {
        host <<= 8;
        host |= (net >> (i * 8)) & 0xFF;
    }
    return host;
}

constant_pool load_constant_pool(const char *bytes, int *ip) {
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

extern "C" void exec(const char* bytes, size_t byte_count) {
    int ip = 0;
    constant_pool pool = load_constant_pool(bytes, &ip);
    run(std::move(pool), ip, bytes, byte_count);
}