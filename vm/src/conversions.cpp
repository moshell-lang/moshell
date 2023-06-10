#include <valarray>
#include <cstdio>
#include <memory>
#include "conversions.h"

int64_t ntohl(int64_t const net) {
    int64_t host = 0;
    for (int i = 0; i < 8; i++) {
        host <<= 8;
        host |= (net >> (i * 8)) & 0xFF;
    }
    return host;
}

std::unique_ptr<char[]> to_str(int64_t i, size_t &str_len) {
    str_len = (size_t) ((ceil(log10((double) i)) + 1) * sizeof(char));
    std::unique_ptr<char[]> str = std::make_unique<char[]>(str_len + 1);
    sprintf(str.get(), "%ld", i);
    return str;
}