#include "conversions.h"

int64_t ntohl(int64_t const net) {
    int64_t host = 0;
    for (int i = 0; i < 8; i++) {
        host <<= 8;
        host |= (net >> (i * 8)) & 0xFF;
    }
    return host;
}
