#pragma once


template<typename T>
T ntohl(T net) {
    T host = 0;
    for (size_t i = 0; i < sizeof(T); i++) {
        host <<= 8;
        host |= (net >> (i * 8)) & 0xFF;
    }
    return host;
}