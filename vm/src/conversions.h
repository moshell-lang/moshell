#pragma once

#include <cstdint>
#include <cstring>
#include <memory>
#include <string>

int64_t ntohl(int64_t net);

template <typename T>
std::unique_ptr<char[]> to_str(T v, size_t &str_len) {
    std::string str = std::to_string(v);
    str_len = str.length();
    std::unique_ptr<char[]> chars = std::make_unique<char[]>(str_len + 1);
    memcpy(chars.get(), str.data(), str_len);
    chars.get()[str_len] = '\0';
    return chars;
}