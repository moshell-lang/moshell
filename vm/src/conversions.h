#pragma once

#include <cstdint>

int64_t ntohl(int64_t net);

std::unique_ptr<char[]> to_str(int64_t i, size_t &str_len);