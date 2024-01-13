#include "locals.h"
#include <cstring>
#include <string>

Locals::Locals(std::byte *bytes, size_t capacity) : bytes{bytes}, capacity{capacity} {}

int64_t Locals::get_q_word(size_t at) const {
    return *get<int64_t>(at);
}

uint8_t Locals::get_byte(size_t at) const {
    return *get<char>(at);
}

void Locals::set_q_word(int64_t i, size_t at) {
    set<int64_t>(i, at);
}
void Locals::set_byte(uint8_t b, size_t at) {
    set<char>(b, at);
}

uint8_t &Locals::reference(size_t at) {
    check_capacity(at, 0, "accessing");
    return reinterpret_cast<uint8_t &>(*(bytes + at));
}
