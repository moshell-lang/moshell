#include "locals.h"
#include <cstring>
#include <string>

Locals::Locals(char *bytes, size_t capacity) : bytes{bytes}, capacity{capacity} {}

int64_t Locals::get_q_word(size_t at) const {
    return get<int64_t>(at);
}
char Locals::get_byte(size_t at) const {
    return get<char>(at);
}
uint64_t Locals::get_ref(uint64_t at) const {
    return get<uint64_t>(at);
}

char *Locals::reference(size_t at, size_t count) {
    check_capacity(at, count, "accessing");
    return bytes + at;
}

void Locals::set_q_word(int64_t i, size_t at) {
    set<int64_t>(i, at);
}
void Locals::set_byte(char b, size_t at) {
    set<char>(b, at);
}
void Locals::set_ref(uint64_t r, size_t at) {
    set<uint64_t>(r, at);
}
void Locals::set_bytes(const char *data, size_t size, size_t at) {
    check_capacity(at, size, "updating");
    memcpy(static_cast<void *>(bytes), data, size);
}

inline void Locals::check_capacity(size_t at, size_t space_size, std::string_view action) const {
    if (at + space_size > capacity) {
        throw LocalsOutOfBoundError("locals out of bound when " + std::string(action) + " value at index " + std::to_string(at));
    }
}

template <typename T>
T Locals::get(size_t at) const {
    check_capacity(at, sizeof(T), "accessing");
    return *(T *)(bytes + (at));
}

template <typename T>
void Locals::set(T t, size_t at) {
    check_capacity(at, sizeof(T), "updating");
    *(T *)(bytes + at) = t;
}