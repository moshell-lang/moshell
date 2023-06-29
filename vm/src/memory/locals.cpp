#include "locals.h"
#include <string>

Locals::Locals(char *bytes, size_t capacity) : bytes{bytes}, capacity{capacity} {}

int64_t Locals::get_q_word(size_t at) const {
    return get<int64_t>(at);
}
char Locals::get_byte(size_t at) const {
    return get<char>(at);
}
size_t Locals::get_ref(size_t at) const {
    return get<size_t>(at);
}

void Locals::set_q_word(int64_t i, size_t at) {
    set<int64_t>(i, at);
}
void Locals::set_byte(char b, size_t at) {
    set<char>(b, at);
}
void Locals::set_ref(size_t r, size_t at) {
    set<size_t>(r, at);
}

template <typename T>
T Locals::get(size_t at) const {
    if (at + sizeof(T) > capacity) {
        throw LocalsOutOfBoundError("locals out of bound when accessing value at index " + std::to_string(at));
    }
    return *(T *)(bytes + (at));
}

template <typename T>
void Locals::set(T t, size_t at) {
    if (at + sizeof(T) > capacity) {
        throw LocalsOutOfBoundError("locals out of bound when updating value at index " + std::to_string(at));
    }
    *(T *)(bytes + at) = t;
}