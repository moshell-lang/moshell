#include "locals.h"
#include <string>

Locals::Locals(char *bytes, size_t capacity) : bytes{bytes}, capacity{capacity} {}

int64_t Locals::get_int(size_t at) const {
    return get<int64_t>(at);
}
double Locals::get_double(size_t at) const {
    return get<double>(at);
}
char Locals::get_byte(size_t at) const {
    return get<char>(at);
}
size_t Locals::get_ref(size_t at) const {
    return get<size_t>(at);
}

void Locals::set_int(int64_t i, size_t at) {
    set<int64_t>(i, at);
}
void Locals::set_double(double d, size_t at) {
    set<double>(d, at);
}
void Locals::set_byte(char b, size_t at) {
    set<char>(b, at);
}
void Locals::set_ref(size_t r, size_t at) {
    set<size_t>(r, at);
}

template <typename T>
T Locals::get(size_t at) const {
    if (at * 8 >= capacity) {
        throw LocalsOutOfBoundError(("locals out of bound when setting value at " + std::to_string(at)).c_str());
    }
    return *(T *)(bytes + (at * 8));
}

template <typename T>
void Locals::set(T t, size_t at) {
    if (at * 8 >= capacity) {
        throw LocalsOutOfBoundError(("locals out of bound when setting value at " + std::to_string(at)).c_str());
    }
    *(T *)(bytes + at * 8) = t;
}