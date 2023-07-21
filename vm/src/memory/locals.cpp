#include "locals.h"
#include <string>

Locals::Locals(char *bytes, size_t capacity) : bytes{bytes}, capacity{capacity} {}

char &Locals::reference(size_t at) {
    if (at > capacity) {
        throw LocalsOutOfBoundError("locals out of bound when accessing value reference at index " + std::to_string(at));
    }
    return *(bytes + at);
}
