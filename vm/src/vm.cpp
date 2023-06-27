#include "byte_reader.h"
#include "interpreter.h"
#include "memory/constant_pool.h"
#include "memory/strings.h"
#include <iostream>

extern "C" void moshell_exec(const char *bytes, size_t byte_count) {
    strings_t strings;

    try {
        ByteReader reader(bytes, byte_count);
        ConstantPool pool = load_constant_pool(reader, strings);

        run(pool, bytes + reader.position(), byte_count - reader.position(), strings);
    } catch (std::exception e) {
        std::cerr << e.what() << '\n';
    }
}