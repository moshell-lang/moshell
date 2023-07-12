#include "byte_reader.h"
#include "definitions/bytecode_unit.h"
#include "interpreter.h"

#include <iostream>

extern "C" void moshell_exec(const char *bytes, size_t byte_count) {
    StringsHeap strings;
    ByteReader reader(bytes, byte_count);
    try {
        // read function definitions
        auto module_def = load_unit(reader, strings);

        run_unit(module_def, strings, load_natives(strings));

    } catch (const VirtualMachineError &e) {
        std::cerr << e.name() << ": " << e.what() << std::endl;
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
    }
}
