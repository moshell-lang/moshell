#include "byte_reader.h"
#include "definitions/loader.h"
#include "definitions/pager.h"
#include "interpreter.h"

#include <iostream>

extern "C" int moshell_exec(const char *bytes, size_t byte_count) {
    StringsHeap strings;
    msh::loader loader;
    msh::pager pager;
    try {
        natives_functions_t natives = load_natives(strings);
        loader.load_raw_bytes(bytes, byte_count, pager, strings);
        loader.resolve_all(pager);
        for (const auto &page : pager) {
            if (!run_unit(loader, pager, page, strings, natives))
                return 1;
        }
    } catch (const VirtualMachineError &e) {
        std::cerr << e.name() << ": " << e.what() << std::endl;
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
    }
    return 0;
}
