#include "vm.h"

#include "byte_reader.h"
#include "definitions/loader.h"
#include "definitions/pager.h"
#include "interpreter.h"

#include <iostream>

struct vm_state {
    msh::loader loader;
    msh::pager pager;
    StringsHeap strings;
    natives_functions_t natives;
    size_t next_page{};
};

int moshell_exec(const char *bytes, size_t byte_count) {
    moshell_vm vm = moshell_vm_init();
    moshell_vm_register(vm, bytes, byte_count);
    int exit = moshell_vm_run(vm);
    moshell_vm_free(vm);
    return exit;
}

moshell_vm moshell_vm_init() {
    vm_state *state = new vm_state();
    state->natives = load_natives(state->strings);
    return {state};
}

void moshell_vm_register(moshell_vm vm, const char *bytes, size_t byte_count) {
    vm_state &state = *static_cast<vm_state *>(vm.vm);
    try {
        state.loader.load_raw_bytes(bytes, byte_count, state.pager, state.strings);
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
    }
}

int moshell_vm_run(moshell_vm vm) {
    vm_state &state = *static_cast<vm_state *>(vm.vm);
    try {
        state.loader.resolve_all(state.pager);
        auto last = state.pager.begin() + (state.pager.size() - state.next_page);
        for (auto it = state.pager.begin(); it != last; ++it) {
            msh::memory_page &page = *it;
            if (!run_unit(state.loader, state.pager, page, state.strings, state.natives)) {
                return 1;
            }
        }
        state.next_page = state.pager.size();
        return 0;
    } catch (const VirtualMachineError &e) {
        std::cerr << e.name() << ": " << e.what() << std::endl;
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
    }
    return -1;
}

size_t moshell_vm_next_page(moshell_vm vm) {
    vm_state &state = *static_cast<vm_state *>(vm.vm);
    return state.next_page;
}

void moshell_vm_free(moshell_vm vm) {
    delete static_cast<vm_state *>(vm.vm);
}
