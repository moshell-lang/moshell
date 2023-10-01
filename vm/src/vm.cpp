#include "vm.h"

#include "byte_reader.h"
#include "definitions/loader.h"
#include "definitions/pager.h"
#include "interpreter.h"
#include "memory/call_stack.h"
#include "memory/gc.h"
#include <iostream>

uint8_t moshell_value_get_as_byte(moshell_value val) {
    return val.b;
}
int64_t moshell_value_get_as_int(moshell_value val) {
    return val.i;
}
double moshell_value_get_as_double(moshell_value val) {
    return val.d;
}

moshell_object moshell_value_get_as_object(moshell_value val) {
    moshell_object_type type;
    msh::obj *obj = (msh::obj *)val.ptr;

    std::visit([&](auto &&data) {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, std::string>) {
            type = OBJ_STR;
        } else if constexpr (std::is_same_v<T, int64_t>) {
            type = OBJ_INT;
        } else if constexpr (std::is_same_v<T, double>) {
            type = OBJ_DOUBLE;
        } else if constexpr (std::is_same_v<T, msh::obj_vector>) {
            type = OBJ_VEC;
        } else {
            // unreachable
            static_assert(sizeof(T) != sizeof(T), "missing object variant");
        }
    },
               obj->get_data());

    return moshell_object{type, obj};
}

moshell_value moshell_object_unbox(moshell_object o) {
    const msh::obj *obj = static_cast<const msh::obj *>(o.val);
    return std::visit([&](auto &&data) -> moshell_value {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, int64_t>) {
            return {.i = data};
        } else if constexpr (std::is_same_v<T, double>) {
            return {.d = data};
        } else {
            std::cerr << "not an unboxable object";
            exit(1);
        }
    },
                      obj->get_data());
}

const char *moshell_object_get_as_string(moshell_object o) {
    msh::obj *obj = (msh::obj *)o.val;
    const char *str = obj->get<const std::string>().c_str();
    return str;
}
moshell_array moshell_object_get_as_array(moshell_object o) {
    msh::obj *obj = (msh::obj *)o.val;
    msh::obj_vector &vec = obj->get<msh::obj_vector>();
    moshell_value *data = (moshell_value *)vec.data();
    return moshell_array{
        vec.size(),
        data,
    };
}

struct vm_state {
    msh::loader loader;
    msh::pager pager;
    msh::heap heap;
    CallStack thread_stack{10000};
    msh::gc gc{heap, thread_stack, pager, loader};
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
    state->natives = load_natives();
    return {state};
}

int moshell_vm_register(moshell_vm vm, const char *bytes, size_t byte_count) {
    vm_state &state = *static_cast<vm_state *>(vm.vm);
    try {
        state.loader.load_raw_bytes(bytes, byte_count, state.pager, state.heap);
        return 0;
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
    }
    return -1;
}

moshell_value moshell_vm_get_exported(moshell_vm vm, const char *name, size_t len) {
    vm_state &state = *static_cast<vm_state *>(vm.vm);
    msh::exported_variable var = state.loader.get_exported(std::string(name, len));
    const void *obj = *state.pager.get_exported_value<const void *>(var);
    return {.ptr = obj};
}

int moshell_vm_run(moshell_vm vm) {
    vm_state &state = *static_cast<vm_state *>(vm.vm);
    try {
        state.loader.resolve_all(state.pager);
        const auto last = state.pager.cbegin() + (state.pager.size() - state.next_page);
        state.next_page = state.pager.size();
        for (auto it = state.pager.cbegin(); it != last; ++it) {
            const msh::memory_page &page = *it;
            if (!run_unit(state.thread_stack, state.loader, state.pager, page, {state.heap, state.gc}, state.natives)) {
                return 1;
            }
        }
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

gc_collection_result moshell_vm_gc_collect(moshell_vm vm) {
    vm_state &state = *static_cast<vm_state *>(vm.vm);

    std::vector<const msh::obj *> gc_collect = state.gc.collect();
    moshell_object *collected_objects = static_cast<moshell_object *>(malloc(sizeof(moshell_object) * gc_collect.size()));
    for (size_t i = 0; i < gc_collect.size(); i++) {
        const msh::obj *obj = gc_collect[i];
        // moshell values are either numbers or object pointers
        // we know the vector contains only objects
        // (as it is the collected objects from heap)
        collected_objects[i] = moshell_value_get_as_object({.ptr = obj});
    }
    return gc_collection_result{gc_collect.size(), collected_objects};
}

void moshell_vm_gc_run(moshell_vm vm) {
    vm_state &state = *static_cast<vm_state *>(vm.vm);
    state.gc.run();
}

void gc_collection_result_free(gc_collection_result res) {
    free(const_cast<moshell_object *>(res.collected_objects));
}