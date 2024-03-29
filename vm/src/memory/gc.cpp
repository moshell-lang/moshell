#include "memory/gc.h"

#ifndef NDEBUG
#include <chrono>
#include <fstream>
#include <iostream>
#endif

using namespace msh;

gc::gc(heap &heap_space, CallStack &thread_stack, const pager &pages, const loader &ldr)
    : heap_space{heap_space}, thread_call_stack{thread_stack}, pages{pages}, ldr{ldr}, last_roots_size{0}, cycle{0} {
}
#ifndef NDEBUG

std::ofstream get_debug_file() {
    char *env_val = getenv("GC_DEBUG_FILE");
    std::ofstream stream;

    if (env_val == nullptr)
        return stream;

    stream.open(env_val);
    if (!stream.is_open()) {
        std::cerr << "could not open debug file " << env_val << std::endl;
        exit(1);
    }
    return stream;
}

std::ofstream debug_file = get_debug_file();

void debug_obj_freed(msh::obj &obj) {
    if (!debug_file.is_open())
        return;

    debug_file << "GC: object freed - ";

    std::visit([&](auto &&data) {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, std::string>) {
            debug_file << "string \"" << data << "\"";
        } else if constexpr (std::is_same_v<T, int64_t>) {
            debug_file << "boxed int64 " << data;
        } else if constexpr (std::is_same_v<T, int8_t>) {
            debug_file << "boxed int8 " << data;
        } else if constexpr (std::is_same_v<T, double>) {
            debug_file << "boxed double " << data;
        } else if constexpr (std::is_same_v<T, int8_t>) {
            debug_file << "boxed byte " << std::to_string(data);
        } else if constexpr (std::is_same_v<T, obj_vector>) {
            debug_file << "vector len:" << data.size();
        } else if constexpr (std::is_same_v<T, obj_struct>) {
            debug_file << "structure " << data.definition->identifier;
        } else {
            // unreachable
            static_assert(sizeof(T) != sizeof(T), "non-exhaustive object visitor ");
        }
    },
               obj.get_data());

    debug_file << std::endl;
}

void gc_debug(std::string msg) {
    if (!debug_file.is_open())
        return;
    debug_file << "GC: " << msg << std::endl;
}

void msh::disable_gc_debug() {
    debug_file.close();
}

#endif

void gc::scan() {
    cycle++;

    std::vector<const msh::obj *> roots;
    roots.reserve(last_roots_size);

    scan_constants(roots);
    scan_exported_vars(roots);
    scan_thread(roots);

    last_roots_size = roots.size();

    walk_objects(std::move(roots));
}

void gc::run() {
    size_t last_roots_size = this->last_roots_size;

#ifndef NDEBUG
    gc_debug("-----------");
    gc_debug("Running cycle " + std::to_string(cycle + 1));
    int t0 = time(nullptr);
#endif

    scan();

#ifndef NDEBUG
    gc_debug(std::to_string(this->last_roots_size) + " roots found (last cycle: " + std::to_string(last_roots_size) + ")");
#endif

    size_t removed_object_count = heap_space.objects.remove_if([&](msh::obj &obj) {
        bool detached = obj.gc_cycle != cycle;
#ifndef NDEBUG
        if (detached)
            debug_obj_freed(obj);
#endif
        return detached;
    });

    this->heap_space.len -= removed_object_count;

#ifndef NDEBUG
    int t1 = time(nullptr);
    gc_debug("Removed " + std::to_string(removed_object_count) + " objects.");
    gc_debug("Cycle ended in " + std::to_string(t1 - t0) + "ms");
#endif
}

std::vector<const msh::obj *> gc::collect() {
    scan();
    std::vector<const msh::obj *> object_refs;
    for (const msh::obj &obj : heap_space.objects) {
        bool detached = obj.gc_cycle != cycle;
        if (detached) {
            object_refs.push_back(&obj);
        }
    }
    return object_refs;
}

void gc::walk_objects(std::vector<const msh::obj *> to_visit) {
    while (!to_visit.empty()) {
        const msh::obj *obj = to_visit.back();
        to_visit.pop_back();

        // object is already marked as present in this cycle
        if (!obj || obj->gc_cycle == cycle)
            continue;

        obj->gc_cycle = cycle;
        std::visit([&](auto &&obj) {
            using T = std::decay_t<decltype(obj)>;
            if constexpr (std::is_same_v<T, msh::obj_vector>) {
                for (msh::obj *item : obj) {
                    to_visit.push_back(item);
                }
            } else if constexpr (std::is_same_v<T, msh::obj_struct>) {
                const msh::struct_definition *def = obj.definition;
                for (size_t obj_offset : def->obj_ref_offsets) {
                    const msh::obj *attribute_obj = *(const msh::obj **)(obj.bytes.data() + obj_offset);
                    to_visit.push_back(attribute_obj);
                }
            }
        },
                   obj->data);
    }
}

void gc::scan_thread(std::vector<const msh::obj *> &roots) {
    for (stack_frame &frame : thread_call_stack) {
        const Locals &locals = frame.locals;
        // add locals roots
        for (size_t obj_ref_offset : frame.function.obj_ref_offsets) {
            msh::obj *obj_ref = *locals.get<msh::obj *>(obj_ref_offset);
            if (obj_ref != nullptr) { // might be not yet initialized
                roots.push_back(obj_ref);
            }
        }
    }

    size_t size = 0;
    if (!thread_call_stack.is_empty()) {
        stack_frame &frame = thread_call_stack.peek_frame();
        size = frame.operands.size();
    }
    for (size_t i = 0; i < size; ++i) {
        if (thread_call_stack.operands_refs_offsets[i]) {
            msh::obj *obj = *(msh::obj **)(thread_call_stack.tape.data() + i);
            if (obj != nullptr) { // might be not yet initialized
                roots.push_back(obj);
            }
            i += 7;
        }
    }
}

void gc::scan_exported_vars(std::vector<const msh::obj *> &roots) {
    for (auto it = ldr.exported_cbegin(); it != ldr.exported_cend(); ++it) {
        const exported_variable &exported = it->second;
        if (!exported.is_obj_ref) {
            continue;
        }
        msh::obj *obj = *(msh::obj **)(pages.pages[exported.page].bytes.data() + exported.offset);
        if (obj != nullptr) // might be not yet initialized
            roots.push_back(obj);
    }
}

void gc::scan_constants(std::vector<const msh::obj *> &roots) {
    for (const ConstantPool &pool : pages.pools) {
        const std::vector<const msh::obj *> &constants = pool.constants;
        roots.insert(roots.end(), constants.cbegin(), constants.cend());
    }
}