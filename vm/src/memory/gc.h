#pragma once

#include "call_stack.h"
#include "definitions/pager.h"
#include "memory/heap.h"
#include <unordered_set>

namespace msh {
    struct gc_collect {
        size_t object_count;
        const msh::obj **object_refs;
    };

#ifndef NDEBUG
    /**
     * Force disable gc debug logs
     * */
    // Interpreter will call this function for any fork created,
    // as the master process already uses the debug file (if debug is enabled)
    void disable_gc_debug();
#endif

    /**
     * Garbage Collector
     * */
    class gc {
        heap &heap_space;
        CallStack &thread_call_stack;
        const pager &pages;
        const loader &ldr;
        size_t last_roots_size;

        /**
         * The GC cycle, incremented each time it performs a garbage collection
         * */
        uint8_t cycle;

        void scan_exported_vars(std::vector<const msh::obj *> &roots);
        void scan_constants(std::vector<const msh::obj *> &roots);
        void scan_thread(std::vector<const msh::obj *> &roots);
        void walk_objects(std::vector<const msh::obj *> to_visit);

        void scan();

    public:
        gc(heap &heap_space, CallStack &thread_stack, const pager &pages, const loader &ldr);

        /**
         * Run a new Garbage Collection cycle
         * and place freed objects in given result if non-null
         * */
        void run();

        gc_collect collect();
    };
}