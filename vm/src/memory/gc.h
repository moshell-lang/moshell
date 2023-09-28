#pragma once

#include "call_stack.h"
#include "definitions/pager.h"
#include "memory/heap.h"
#include <unordered_set>

namespace msh {

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
         * to remove detached objects from heap.
         * */
        void run();

        /**
         * Run a new Garbage Collection cycle
         * but instead of removing detached heap objects,
         * collect their references and return a vector of those objects pointers
         *
         * Returned object pointers will be invalidated on the next gc run
         * */
        std::vector<const msh::obj *> collect();
    };
}