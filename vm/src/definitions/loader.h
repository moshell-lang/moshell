#pragma once

#include <cstddef>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>

#include "memory/constant_pool.h"
#include "memory/heap.h"

class ByteReader;

namespace msh {
    class pager;

    struct unresolved_variable {
        size_t pool_index;
        size_t index;
        std::string symbol_name;
    };

    /**
     * The effective location in the virtual memory for a given exported symbol.
     */
    struct exported_variable {
        /**
         * The page the value it is stored in.
         */
        size_t page;

        /**
         * The offset in the page.
         */
        size_t offset;

        /**
         * GC hint to mark this variable as being a reference to a heap object
         * */
        bool is_obj_ref;
    };

    class loader {
        using function_map = std::unordered_map<std::string, function_definition>;
        using exported_variable_map = std::unordered_map<std::string, exported_variable>;

        /**
         * The functions that have been loaded.
         */
        function_map functions;

        /**
         * The effective locations in the loaded pages for each named symbol.
         */
        exported_variable_map exported;

        /**
         * The instructions bytes that have been loaded and concatenated.
         */
        std::vector<char> concatened_instructions;

        /**
         * The unresolved symbols that have been found and need to be resolved.
         */
        std::stack<unresolved_variable> unresolved;

        std::pair<const std::string &, const function_definition &> load_function(ByteReader &reader, const ConstantPool &pool, size_t pool_index);

    public:
        /**
         * Loads the given bytes and init the pager without running any function.
         *
         * @param bytes The bytes to load.
         * @param size The array size of the bytes.
         * @param pager The pager where to initialize the memory.
         * @param heap The heap heap where to store the constant strings.
         */
        void load_raw_bytes(const char *bytes, size_t size, pager &pager, msh::heap &heap);

        /**
         * Gets the function definition for the given name.
         *
         * @param name The name of the function.
         * @return The function definition.
         */
        const function_definition &get_function(const std::string &name) const;

        /**
         * Finds the function definition for the given name.
         *
         * @param name The name of the function.
         * @return An iterator to the function definition, or `functions_cend()` if not found.
         */
        function_map::const_iterator find_function(const std::string &name) const;

        /**
         * Gets the end iterator for the functions.
         *
         * @return The end iterator for the functions.
         */
        function_map::const_iterator functions_cend() const;

        exported_variable_map::const_iterator exported_cbegin() const;
        exported_variable_map::const_iterator exported_cend() const;

        /**
         * Gets the exported variable for the given name.
         *
         * @param name The name of the exported variable.
         * @return The exported variable.
         */
        const exported_variable &get_exported(const std::string &name) const;

        /**
         * Gets the instructions bytes for the given index.
         *
         * @param index The index of the instructions.
         * @return The instructions bytes.
         */
        const char *get_instructions(size_t index) const;

        /**
         * Resolves all the unresolved symbols.
         *
         * @param pager The pager where to resolve the symbols.
         * @throws std::runtime_error If any symbol cannot be resolved.
         */
        void resolve_all(pager &pager);
    };
}
