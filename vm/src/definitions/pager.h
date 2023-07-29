#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "loader.h"
#include "memory/constant_pool.h"

namespace msh {
    using dynsym = std::variant<void *, function_definition *>;

    /**
     * A virtual memory page, that contain the global variable values.
     */
    struct memory_page {
        /**
         * The stored memory content of the page.
         */
        std::vector<char> bytes;

        /**
         * The name of the function that initializes the page.
         */
        std::string init_function_name;
    };

    /**
     * Loaded memory pages, once fetched by the loader.
     *
     * The pager holds static values that should be kept in memory for the whole
     * execution of the program.
     */
    class pager {
        using page_vector = std::vector<memory_page>;

        using index_vector = std::vector<dynsym>;

        /**
         * The pages of memory that have been loaded.
         */
        page_vector pages;

        /**
         * The different constant pools that have been loaded.
         *
         * Multiple pages can share the same constant pool.
         */
        std::vector<ConstantPool> pools;

        /**
         * The dynamic symbols that have been loaded.
         */
        std::vector<index_vector> indexes;

    public:
        /**
         * Adds a new page to be kept around.
         *
         * @param page The page to add.
         * @return The index of the page in this pager.
         */
        size_t push_page(memory_page page);

        /**
         * Indexes a new constant pool.
         *
         * The strings are not copied, but referenced. The heap that contains the strings
         * must be kept around for the whole execution of this pager.
         *
         * @param pool The constant pool to add.
         * @param size The number of dynamic symbols in the pool.
         * @return The index of the pool in this pager.
         */
        size_t push_pool(ConstantPool pool, size_t size);

        /**
         * Gets the page at the given index.
         *
         * @param index The index of the page to get.
         * @return The page at the given index.
         */
        const ConstantPool &get_pool(size_t index) const;

        page_vector::reverse_iterator begin();

        page_vector::reverse_iterator end();

        page_vector::const_reverse_iterator cbegin() const;

        page_vector::const_reverse_iterator cend() const;

        /**
         * Binds the given exported variable to the given dynamic symbol.
         *
         * @param pool_index The index of the pool containing the dynamic symbol.
         * @param dynsym_id The index of the dynamic symbol to link.
         * @param value The actual value to bind.
         */
        void bind(size_t pool_index, size_t dynsym_id, exported_variable value);

        /**
         * Gets the value of the given exported variable.
         *
         * @tparam T The type of the value to get.
         * @param exported The exported variable to get.
         * @return The value of the exported variable.
         */
        template <typename T>
        T get(size_t pool_index, size_t dynsym_index) const {
            dynsym location = indexes.at(pool_index).at(dynsym_index);
            if (std::holds_alternative<void *>(location)) {
                return *((T *)std::get<void *>(location));
            }
            throw std::runtime_error("Cannot get function as variable");
        }

        /**
         * Sets the value of the given exported variable.
         *
         * @tparam T The type of the value to set.
         * @param exported The exported variable to set.
         * @param value The value to set.
         */
        template <typename T>
        void set(size_t pool_index, size_t dynsym_index, T value) const {
            dynsym location = indexes.at(pool_index).at(dynsym_index);
            if (std::holds_alternative<void *>(location)) {
                *((T *)std::get<void *>(location)) = value;
                return;
            }
            throw std::runtime_error("Cannot get function as variable");
        }
    };
}
