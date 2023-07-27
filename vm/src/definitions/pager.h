#pragma once

#include <cstddef>
#include <string>
#include <vector>

#include "loader.h"
#include "memory/constant_pool.h"

namespace msh {
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
         * @return The index of the pool in this pager.
         */
        size_t push_pool(ConstantPool pool);

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
         * Gets the value of the given exported variable.
         *
         * @tparam T The type of the value to get.
         * @param exported The exported variable to get.
         * @return The value of the exported variable.
         */
        template <typename T>
        T get(exported_variable exported) const {
            return *((T *)(pages.at(exported.page).bytes.data() + exported.offset));
        }

        /**
         * Sets the value of the given exported variable.
         *
         * @tparam T The type of the value to set.
         * @param exported The exported variable to set.
         * @param value The value to set.
         */
        template <typename T>
        void set(exported_variable exported, T value) const {
            *((T *)(pages.at(exported.page).bytes.data() + exported.offset)) = value;
        }
    };
}
