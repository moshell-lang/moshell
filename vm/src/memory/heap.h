#pragma once

#include <cstdint>
#include <forward_list>
#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace msh {

    struct struct_definition;

    // Create a recursive variant type by forward declaring the vector type.
    // Since C++17, `std::vector` doesn't require the type to be complete with
    // an appropriate allocator, and only a pointer is used here.

    class obj;

    /**
     * A vector of heap allocated objects.
     */
    struct obj_vector : public std::vector<obj *> {
        using std::vector<obj *>::vector;
    };

    struct obj_struct {
        const struct_definition *definition;
        std::vector<char> bytes;
    };

    using obj_data = std::variant<int64_t, int8_t, double, const std::string, obj_vector, obj_struct>;

    class gc;

    /**
     * A vm object that can be stored in the heap.
     */
    class obj {
        mutable uint8_t gc_cycle;
        obj_data data;

        friend gc;

    public:
        template <typename T>
        obj(T val) : gc_cycle{0}, data{std::move(val)} {}

        obj_data &get_data();
        const obj_data &get_data() const;

        template <typename T>
        T &get()
            requires(!std::is_const_v<T>)
        {
            return std::get<T>(data);
        }
        template <typename T>
        T &get() const
            requires std::is_const_v<T>
        {
            return std::get<T>(data);
        }
    };

    /**
     * A collection of objects that can be referenced by other objects.
     *
     * The VM keep track of all objects allocated in the heap.
     */
    class heap {
        /**
         * The allocated objects.
         *
         * A linked list is used to avoid invalidating references to objects when
         * inserting or removing new objects.
         */
        std::forward_list<obj> objects;

        /**
         * heap size
         * */
        size_t len;

        friend gc;

    public:
        /**
         * Inserts a new object in the heap.
         *
         * @param obj The object to insert.
         * @return A reference to this object, valid as long as the object is not deleted.
         */
        msh::obj &insert(msh::obj &&obj);

        size_t size() const;
    };
}
