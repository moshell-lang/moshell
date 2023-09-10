#pragma once

#include <cstdint>
#include <forward_list>
#include <string>
#include <variant>
#include <vector>

namespace msh {
    // Create a recursive variant type by forward declaring the vector type.
    // Since C++17, `std::vector` doesn't require the type to be complete with
    // an appropriate allocator, and only a pointer is used here.
    /**
     * A vector of heap allocated objects.
     */
    struct obj_vector;
    /**
     * An object of the language that can be stored in the heap.
     */
    using obj = std::variant<int64_t, double, const std::string, obj_vector>;
    struct obj_vector : public std::vector<obj *> {
        using std::vector<obj *>::vector;
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

    public:
        /**
         * Inserts a new object in the heap.
         *
         * @param obj The object to insert.
         * @return A reference to this object, valid as long as the object is not deleted.
         */
        msh::obj &insert(msh::obj &&obj);
    };
}
