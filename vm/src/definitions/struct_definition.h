#pragma once

#include <string>
#include <vector>

namespace msh {

/**
 * Contains all the information about a structure
 */
struct struct_definition {
    /**
     * structure's string identifier
     */
    std::string_view identifier;
    /**
     * bytes allocated in heap
     */
    size_t heap_size;
    /**
     * object references offsets in the structure's heap
     */
    std::vector<size_t> obj_ref_offsets;
};

}