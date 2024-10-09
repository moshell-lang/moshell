#include "heap.h"

namespace msh {

    obj_data &obj::get_data() {
        return data;
    }

    const obj_data &obj::get_data() const {
        return data;
    }

    obj &heap::insert(obj_data &&obj) {
        objects.emplace_front(obj);
        len++;
        return objects.front();
    }

    size_t heap::size() const {
        return len;
    }
}
