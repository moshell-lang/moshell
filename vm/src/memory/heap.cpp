#include "heap.h"

namespace msh {

    obj_data &obj::get_data() {
        return data;
    }

    const obj_data &obj::get_data() const {
        return data;
    }

    obj &heap::insert(msh::obj &&obj) {
        objects.push_front(std::forward<msh::obj>(obj));
        len++;
        return objects.front();
    }

    size_t heap::size() const {
        return len;
    }
}
