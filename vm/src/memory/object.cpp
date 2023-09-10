#include "object.h"

namespace msh {
    msh::obj &heap::insert(msh::obj &&obj) {
        objects.push_front(std::forward<msh::obj>(obj));
        return objects.front();
    }
}
