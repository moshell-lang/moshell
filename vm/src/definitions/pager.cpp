#include "pager.h"

namespace msh {
    size_t pager::push_page(memory_page page) {
        size_t index = pages.size();
        pages.push_back(std::move(page));
        return index;
    }

    size_t pager::push_pool(ConstantPool pool) {
        size_t index = pages.size();
        pools.push_back(std::move(pool));
        return index;
    }

    const ConstantPool &pager::get_pool(size_t index) const {
        return pools.at(index);
    }

    pager::page_vector::reverse_iterator pager::begin() {
        return pages.rbegin();
    }

    pager::page_vector::reverse_iterator pager::end() {
        return pages.rend();
    }

    pager::page_vector::const_reverse_iterator pager::cbegin() const {
        return pages.rbegin();
    }

    pager::page_vector::const_reverse_iterator pager::cend() const {
        return pages.rend();
    }
}
