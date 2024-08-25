#include "pager.h"

namespace msh {
    size_t pager::push_page(memory_page page) {
        size_t index = pages.size();
        pages.push_back(std::move(page));
        return index;
    }

    size_t pager::push_pool(ConstantPool pool, size_t dynsym_size) {
        size_t index = pools.size();
        pools.push_back(std::move(pool));
        indexes.emplace_back(dynsym_size, dynsym{static_cast<void *>(nullptr)});
        return index;
    }

    const ConstantPool &pager::get_pool(size_t index) const {
        return pools.at(index);
    }

    pager::page_vector::iterator pager::begin() {
        return pages.begin();
    }

    pager::page_vector::iterator pager::end() {
        return pages.end();
    }

    pager::page_vector::const_iterator pager::cbegin() const {
        return pages.begin();
    }

    pager::page_vector::const_iterator pager::cend() const {
        return pages.end();
    }

    void pager::bind(size_t pool_index, size_t dynsym_id, exported_variable value) {
        void *ptr = &pages.at(value.page).bytes.at(value.offset);
        indexes.at(pool_index).at(dynsym_id) = ptr;
    }

    size_t pager::size() const {
        return pages.size();
    }
}
