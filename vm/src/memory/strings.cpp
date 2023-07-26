#include "memory/strings.h"

const std::string &StringsHeap::insert(std::string str) {
    auto [it, _] = strings.insert(std::make_unique<std::string>(std::move(str)));
    return **it;
}