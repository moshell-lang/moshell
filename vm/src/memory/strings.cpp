#pragma once

#include "memory/strings.h"

const std::string &StringsHeap::insert(std::string str) {
    auto [it, _] = strings.insert(std::make_unique<std::string>(str));
    return *it->get();
}