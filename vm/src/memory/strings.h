#pragma once

#include <memory>
#include <string>
#include <unordered_set>

struct UniquePtrEq {
    bool operator()(const std::unique_ptr<std::string> &a, const std::unique_ptr<std::string> &b) const {
        return *a == *b;
    }
};

struct UniquePtrHash {
    size_t operator()(const std::unique_ptr<std::string> &a) const {
        return std::hash<std::string>()(*a);
    }
};

/**
 * The `StringsHeap` is a simple set wrapper that contains all the strings during the lifetime
 * of the virtual machine.
 * All string constants and runtime strings are interned in this structure.
 */
class StringsHeap {
    std::unordered_set<std::unique_ptr<std::string>, UniquePtrHash, UniquePtrEq> strings;

public:
    StringsHeap() = default;

    StringsHeap(const StringsHeap &other) = delete;
    StringsHeap &operator=(const StringsHeap &other) = delete;

    const std::string &insert(std::string str);
};