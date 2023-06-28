#pragma once

#include <memory>
#include <unordered_set>

/**
 * The `strings_t` type is a simple set that contains all the strings during the lifetime
 * of the virtual machine.
 * All string constants and runtime strings are contained in this set.
 * */
typedef std::unordered_set<std::unique_ptr<std::string>> strings_t;