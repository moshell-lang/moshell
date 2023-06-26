#include <unordered_set>
#include <memory>

/**
 * The `strings_t` type is a simple set that contains all the strings during the lifetime
 * of the virtual machine.
 * All string constants and runtime strings are interned in this set.
 * */
typedef std::unordered_set<std::unique_ptr<std::string>> strings_t;