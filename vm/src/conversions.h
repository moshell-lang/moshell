#pragma once

#include <algorithm>

namespace msh {
    /**
     * Read a value from a byte array in network byte order (big endian) and convert it to host byte order.
     *
     * @tparam T The type of the value to read.
     * @param bytes The byte array to read from.
     * @return The value read.
     */
    template <typename T>
    T read_big_endian(const std::byte *bytes)
        requires std::is_trivial_v<T>
    {
        T val;
        std::reverse_copy(bytes, bytes + sizeof(T), reinterpret_cast<std::byte *>(&val));
        return val;
    }
}
