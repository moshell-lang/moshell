#pragma once

#include <algorithm>
#include <cstddef>
#include <stdexcept>
#include <type_traits>

/**
 * reader utility class to read fixed byte arrays
 */
class ByteReader {
    const std::byte *bytes;
    const size_t byte_count;
    size_t pos;

public:
    ByteReader(const std::byte *bytes, size_t byte_count);

    ByteReader(const ByteReader &other) = delete;
    ByteReader &operator=(const ByteReader &other) = delete;

    /**
     * returns current byte position in given byte array
     */
    size_t position() const;

    template <typename T>
    T read()
        requires std::is_trivial_v<T>
    {
        if (pos + sizeof(T) > byte_count) {
            throw std::out_of_range("Cannot read more bytes: Byte Reader ran out of bytes");
        }
        T val;
        std::reverse_copy(bytes + pos, bytes + pos + sizeof(T), reinterpret_cast<std::byte *>(&val));
        pos += sizeof(T);
        return val;
    }

    template <typename T>
    T *read_n(size_t n)
        requires std::is_trivial_v<T>
    {
        if (pos + sizeof(T) * n > byte_count) {
            throw std::out_of_range("Cannot read more bytes: Byte Reader ran out of bytes");
        }

        T *val = (T *)(bytes + pos);
        pos += sizeof(T) * n;
        return val;
    }
};
