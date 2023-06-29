#pragma once

#include <cstddef>
#include <stdexcept>

/**
 * reader utility class to read fixed byte arrays
 */
class ByteReader {
    const char *bytes;
    const size_t byte_count;
    size_t pos;

public:
    ByteReader(const char *bytes, size_t byte_count);

    ByteReader(const ByteReader &other) = delete;
    ByteReader &operator=(const ByteReader &other) = delete;

    /// returns current byte position in given byte array
    size_t position();

    template <typename T>
    T &read() {
        return *read_n<T>(1);
    }

    template <typename T>
    T *read_n(size_t n) {
        if (pos + sizeof(T) * n > byte_count)
            throw std::out_of_range("Cannot read more bytes: Byte Reader ran out of bytes");

        T *val = (T *)(bytes + pos);
        pos += sizeof(T) * n;
        return val;
    }
};
