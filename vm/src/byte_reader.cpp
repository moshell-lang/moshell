#include "byte_reader.h"

ByteReader::ByteReader(const char *bytes, size_t byte_count)
    : bytes{reinterpret_cast<const std::byte *>(bytes)}, byte_count{byte_count}, pos{0} {}

size_t ByteReader::position() const {
    return pos;
}
