#include "constant_pool.h"
#include "byte_reader.h"
#include "conversions.h"

const std::string *read_string(ByteReader &reader, strings_t &strings) {
    // Read the length
    uint32_t length = ntohl(reader.read<uint32_t>());

    // Allocate the string
    std::string str(reader.read_n<char>(length), length);

    auto [str_it, _] = strings.insert(std::make_unique<std::string>(std::move(str)));
    return str_it->get();
}

ConstantPool load_constant_pool(ByteReader &reader, strings_t &strings) {
    // Read the number of strings on a single byte
    uint32_t count = ntohl(reader.read<uint32_t>());

    ConstantPool pool(count);
    // Read each constant and store them in the constant pool
    for (uint32_t i = 0; i < count; i++) {
        pool.constants[i] = read_string(reader, strings);
    }
    return pool;
}

ConstantPool::ConstantPool(uint32_t size)
    : constants{std::make_unique<std::string const *[]>(size)},
      size{size} {}

const std::string &ConstantPool::get_string(constant_index at) const {
    if (at >= size) {
        throw std::out_of_range("index " + std::to_string(at) + " >= " + std::to_string(at));
    }
    return *constants[at];
}
