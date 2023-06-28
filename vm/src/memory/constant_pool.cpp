#include "constant_pool.h"
#include "byte_reader.h"
#include "conversions.h"
#include "errors.h"

const std::string *read_string(ByteReader &reader, strings_t &strings) {
    // Read the length
    uint64_t length = ntohl(reader.read<uint64_t>());

    // Allocate the string
    std::string str(reader.read_n<char>(length), length);

    auto [str_it, _] = strings.insert(std::make_unique<std::string>(std::move(str)));
    return str_it->get();
}

ConstantPool load_constant_pool(ByteReader &reader, strings_t &strings) {
    try {
        // Read the number of strings on a single byte
        uint32_t count = ntohl(reader.read<uint32_t>());

        ConstantPool pool(count);
        // Read each constant and store them in the constant pool
        for (uint32_t i = 0; i < count; i++) {
            const std::string *value = read_string(reader, strings);
            pool.constants[i] = value;
        }
        return pool;
    } catch (std::out_of_range e) {
        throw InvalidBytecodeError("Error when reading module bytecode: " + std::string(e.what()));
    }
}

ConstantPool::ConstantPool(uint32_t size)
    : constants{std::make_unique<const std::string*[]>(size)},
      size{size} {}

const std::string &ConstantPool::get_string(constant_index at) const {
    if (at >= size) {
        throw std::out_of_range("get string at index " + std::to_string(at) + " exceeds constant pool size (" + std::to_string(size) + ")");
    }
    return *constants[at];
}