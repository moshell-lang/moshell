#include "constant_pool.h"
#include "byte_reader.h"

static const msh::obj &read_string(ByteReader &reader, msh::heap &heap) {
    // Read the length
    uint64_t length = reader.read<uint64_t>();

    // Allocate the string
    std::string str(reader.read_n<char>(length), length);

    return heap.insert(std::move(str));
}

ConstantPool load_constant_pool(ByteReader &reader, msh::heap &heap) {
    // Read the number of strings on four bytes
    uint32_t count = reader.read<uint32_t>();

    ConstantPool pool(count);
    // Read each constant and store them in the constant pool
    for (uint32_t i = 0; i < count; i++) {
        pool.constants[i] = &read_string(reader, heap);
    }
    return pool;
}

ConstantPool::ConstantPool(uint32_t size)
    : constants{std::vector<const msh::obj *>(size)},
      size{size} {}

const msh::obj &ConstantPool::get_ref(constant_index at) const {
    if (at >= size) {
        throw std::out_of_range("get string at index " + std::to_string(at) + " exceeds constant pool size (" + std::to_string(size) + ")");
    }
    return *constants[at];
}

const std::string &ConstantPool::get_string(constant_index at) const {
    return get_ref(at).get<std::string>();
}
