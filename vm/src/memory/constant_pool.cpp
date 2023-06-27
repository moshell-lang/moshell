#include "constant_pool.h"
#include "byte_reader.h"
#include "conversions.h"
#include "strings.h"
#include <vector>

PoolConstantValue::PoolConstantValue(PoolConstantType type) : type{type} {}

template <typename T>
class PoolConstantWrappedValue : public PoolConstantValue {
public:
    const T value;
    explicit PoolConstantWrappedValue(PoolConstantType type, T value) : PoolConstantValue{type}, value{std::move(value)} {}
};

std::unique_ptr<PoolConstantWrappedValue<const std::string *>> read_string(ByteReader &reader, strings_t &strings) {
    // Read the length
    uint64_t length = ntohl(reader.read<uint64_t>());

    // Allocate the string
    std::string str(reader.read_n<char>(length), length);

    auto [str_it, _] = strings.insert(std::make_unique<std::string>(std::move(str)));
    const std::string *pool_str = str_it->get();
    return std::make_unique<PoolConstantWrappedValue<const std::string *>>(PoolConstantWrappedValue(C_STR, pool_str));
}

ConstantPool load_constant_pool(ByteReader &reader, strings_t &strings) {
    try {
        // Read the number of strings on a single byte
        char count = reader.read<char>();

        ConstantPool pool(count);
        // Read each constant and store them in the constant pool
        for (int i = 0; i < count; i++) {
            std::unique_ptr<const PoolConstantValue> value = read_string(reader, strings);
            pool.constants[i] = std::move(value);
        }
        return pool;
    } catch (std::out_of_range e) {
        throw InvalidBytecodeError("Error when reading module bytecode: " + std::string(e.what()));
    }
}

template <typename T>
const T &ConstantPool::get(constant_index pos, PoolConstantType type, const char *type_name) const {
    const PoolConstantValue *value = constants.at(pos).get();
    if (value->type != type) {
        throw BadConstantType("attempted to access " + std::string(type_name) + " in constant pool at index " + std::to_string(pos) + " which is not a string");
    }
    // we already checked that the constant value is of type PoolConstantWrappedValue<std::string>
    return static_cast<const PoolConstantWrappedValue<T> *>(value)->value;
}

ConstantPool::ConstantPool(uint32_t size)
    : constants{std::vector<std::unique_ptr<const PoolConstantValue>>(size)} {}

const std::string &ConstantPool::get_string(constant_index at) const {
    return *get<const std::string *>(at, C_STR, "string");
}
