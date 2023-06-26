#include "constant_pool.h"
#include "byte_reader.h"
#include "conversions.h"
#include "types.h"
#include <vector>

PoolConstantValue::PoolConstantValue(PoolConstantType type) : type{type} {}

template <typename T>
class PoolConstantWrappedValue : public PoolConstantValue {
public:
    const T value;
    explicit PoolConstantWrappedValue(PoolConstantType type, T value) : PoolConstantValue{type}, value{std::move(value)} {}
};

std::unique_ptr<PoolConstantWrappedValue<const std::string*>> read_string(ByteReader &reader, strings_t &strings) {
    // Read the length
    size_t length = ntohl(reader.read<size_t>());

    // Allocate the string
    std::string str(reader.read_n<char>(length), length);

    auto [str_it, _] = strings.insert(std::make_unique<std::string>(str));
    const std::string* pool_str = str_it->get();
    return std::make_unique<PoolConstantWrappedValue<const std::string*>>(PoolConstantWrappedValue(C_STR, pool_str));
}

std::unique_ptr<PoolConstantWrappedValue<function_signature>> read_signature(ByteReader &reader, ConstantPool &pool) {
    // Read the function name
    const std::string_view &name = pool.get_string(ntohl(reader.read<constant_index>()));

    // Read the param count
    unsigned char param_count = reader.read<char>();

    // Allocate the params vector
    std::vector<Type> params;
    params.reserve(param_count + 1);

    for (int p = 0; p < param_count + 1; p++) {
        auto constant_idx = ntohl(reader.read<constant_index>());
        const std::string_view &param_type_name = pool.get_string(constant_idx);
        params.push_back(get_type(param_type_name));
    }

    Type return_type = params.back();
    params.pop_back();
    function_signature signature(std::string(name), std::move(params), std::move(return_type));
    return std::make_unique<PoolConstantWrappedValue<function_signature>>(PoolConstantWrappedValue(C_SIGNATURE, signature));
}

ConstantPool load_constant_pool(ByteReader &reader, strings_t &strings) {
    // Read the number of strings on a single byte
    char count = reader.read<char>();

    ConstantPool pool(count);
    // Read each constant and store them in the constant pool
    for (int i = 0; i < count; i++) {
        // read the constant type
        char type = reader.read<char>();

        switch (type) {
        case 1: {
            pool.constants[i] = read_string(reader, strings);
            break;
        }
        case 2: {
            pool.constants[i] = read_signature(reader, pool);
            break;
        }
        default: {
            throw InvalidBytecodeError("unknown constant type " + std::to_string((int)type) + " when reading constant pool");
        }
        }
    }
    return pool;
}


template <typename T>
const T &ConstantPool::get(constant_index pos, PoolConstantType type, const char *type_name) const {
    const PoolConstantValue *value = constants.get()[pos].get();
    if (value->type != type) {
        throw BadConstantType("attempted to access " + std::string(type_name) + " in constant pool pos index " + std::to_string(pos) + " which is not a string");
    }
    // we already checked that the constant value is of type PoolConstantWrappedValue<std::string>
    return static_cast<const PoolConstantWrappedValue<T> *>(value)->value;
}

ConstantPool::ConstantPool(uint32_t size)
    : constants{std::make_unique<std::unique_ptr<const PoolConstantValue>[]>(size)},
      size{size} {}

 const std::string &ConstantPool::get_string(constant_index at) const {
    return *get<const std::string*>(at, C_STR, "string");
}

const function_signature &ConstantPool::get_signature(constant_index at) const {
    return get<function_signature>(at, C_SIGNATURE, "function signature");
}

