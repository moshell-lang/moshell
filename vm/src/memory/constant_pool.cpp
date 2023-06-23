#include "constant_pool.h"
#include "conversions.h"
#include "types.h"
#include <iostream>
#include <utility>
#include "memory/strings.h"

template <typename T>
class PoolConstantWrappedValue : public PoolConstantValue {
public:
    const T value;
    explicit PoolConstantWrappedValue(PoolConstantType type, T value) : PoolConstantValue{type}, value{std::move(value)} {}
};

std::unique_ptr<PoolConstantWrappedValue<const std::string*>> read_string(const char *bytes, unsigned int &ip, strings_t& strings) {
    // A string is an 8-byte length big endian followed by the string data without a null byte

    // Read the length
    size_t length = ntohl(*(u_int64_t *)(bytes + ip));
    ip += 8;

    // Allocate the string
    std::string str(bytes + ip, length);

    auto [str_it, _] = strings.insert(std::make_unique<std::string>(str));
    ip += length;
    const std::string* pool_str = str_it->get();
    return std::make_unique<PoolConstantWrappedValue<const std::string*>>(PoolConstantWrappedValue(C_STR, pool_str));
}

std::unique_ptr<PoolConstantWrappedValue<function_signature>> read_signature(const char *bytes, unsigned int &ip, ConstantPool &pool) {
    // Read the function name
    const std::string &name = pool.get_string(ntohl(*(constant_index *)(bytes + ip)));
    ip += sizeof(constant_index);

    // Read the param count
    unsigned char param_count = bytes[ip];
    ip++;

    // Allocate the params vector
    std::vector<Type> params;
    params.reserve(param_count + 1);

    for (int p = 0; p < param_count + 1; p++) {
        auto constant_idx = ntohl(*(constant_index *)(bytes + ip));
        const std::string &param_type_name = pool.get_string(constant_idx);
        params.push_back(get_type(param_type_name));
        ip += sizeof(constant_index);
    }

    Type return_type = params.back();
    params.pop_back();
    function_signature signature(std::string(name), std::move(params), std::move(return_type));
    return std::make_unique<PoolConstantWrappedValue<function_signature>>(PoolConstantWrappedValue(C_SIGNATURE, signature));
}

ConstantPool load_constant_pool(const char *bytes, unsigned int &ip, strings_t& strings) {
    // Read the number of strings on a single byte
    char count = *(bytes + ip);
    ip++;

    ConstantPool pool(count);
    // Read each constant and store them in the constant pool
    for (int i = 0; i < count; i++) {
        // read the constant type
        char type = bytes[ip];
        ip++;

        switch (type) {
        case 1: {
            pool.constants[i] = read_string(bytes, ip, strings);
            break;
        }
        case 2: {
            pool.constants[i] = read_signature(bytes, ip, pool);
            break;
        }
        default: {
            std::cerr << "unknown constant type " << std::to_string((int)type) << std::endl;
            exit(1);
        }
        }
    }
    return pool;
}

template <typename T>
[[nodiscard]] const T &ConstantPool::get(constant_index at, PoolConstantType type, const char *type_name) const {
    const PoolConstantValue *value = constants.get()[at].get();
    if (value->type != type) {
        throw ConstantPoolAccessError(("attempted to access " + std::string(type_name) + " in constant pool at index " + std::to_string(at) + " which is not a string").c_str());
    }
    // we already checked that the constant value is of type PoolConstantWrappedValue<std::string>
    return static_cast<const PoolConstantWrappedValue<T> *>(value)->value;
}

ConstantPool::ConstantPool(uint32_t size)
    : constants{std::make_unique<std::unique_ptr<const PoolConstantValue>[]>(size)},
      size{size} {}

[[nodiscard]] const std::string &ConstantPool::get_string(constant_index at) const {
    return *get<const std::string*>(at, C_STR, "string");
}

const function_signature &ConstantPool::get_signature(constant_index at) const {
    return get<function_signature>(at, C_SIGNATURE, "function signature");
}

bool ConstantPool::is_of_type(PoolConstantType type, constant_index at) const {
    return at < size && constants.get()[at].get()->type == type;
}
