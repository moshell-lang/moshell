#include "constant_pool.h"
#include "conversions.h"
#include "types.h"
#include <iostream>
#include <utility>

template <typename T>
class PoolConstantWrappedValue : public PoolConstantValue {
public:
    const T value;
    explicit PoolConstantWrappedValue(PoolConstantType type, T value) : PoolConstantValue{type}, value{std::move(value)} {}
};

std::unique_ptr<PoolConstantWrappedValue<std::string>> read_string(const char *bytes, unsigned int &ip) {
    // A string is an 8-byte length big endian followed by the string data without a null byte

    // Read the length
    size_t length = ntohl(*(u_int64_t *)(bytes + ip));
    ip += 8;

    // Allocate the string
    std::string str(bytes + ip, length);

    ip += length;
    return std::make_unique<PoolConstantWrappedValue<std::string>>(PoolConstantWrappedValue(C_STR, str));
}

std::unique_ptr<PoolConstantWrappedValue<function_signature>> read_signature(const char *bytes, unsigned int &ip, ConstantPool &pool) {
    // Read the function name
    const std::string &name = pool.get_string(ntohl(*(u_int32_t *)(bytes + ip)));
    ip += 4;

    // Read the param count
    unsigned char param_count = bytes[ip];
    ip++;

    // Allocate the params vector
    std::vector<Type> params;
    params.reserve(param_count + 1);

    for (int p = 0; p < param_count + 1; p++) {
        auto constant_idx = ntohl(*(u_int32_t *)(bytes + ip));
        const std::string &param_type_name = pool.get_string(constant_idx);
        params.push_back(get_type(param_type_name));
        ip += 4;
    }

    Type return_type = params.back();
    params.pop_back();
    function_signature signature(name, params, return_type);
    return std::make_unique<PoolConstantWrappedValue<function_signature>>(PoolConstantWrappedValue(C_SIGNATURE, signature));
}

ConstantPool load_constant_pool(const char *bytes, unsigned int &ip) {
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
            pool.constants[i] = read_string(bytes, ip);
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
    return get<std::string>(at, C_STR, "string");
}

const function_signature &ConstantPool::get_signature(constant_index at) const {
    return get<function_signature>(at, C_SIGNATURE, "function signature");
}

bool ConstantPool::is_of_type(PoolConstantType type, constant_index at) const {
    return at < size && constants.get()[at].get()->type == type;
}
