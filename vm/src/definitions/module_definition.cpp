#include "module_definition.h"
#include "conversions.h"
#include "memory/constant_pool.h"
#include <unordered_map>

std::unordered_map<constant_index, function_definition>
load_functions_definitions(const char *bytes, unsigned int &ip, const ConstantPool &pool) {

    std::unordered_map<constant_index, function_definition> map;

    u_int32_t function_count = ntohl(*(u_int32_t *)(bytes + ip));
    ip += 4;

    for (u_int32_t i = 0; i < function_count; i++) {
        constant_index signature = ntohl(*(u_int32_t *)(bytes + ip));

        if (!pool.is_of_type(C_SIGNATURE, signature)) {
            throw InvalidModuleDescription("invalid function declaration signature: the constant index read at function declaration does not points to a signature in module's constant pool");
        }

        ip += 4;
        u_int32_t instruction_count = ntohl(*(u_int32_t *)(bytes + ip));
        ip += 4;
        u_int32_t locals_size = ntohl(*(u_int32_t *)(bytes + ip));
        ip += 4;
        function_definition def {bytes + ip, instruction_count, locals_size * 8};
        map.insert(std::pair(signature, def));
        bytes += instruction_count;
    }

    return map;
}

module_definition load_module(const char *bytes, strings_t& strings) {
    unsigned int ip = 0;
    // read constant pool
    const ConstantPool pool = load_constant_pool(bytes, ip, strings);
    const auto functions = load_functions_definitions(bytes, ip, pool);
    return {pool, functions};
}
