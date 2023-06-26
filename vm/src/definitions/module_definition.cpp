#include "module_definition.h"
#include "byte_reader.h"
#include "conversions.h"
#include "definitions/function_definition.h"
#include "memory/constant_pool.h"
#include "memory/locals.h"

std::unordered_map<constant_index, function_definition>
load_functions_definitions(ByteReader& reader, const ConstantPool &pool) {

    std::unordered_map<constant_index, function_definition> map;

    u_int32_t function_count = ntohl(reader.read<u_int32_t>());

    for (u_int32_t i = 0; i < function_count; i++) {
        constant_index signature = ntohl(reader.read<u_int32_t>());

        try {
            validate_signature(pool.get_signature(signature));
        } catch (BadConstantType e) {
            throw InvalidModuleDescription("invalid function declaration signature: the constant index read at function declaration does not points to a signature in module's constant pool");
        }

        u_int32_t instruction_count = ntohl(reader.read<u_int32_t>());
        u_int32_t locals_size = ntohl(reader.read<u_int32_t>());
        char* instructions = reader.read_n<char>(instruction_count);
        function_definition def{
            locals_size * LOCAL_CELL_SIZE,
            instruction_count,
            instructions,
        };
        map[signature] = def;
    }

    return map;
}

module_definition load_module(ByteReader& reader, strings_t& strings) {
    try {
        // read constant pool
        const ConstantPool pool = load_constant_pool(reader, strings);
        const auto functions = load_functions_definitions(reader, pool);
        return module_definition{pool, functions};
    } catch (std::out_of_range e) {
        throw InvalidBytecodeError("Error when reading module bytecode: " + std::string(e.what()));
    }
}
