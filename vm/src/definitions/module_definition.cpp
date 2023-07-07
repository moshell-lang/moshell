#include "byte_reader.h"
#include "bytecode_unit.h"
#include "conversions.h"
#include "definitions/function_definition.h"
#include "memory/constant_pool.h"
#include "memory/locals.h"

std::unordered_map<const std::string *, function_definition>
load_function_definitions(ByteReader &reader, const ConstantPool &pool) {

    std::unordered_map<const std::string *, function_definition> map;

    uint32_t function_count = ntohl(reader.read<uint32_t>());

    for (uint32_t i = 0; i < function_count; i++) {
        // read the function's string identifier
        constant_index id_idx = ntohl(reader.read<constant_index>());
        const std::string *identifier = &pool.get_string(id_idx);

        uint32_t locals_byte_count = ntohl(reader.read<uint32_t>());
        uint32_t parameters_byte_count = ntohl(reader.read<uint32_t>());
        uint8_t return_byte_count = reader.read<uint8_t>();
        uint32_t instruction_count = ntohl(reader.read<uint32_t>());

        char *instructions = reader.read_n<char>(instruction_count);

#ifndef NDEBUG
        // returned bytes must be <= allocated bytes for locals
        if (return_byte_count > locals_byte_count) {
            throw InvalidBytecodeStructure("Function " + *identifier + " declares more return bytes than allocated locals capacity.");
        }

        // parameters bytes must be <= allocated bytes for locals
        if (parameters_byte_count > locals_byte_count) {
            throw InvalidBytecodeStructure("Function " + *identifier + " declares more parameters bytes than allocated locals capacity.");
        }

        if (return_byte_count > sizeof(uint64_t)) {
            throw InvalidBytecodeStructure("Function " + *identifier + " declares a return byte count which is greater than maximum allocated size " + std::to_string(sizeof(uint64_t)) + " (" + std::to_string(return_byte_count) + ").");
        }
#endif

        function_definition def{
            locals_byte_count,
            parameters_byte_count,
            return_byte_count,
            instruction_count,
            instructions,
        };

        map[identifier] = def;
    }

    return map;
}

bytecode_unit load_unit(ByteReader &reader, strings_t &strings) {
    try {
        ConstantPool pool = load_constant_pool(reader, strings);

        auto functions = load_function_definitions(reader, pool);

        return bytecode_unit{std::move(pool), std::move(functions)};
    } catch (const std::out_of_range &e) {
        throw InvalidBytecodeError("Error when reading module bytecode: " + std::string(e.what()));
    }
}
