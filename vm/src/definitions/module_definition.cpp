#include "module_definition.h"
#include "byte_reader.h"
#include "conversions.h"
#include "definitions/function_definition.h"
#include "memory/constant_pool.h"
#include "memory/locals.h"

std::unordered_map<const std::string *, function_definition>
load_functions_definitions(ByteReader &reader, const ConstantPool &pool) {

    std::unordered_map<const std::string *, function_definition> map;

    uint32_t function_count = ntohl(reader.read<uint32_t>());

    for (uint32_t i = 0; i < function_count; i++) {
        // read the function's string identifier
        constant_index id_idx = ntohl(reader.read<constant_index>());
        const std::string *identifier = &pool.get_string(id_idx);

        uint32_t parameters_byte_count = ntohl(reader.read<uint32_t>());
        uint8_t return_byte_count = reader.read<uint8_t>();
        uint32_t instruction_count = ntohl(reader.read<uint32_t>());
        uint32_t locals_byte_count = ntohl(reader.read<uint32_t>());

        char *instructions = reader.read_n<char>(instruction_count);

        // returned bytes must be <= allocated bytes for locals
        if (return_byte_count > locals_byte_count) {
            throw InvalidModuleDescription("Function " + *identifier + " declares more return bytes than allocated locals capacity.");
        }

        // parameters bytes must be <= allocated bytes for locals
        if (parameters_byte_count > locals_byte_count) {
            throw InvalidModuleDescription("Function " + *identifier + " declares more parameters bytes than allocated locals capacity.");
        }

        if (return_byte_count > sizeof(uintptr_t)) {
            throw InvalidModuleDescription("Function " + *identifier + " declares a return byte count which is greater than maximum allocated size " + std::to_string(sizeof(uintptr_t)) + " (" + std::to_string(return_byte_count) + ").");
        }

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

void check_bytecode_flags(ByteReader &reader) {
    // read supported architecture and check it is supported
    char arch = reader.read<char>();

#if UINTPTR_MAX == 0xFFFFFFFF // 32 bits
    char expected = 1;
    std::string arch_name = "32 bits";
#elif UINTPTR_MAX == 0xFFFFFFFFFFFFFFFFu // 64 bits
    char expected = 2;
    std::string arch_name = "64 bits";
#else
#error "VM only supports 32 and 64 bits architectures"
#endif

    if (arch != expected) {
        std::string received_architecture;
        switch (arch) {
        case 1: {
            received_architecture = "32 bits";
            break;
        }
        case 2: {
            received_architecture = "64 bits";
            break;
        }
        }
        throw InvalidBytecodeError("Specified architecture is invalid, this VM instance attempted to read bytecode for " + received_architecture + " systems but can only support " + arch_name);
    }
}

module_definition load_module(ByteReader &reader, strings_t &strings) {
    try {
        check_bytecode_flags(reader);

        ConstantPool pool = load_constant_pool(reader, strings);

        const auto functions = load_functions_definitions(reader, pool);

        return module_definition{std::move(pool), functions};
    } catch (const std::out_of_range &e) {
        throw InvalidBytecodeError("Error when reading module bytecode: " + std::string(e.what()));
    }
}
