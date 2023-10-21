#include "loader.h"

#include "byte_reader.h"
#include "memory/constant_pool.h"
#include "pager.h"

#define MAPPINGS_ATTRIBUTE 1

namespace msh {
    void loader::load_raw_bytes(const std::byte *bytes, size_t size, pager &pager, msh::heap &heap) {
        ByteReader reader(bytes, size);

        ConstantPool tmp_pool = load_constant_pool(reader, heap);
        uint32_t dynsym_len = reader.read<uint32_t>();
        size_t pool_index = pager.push_pool(std::move(tmp_pool), dynsym_len);
        const ConstantPool &pool = pager.get_pool(pool_index);
        for (uint32_t i = 0; i < dynsym_len; ++i) {
            constant_index id_idx = reader.read<constant_index>();
            const std::string &identifier = pool.get_string(id_idx);
            unresolved.push({pool_index, i, identifier});
        }

        while (reader.position() < size) {
            // Read main function
            {
                const auto &[identifier, function] = load_function(reader, pool, pool_index);

                // read page exports
                uint32_t page_size = reader.read<uint32_t>();
                size_t page = pager.push_page(memory_page{std::vector<char>(page_size), identifier});

                uint32_t exports_len = reader.read<uint32_t>();
                for (uint32_t i = 0; i < exports_len; ++i) {
                    constant_index id_idx = reader.read<constant_index>();
                    uint32_t offset = reader.read<uint32_t>();
                    bool is_obj_ref = reader.read<bool>();

                    const std::string &identifier = pool.get_string(id_idx);
                    exported[identifier] = exported_variable{page, offset, is_obj_ref};
                }

                std::vector<uint32_t> obj_refs_offsets;
            }

            uint32_t functions_len = reader.read<uint32_t>();
            for (uint32_t i = 0; i < functions_len; ++i) {
                load_function(reader, pool, pool_index);
            }
        }
    }

    const function_definition &loader::get_function(const std::string &name) const {
        return functions.at(name);
    }

    loader::function_map::const_iterator loader::find_function(const std::string &name) const {
        return functions.find(name);
    }

    loader::function_map::const_iterator loader::functions_cend() const {
        return functions.cend();
    }

    loader::exported_variable_map::const_iterator loader::exported_cbegin() const {
        return exported.cbegin();
    }
    loader::exported_variable_map::const_iterator loader::exported_cend() const {
        return exported.cend();
    }

    const exported_variable &loader::get_exported(const std::string &name) const {
        return exported.at(name);
    }

    const std::byte *loader::get_instructions(size_t index) const {
        return concatened_instructions.data() + index;
    }

    std::pair<const std::string &, const function_definition &> loader::load_function(ByteReader &reader, const ConstantPool &pool, size_t pool_index) {
        constant_index id_idx = reader.read<constant_index>();
        const std::string &identifier = pool.get_string(id_idx);

        uint32_t locals_byte_count = reader.read<uint32_t>();
        uint32_t parameters_byte_count = reader.read<uint32_t>();
        uint8_t return_byte_count = reader.read<uint8_t>();
        uint32_t instruction_count = reader.read<uint32_t>();

        const std::byte *instructions = reader.read_n<std::byte>(instruction_count);
        size_t effective_address = concatened_instructions.size();
        std::copy(instructions, instructions + instruction_count, std::back_inserter(concatened_instructions));

        uint32_t offsets_count = reader.read<uint32_t>();
        std::vector<uint32_t> offsets;
        offsets.reserve(offsets_count);

        for (uint32_t i = 0; i < offsets_count; i++)
            offsets.push_back(reader.read<uint32_t>());

        function_definition def = {
            identifier,
            locals_byte_count,
            parameters_byte_count,
            return_byte_count,
            effective_address,
            instruction_count,
            pool_index,
            offsets,
            {},
        };

        uint8_t attributes_count = reader.read<uint8_t>();

        for (uint8_t i = 0; i < attributes_count; i++) {
            uint8_t attribute_kind = reader.read<uint8_t>();
            switch (attribute_kind) {
            case MAPPINGS_ATTRIBUTE: {
                // read Mappings attribute
                if (!def.mappings.empty()) {
                    throw InvalidBytecodeError("Mappings (1) attribute defined multiple times for function " + identifier);
                }
                uint32_t mappings_count = reader.read<uint32_t>();
                for (uint32_t i = 0; i < mappings_count; i++) {
                    size_t instruction_start = reader.read<uint32_t>();
                    size_t line = reader.read<uint32_t>();
                    def.mappings.push_back({instruction_start, line});
                }
                break;
            }
            default:
                throw InvalidBytecodeError("Unknown attribute kind: " + std::to_string(attribute_kind));
            }
        }

        return *functions.insert_or_assign(identifier, def).first;
    }

    void loader::resolve_all(pager &pager) {
        while (!unresolved.empty()) {
            auto [pool_index, index, name] = unresolved.top();
            unresolved.pop();
            auto it = exported.find(name);
            if (it == exported.end()) {
                throw std::runtime_error("unresolved symbol " + name);
            }
            pager.bind(pool_index, index, it->second);
        }
    }
}
