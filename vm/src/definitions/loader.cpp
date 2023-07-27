#include "loader.h"

#include "byte_reader.h"
#include "conversions.h"
#include "memory/constant_pool.h"
#include "pager.h"

namespace msh {
    void loader::load_raw_bytes(const char *bytes, size_t size, pager &pager, StringsHeap &strings) {
        ByteReader reader(bytes, size);

        ConstantPool tmp_pool = load_constant_pool(reader, strings);
        uint32_t dynsym_len = ntohl(reader.read<uint32_t>());
        size_t pool_index = pager.push_pool(std::move(tmp_pool), dynsym_len);
        const ConstantPool &pool = pager.get_pool(pool_index);
        for (uint32_t i = 0; i < dynsym_len; ++i) {
            constant_index id_idx = ntohl(reader.read<constant_index>());
            const std::string &identifier = pool.get_string(id_idx);
            unresolved.emplace(pool_index, i, identifier);
        }

        while (reader.position() < size) {
            size_t page;
            // Read main function
            {
                const auto &[identifier, function] = load_function(reader, pool, pool_index);
                page = pager.push_page(memory_page{std::vector<char>(function.locals_size), identifier});
            }

            uint32_t exports_len = ntohl(reader.read<uint32_t>());
            for (uint32_t i = 0; i < exports_len; ++i) {
                constant_index id_idx = ntohl(reader.read<constant_index>());
                const std::string &identifier = pool.get_string(id_idx);
                uint32_t offset = ntohl(reader.read<uint32_t>());
                exported[identifier] = exported_variable{page, offset};
            }

            uint32_t functions_len = ntohl(reader.read<uint32_t>());
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

    const exported_variable &loader::get_exported(const std::string &name) const {
        return exported.at(name);
    }

    const char *loader::get_instructions(size_t index) const {
        return concatened_instructions.data() + index;
    }

    std::pair<const std::string &, const function_definition &> loader::load_function(ByteReader &reader, const ConstantPool &pool, size_t pool_index) {
        constant_index id_idx = ntohl(reader.read<constant_index>());
        const std::string &identifier = pool.get_string(id_idx);
        uint32_t locals_byte_count = ntohl(reader.read<uint32_t>());
        uint32_t parameters_byte_count = ntohl(reader.read<uint32_t>());
        uint8_t return_byte_count = reader.read<uint8_t>();
        uint32_t instruction_count = ntohl(reader.read<uint32_t>());
        const char *instructions = reader.read_n<char>(instruction_count);
        size_t effective_address = concatened_instructions.size();
        std::copy(instructions, instructions + instruction_count, std::back_inserter(concatened_instructions));
        function_definition def{
            locals_byte_count,
            parameters_byte_count,
            return_byte_count,
            effective_address,
            instruction_count,
            pool_index};
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
