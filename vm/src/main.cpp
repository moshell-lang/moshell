#include "vm.h"
#include <cstring>
#include <fstream>
#include <iostream>
#include <vector>

int main(int argc, const char *argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream input(argv[1], std::ios::binary);
    if (!input) {
        std::cerr << "Could not open file " << argv[1] << " (" << strerror(errno) << ")\n";
        return 1;
    }
    std::vector<char> bytes_vec(std::istreambuf_iterator<char>(input), {});
    const char *bytes = bytes_vec.data();

    std::vector<size_t> lens;
    lens.resize(argc);
    for (int i = 0; i < argc; i++) {
        lens[i] = strlen(argv[i]);
    }
    moshell_vm vm = moshell_vm_init(argv, argc, lens.data());

    moshell_vm_register(vm, bytes, bytes_vec.size());
    int exit = moshell_vm_run(vm);
    moshell_vm_free(vm);

    return exit;
}
