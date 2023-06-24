#include "vm.h"
#include <cstring>
#include <fstream>
#include <iostream>
#include <vector>

int main(int argc, char *argv[]) {
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
    moshell_exec(bytes, bytes_vec.size());
    return 0;
}
