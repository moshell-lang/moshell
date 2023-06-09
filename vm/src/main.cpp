#include "interpreter.h"
#include <fstream>
#include <iostream>
#include <vector>

int main(int argc, char *argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    std::ifstream input(argv[1], std::ios::binary);
    std::vector<char> bytes(std::istreambuf_iterator<char>(input), {});
    int ip = 0;
    struct constant_pool pool = load_constant_pool(bytes.data(), &ip);
    run(pool, ip, bytes.data(), bytes.size());
    return 0;
}
