#include <iostream>
#include <filesystem>
#include <fstream>
#include <string>

#include "ppga.hpp"

namespace fs = std::filesystem;

std::string read_file(fs::path);

int main() {
    auto source = read_file("../../tour.ppga");
    auto result = ppga::ppga_to_lua(
        source,
        ppga::PPGAConfig{}
    );
    std::cout << result << "\n";
    return 0;
}

std::string read_file(fs::path path) {
    std::ifstream f(path, std::ios::in | std::ios::binary);
    const auto sz = fs::file_size(path);

    std::string result(sz, '\0');
    f.read(result.data(), sz);

    return result;
}