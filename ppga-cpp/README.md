# ppga-cpp
A header-only C++ implementation of PPGA. In contrast with the Rust implementation, this one doesn't try to preserve comments or whitespace.
The resulting Lua code should be identical functionally.

## Usage example
```cpp
#include <iostream>
#include "ppga.hpp"

int main() {
    auto source = read_file(...);
    try {
        auto result = ppga::ppga_to_lua(source);
        std::cout << result << std::flush;
    } catch (std::runtime_error& e){
        std::cerr << e.what() << std::endl;
    }
    return 0;
}
```

## Interface
Although all classes and pipeline stages are public and can be accessed by the users, the intended way to use the library is via the `ppga::ppga_to_lua` function, which has two overloads:
```cpp
/// Transpile the given ppga source to lua code. An `std::runtime_error` will be thrown if an error is encountered at any stage of the pipeline.
inline std::string ppga_to_lua(
    const std::string_view source,
    PPGAConfig config = PPGAConfig{}
) /* throws std::runtime_error */;

/// Transpile the given ppga source to lua code. This function will return an option with the lua sorce
/// or an `std::nullopt` instance with all errors written to the given `ErrCtx`.
inline std::optional<std::string> ppga_to_lua(
    const std::string_view source,
    ppga::error::ErrCtx& ex,
    PPGAConfig config = PPGAConfig{}
) noexcept;
```

Additionally, `ppga::visitors` contains the available visitor classes: `LuaTranspiler` and `ASTPrinter`. A user may choose to execute them manually by feeding the output of
`ppga::ppga_parse` to the visitor class of choice. The snippet below demonstrates how to print a script's AST:
```cpp
#include <iostream>
#include "ppga.hpp"

int main() {
    auto source = read_file("../../tour.ppga");
    try {
        auto ex = ppga::error::ErrCtx{};
        auto result = ppga::ppga_parse(source, ex);
        ex.raise(); // will throw an exception if an error has occurred

        auto ast = std::move(result.value());
        auto printer = ppga::visitors::ASTPrinter();
        printer.visit(ast);

        std::cout << printer.finish() << std::endl;
    } catch (std::runtime_error& e){
        std::cerr << e.what() << std::endl;
    }
    return 0;
}
```

