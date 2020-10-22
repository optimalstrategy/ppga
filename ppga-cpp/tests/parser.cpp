#include <string>
#include <filesystem>
#include <fstream>
#include "gtest/gtest.h"

#include "../ppga.hpp"

namespace {
namespace fs = std::filesystem;

using namespace ppga::lexer;
using namespace ppga::parser;

std::string err_to_string(ppga::error::ErrCtx& ex) {
    std::ostringstream ss;
    for (const auto& e : ex.errors) {
        ss << e << " ";
    }
    return ss.str();
}

TEST(ParserTests, TestPrimitiveExpressions) {
    static constexpr auto source = "let a = 2 + 1;\n"
                  "print(\"x\", x);\n"
                  "print(f\"{a} string\");";

    auto ex = ppga::error::ErrCtx();
    auto maybe_ast = ppga::ppga_parse(source, ex);

    EXPECT_EQ(ex.had_error(), false) << err_to_string(ex);
    if (ex.had_error()) return;

    auto ast = std::move(maybe_ast.value());
    auto printer = ppga::visitors::ASTPrinter();
    printer.visit(ast);

    std::string output = printer.finish();
    EXPECT_EQ(output,
  R"(VarDecl
  kind: VarKind::Local
  name0: a
  initializer: Binary `+`
    left: Literal: `2`
    right: Literal: `1`
ExprStmt
  expr: Call
    callee: Variable: `print`
    arg0: Literal: `"x"`
    arg1: Variable: `x`
ExprStmt
  expr: Call
    callee: Variable: `print`
    arg0: FString
      frag0: Variable: `a`
      frag1: Literal: ` string`
)");
}


std::string read_file(const fs::path& path) {
    std::ifstream f(path, std::ios::in | std::ios::binary);
    const auto sz = fs::file_size(path);

    std::string result(sz, '\0');
    f.read(result.data(), sz);

    return result;
}

TEST(ParserTests, TestPPGATourAST) {
    auto tour_source = read_file("../../../tour.ppga");
    auto ast_string = read_file("../../../ppga-cpp/tests/tour.ppga.ast");

    auto ex = ppga::error::ErrCtx();
    auto lexer = Lexer(tour_source);
    auto maybe_tokens = lexer.lex(ex);

    if (ex.had_error()) {
        ASSERT_FALSE(ex.had_error()) << "Unexpected lexing errors: " << err_to_string(ex);
    }
    auto tokens = std::move(maybe_tokens.value());

    auto parser = Parser(std::move(tokens), ppga::PPGAConfig{});
    auto maybe_ast = parser.parse(ex);

    if (ex.had_error()) {
        ASSERT_FALSE(ex.had_error()) << "Unexpected parsing errors: " << err_to_string(ex);
    }

    auto ast = std::move(maybe_ast.value());
    auto printer = ppga::visitors::ASTPrinter();
    printer.visit(ast);
    EXPECT_EQ(printer.finish(), ast_string);
}
}  // namespace
