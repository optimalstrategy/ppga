#include <string>
#include "gtest/gtest.h"

#include "../ppga.hpp"

namespace {
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
    auto source = "let a = 2 + 1;\n"
                  "print(\"x\", x);\n"
                  "print(f\"{a} string\");";

    auto ex = ppga::error::ErrCtx();
    auto lexer = ppga::lexer::Lexer(source);
    auto result = lexer.lex(ex);

    EXPECT_EQ(ex.had_error(), false) << err_to_string(ex);
    if (ex.had_error()) {
        return;
    }

    auto parser = ppga::parser::Parser(std::move(result), ppga::PPGAConfig{});
    auto ast = parser.parse(ex);

    EXPECT_EQ(ex.had_error(), false) << err_to_string(ex);
    if (ex.had_error()) {
        return;
    }
}
}  // namespace
