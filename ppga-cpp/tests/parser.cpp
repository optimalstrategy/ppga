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
    if (ex.had_error()) return;

    auto parser = ppga::parser::Parser(std::move(result), ppga::PPGAConfig{});
    auto maybe_ast = parser.parse(ex);

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
}  // namespace
