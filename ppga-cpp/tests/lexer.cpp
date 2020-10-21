#include <string>
#include "gtest/gtest.h"

#include "../ppga.hpp"

namespace {
using namespace ppga::lexer;

std::string err_to_string(ppga::error::ErrCtx& ex) {
    std::ostringstream ss;
    for (const auto& e : ex.errors) {
        ss << e << " ";
    }
    return ss.str();
}

/// Test the lexer.
TEST(LexerTest, TestTokensAreLexedCorrectly) {
    auto source = "1 2. 3.141592 true false nil \"a string\" . .. ... @ ; : ,"
                  "( { [ ] } ) => ident range len fn ? ?? let global break as match and or in if else while for fori return "
                  "lua {} not + - * / \\ ** % += -= *= /= \\= **= %= < <= > >= == != =\n"
                  "// this is a comment\n"
                  "/* this */ f\"frag {interp} \\{escaped}\" /* is a comment */";

    auto ex = ppga::error::ErrCtx();
    auto lexer = ppga::lexer::Lexer(source);
    auto result = lexer.lex(ex);

    EXPECT_FALSE(ex.had_error()) << err_to_string(ex);

    std::vector<std::pair<TokenKind, std::string>> expected {
        { TokenKind::Number, "1" },
        { TokenKind::Number, "2." },
        { TokenKind::Number, "3.141592" },
        { TokenKind::True, "true" },
        { TokenKind::False, "false" },
        { TokenKind::Nil, "nil" },
        { TokenKind::StringLiteral, "\"a string\"" },
        { TokenKind::Dot, "." },
        { TokenKind::DoubleDot, ".." },
        { TokenKind::Ellipsis, "..." },
        { TokenKind::Variadics, "@" },
        { TokenKind::Semicolon, ";" },
        { TokenKind::Colon, ":" },
        { TokenKind::Comma, "," },
        { TokenKind::LeftParen, "(" },
        { TokenKind::LeftBrace, "{" },
        { TokenKind::LeftBracket, "[" },
        { TokenKind::RightBracket, "]" },
        { TokenKind::RightBrace, "}" },
        { TokenKind::RightParen, ")" },
        { TokenKind::FatArrow, "=>" },
        { TokenKind::Identifier, "ident" },
        { TokenKind::Range, "range" },
        { TokenKind::Len, "len" },
        { TokenKind::Fn, "fn" },
        { TokenKind::Query, "?" },
        { TokenKind::DoubleQuery, "??" },
        { TokenKind::Let, "let" },
        { TokenKind::Global, "global" },
        { TokenKind::Break, "break" },
        { TokenKind::As, "as" },
        { TokenKind::Match, "match" },
        { TokenKind::And, "and" },
        { TokenKind::Or, "or" },
        { TokenKind::In, "in" },
        { TokenKind::If, "if" },
        { TokenKind::Else, "else" },
        { TokenKind::While, "while" },
        { TokenKind::For, "for" },
        { TokenKind::ForI, "fori" },
        { TokenKind::Return, "return" },
        { TokenKind::Lua, "lua {}" },
        { TokenKind::Not, "not" },
        { TokenKind::Plus, "+" },
        { TokenKind::Minus, "-" },
        { TokenKind::Star, "*" },
        { TokenKind::Slash, "/" },
        { TokenKind::BackSlash, "\\" },
        { TokenKind::Pow, "**" },
        { TokenKind::Percent, "%" },
        { TokenKind::PlusEqual, "+=" },
        { TokenKind::MinusEqual, "-=" },
        { TokenKind::StarEqual, "*=" },
        { TokenKind::SlashEqual, "/=" },
        { TokenKind::BackSlashEqual, "\\=" },
        { TokenKind::PowEqual, "**=" },
        { TokenKind::PercentEqual, "%=" },
        { TokenKind::Lt, "<" },
        { TokenKind::Le, "<=" },
        { TokenKind::Gt, ">" },
        { TokenKind::Ge, ">=" },
        { TokenKind::Eq, "==" },
        { TokenKind::Ne, "!=" },
        { TokenKind::Equal, "=" },
        { TokenKind::FString, R"(f"frag {interp} \{escaped}")" },
        { TokenKind::EndOfFile, "" }
    };

    EXPECT_EQ(result.size(), expected.size());

    size_t i = 0;
    for (size_t i = 0 ; i <expected.size(); ++i) {
        EXPECT_EQ(expected[i].first, result[i].kind()) << "[Test #" << i << "] Token kinds didn't match";
        EXPECT_EQ(expected[i].second, result[i].lexeme()) << "[Test #" << i << "] Token lexemes didn't match";
    }

}

}  // namespace
