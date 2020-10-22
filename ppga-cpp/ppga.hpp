#ifndef PPGA_SCRIPT_PPGA_HPP
#define PPGA_SCRIPT_PPGA_HPP

#include <cstddef>
#include <memory>
#include <string>
#include <cstring>
#include <sstream>
#include <utility>
#include <vector>
#include <optional>
#include <variant>
#include <iostream>
#include <algorithm>
#include <type_traits>
#include <cassert>

/// Enables debug checks and logging
#ifndef PPGA_DEBUG
#define PPGA_DEBUG 0
#endif

/// Enables parser logging
#ifndef PPGA_PARSER_DEBUG
#define PPGA_PARSER_DEBUG 0
#endif


/// Makes the parser terminate after the first fail.
#ifndef PPGA_PARSER_INSTANTLY_FAIL
#define PPGA_PARSER_INSTANTLY_FAIL 0
#endif

#define PPGA_PARSER_LOG(name) \
    if (PPGA_DEBUG && PPGA_PARSER_DEBUG) { \
        std::cout << #name << ":\n    previous = "; \
        if (this->current > 0) std::cout << this->previous(); \
        else std::cout << "..."; \
        std::cout << "\n    current = " << this->peek() << std::endl; \
    }
#define PPGA_PARSER_LOG_EXPR(expr) if (PPGA_DEBUG && PPGA_PARSER_DEBUG) { std::cout << (expr) << std::endl; }

namespace ppga {
namespace constants {
using namespace std::string_view_literals;

/// The number of indentation spaces in the resulting lua code.
static size_t DEFAULT_PPGA_INDENT_SIZE = 4;
/// The name of the default operator function
static constexpr auto DEFAULT_OP_NAME = "__PPGA_INTERNAL_DEFAULT"sv;
/// The name of the error handler function
static constexpr auto ERR_HANDLER_NAME = "__PPGA_INTERNAL_HANDLE_ERR"sv;
/// The name of the default error handler callback
static constexpr auto ERR_CALLBACK_NAME = "__PPGA_INTERNAL_DFLT_ERR_CB"sv;
/// The DEFAULT_OP_NAME definition
static constexpr auto DEFAULT_OP_DEFN = R"(local function __PPGA_INTERNAL_DEFAULT(x, default)
    if x ~= nil then return (x) end
    return (default)
end)"sv;
/// THe ERR_HANDLER_NAME definition
static constexpr auto ERR_HANDLER_DEFN = R"(local function __PPGA_INTERNAL_HANDLE_ERR(cb, ...)
    local ok, err = ...
    if err ~= nil then
        ok, err = cb(err)
    end
    return (ok), (err)
end)"sv;
/// The ERR_CALLBACK_NAME definition
static constexpr auto ERR_CALLBACK_DEFN = R"(local function __PPGA_INTERNAL_DFLT_ERR_CB(err)
    error(err)
end)"sv;
}

struct PPGAConfig {
    size_t indent_size = constants::DEFAULT_PPGA_INDENT_SIZE;
    bool include_ppga_std = true;
};

namespace lexer {
struct Span {
    size_t start;
    /// Exclusive
    size_t end;
    size_t line;
    std::string_view source;

    explicit Span(size_t start, size_t end, size_t line, std::string_view source)
            : start(start), end(end), line(line), source(source) {}

    [[nodiscard]]
    constexpr std::string_view slice() const {
        return source.substr(start, end - start);
    }

    friend std::ostream& operator<<(std::ostream& out, const Span& span) {
        out << "Span { line = " << span.line << ", lexeme = \"" << span.slice()
            << "\" (" << span.start << ".." << span.end << ") }";
        return out;
    }
};
}

namespace error {
struct PPGAError {
    lexer::Span span;
    std::string description;

    friend std::ostream& operator<<(std::ostream& out, const PPGAError& error) {
        out << "PPGAError {\n    Span: " << error.span << ",\n    description: \""
            << error.description << "\"\n}";
        return out;
    }
};

struct ErrCtx {
    std::vector<PPGAError> errors;

    void error(lexer::Span span, std::string&& description) {
        errors.push_back(PPGAError {
            span,
            std::move(description)
        });
    }

    void error(PPGAError&& error) {
        errors.push_back(std::move(error));
    }

    void extend(ErrCtx& ex) {
        errors.reserve(errors.size() + ex.errors.size());
        errors.insert(errors.end(),ex.errors.begin(),ex.errors.end());
        ex.errors.clear();
    }

    void raise() {
        if (!had_error()) return;
        std::ostringstream errors;
        for (const auto& e : this->errors) {
            errors << e << "\n";
        }
        throw std::runtime_error{ errors.str() };
    }

    [[nodiscard]]
    inline bool had_error() const noexcept {
        return !errors.empty();
    }
};
}

namespace lexer {
using namespace std::string_view_literals;

class Token;

enum class TokenKind {
    /// A single dot: `.`. Used in attribute access expressions.
    Dot,
    /// Two dots in a row: `..`. Used for string concatenation.
    DoubleDot,
    /// Three dots in a row: `...`. Used to unpack arrays/tables/function calls.
    Ellipsis,
    /// A single `@`. Used to define variadic parameters in function declarations.
    Variadics,
    /// A single `;`. Used to end statements.
    Semicolon,
    /// A single `:`. Used to call static methods on tables.
    Colon,
    /// A single `,`. Used for value separation in lists, dicts, and function and variable declarations.
    Comma,
    /// A single `(`.
    LeftParen,
    /// A single `)`.
    RightParen,
    /// A single `{`. Used to open blocks and dict literals.
    LeftBrace,
    /// A single `}`. Used to close blocks and dict literals.
    RightBrace,
    /// A single `[`. Used to open attribute access expressions and list literals.
    LeftBracket,
    /// A single `]`. Used to close attribute access expressions and list literals.
    RightBracket,
    /// A fat arrow: `=>`. Used in single-expression functions.
    FatArrow,
    /// An ASCII identifier. Must start with a letter and may contain letters and numbers afterwards.
    Identifier,
    /// The `range` keyword.
    Range,
    /// The `len` keyword.
    Len,
    /// The `fn` keyword.
    Fn,
    /// A `?`. Used to indicate that an argument is optional or
    /// to automatically unpack let statements initialized with an ok/err pair.
    Query,
    /// The default value operator: `??`. Evaluates the second operand if the first operand is `nil`.
    DoubleQuery,
    /// The `let` keyword. Used for local variable definitions.
    Let,
    /// The `global` keyword. Used for global variable and function definitions.
    Global,
    /// The `break` keyword.
    Break,
    /// The `as` keyword.
    As,
    /// The `match` keyword.
    Match,
    /// The `and` keyword.
    And,
    /// The `or` keyword.
    Or,
    /// The `in` keyword.
    In,
    /// The `if` keyword.
    If,
    /// The `else` keyword.
    Else,
    /// The `while` keyword.
    While,
    /// The `for` keyword.
    For,
    /// The `fori` keyword.
    ForI,
    /// The `return` keyword.
    Return,
    /// A `lua {}` block. The sequence of bytes inside the block
    /// is de-indented once and directly pasted into the lua file.
    Lua,
    /// The `true` literal.
    True,
    False,
    /// The `nil` literal.
    Nil,
    /// A floating point number.
    Number,
    /// An interpolated string literal. Must start with an `f`: `f"a + b = {a + b}"`.
    /// An interpolated fragment may be escaped with a backslash: `f"I am \{ escaped }"`.
    FString,
    /// A string literal.
    StringLiteral,
    /// The `not` keyword.
    Not,
    /// A single `*`. Used for multiplication.
    Star,
    /// A single `/`. Used for float division.
    Slash,
    /// A single `\`. Used for integer division.
    BackSlash,
    /// A single `%`. Used to find remainders.
    Percent,
    /// Two asterisks in a row: `**`. Used for exponentiation.
    Pow,
    /// A single `+`. Used for addition.
    Plus,
    /// A single `-`. Used subtraction multiplication.
    Minus,
    /// A single `<`. Used for less than comparisons.
    Lt,
    /// A `<=`. Used for less than or equal to comparisons.
    Le,
    /// A single `>`. Used for greater than comparisons,
    Gt,
    /// A `>=`. Used for greater than or equal to comparisons.
    Ge,
    /// A double equals sign: `==`. Used for equality checks.
    Eq,
    /// A `!=`. Used for not-equality checks.
    Ne,
    /// A single `=`. Used for assignment and dict initialization.
    Equal,
    /// A single `+=`. Expanded to `$1 = $1 + $2`.
    PlusEqual,
    /// A single `-=`. Expanded to `$1 = $1 - $2`.
    MinusEqual,
    /// A single `*=`. Expanded to `$1 = $1 * $2`.
    StarEqual,
    /// A single `**=`. Expanded to `$1 = $1 ** $2`.
    PowEqual,
    /// A single `/=`. Expanded to `$1 = $1 / $2`.
    SlashEqual,
    /// A single `\=`. Expanded to `$1 = $1 \ $2`.
    BackSlashEqual,
    /// A single `%=`. Expanded to `$1 = $1 % $2`.
    PercentEqual,
    /// A C-style comment.
    Comment,
    MultilineComment,
    /// A single End of Line character.
    EOL,
    /// A sequence of one or more whitespace characters. Skipped by the lexer.
    Whitespace,
    /// Used to indicate an error.
    Error,
    /// Used to indicate the end of the file.
    EndOfFile,
};

inline std::ostream& operator<<(std::ostream& out, const TokenKind kind) {
    const char* s;
#define PROCESS_VAL(p) case(TokenKind :: p): s = #p; break;
    switch (kind) {
        PROCESS_VAL(Dot);
        PROCESS_VAL(DoubleDot);
        PROCESS_VAL(Ellipsis);
        PROCESS_VAL(Variadics);
        PROCESS_VAL(Semicolon);
        PROCESS_VAL(Colon);
        PROCESS_VAL(Comma);
        PROCESS_VAL(LeftParen);
        PROCESS_VAL(RightParen);
        PROCESS_VAL(LeftBrace);
        PROCESS_VAL(RightBrace);
        PROCESS_VAL(LeftBracket);
        PROCESS_VAL(RightBracket);
        PROCESS_VAL(FatArrow);
        PROCESS_VAL(Identifier);
        PROCESS_VAL(Range);
        PROCESS_VAL(Len);
        PROCESS_VAL(Fn);
        PROCESS_VAL(Query);
        PROCESS_VAL(DoubleQuery);
        PROCESS_VAL(Let);
        PROCESS_VAL(Global);
        PROCESS_VAL(Break);
        PROCESS_VAL(As);
        PROCESS_VAL(Match);
        PROCESS_VAL(And);
        PROCESS_VAL(Or);
        PROCESS_VAL(In);
        PROCESS_VAL(If);
        PROCESS_VAL(Else);
        PROCESS_VAL(While);
        PROCESS_VAL(For);
        PROCESS_VAL(ForI);
        PROCESS_VAL(Return);
        PROCESS_VAL(Lua);
        PROCESS_VAL(True);
        PROCESS_VAL(False);
        PROCESS_VAL(Nil);
        PROCESS_VAL(Number);
        PROCESS_VAL(FString);
        PROCESS_VAL(StringLiteral);
        PROCESS_VAL(Not);
        PROCESS_VAL(Star);
        PROCESS_VAL(Slash);
        PROCESS_VAL(BackSlash);
        PROCESS_VAL(Percent);
        PROCESS_VAL(Pow);
        PROCESS_VAL(Plus);
        PROCESS_VAL(Minus);
        PROCESS_VAL(Lt);
        PROCESS_VAL(Le);
        PROCESS_VAL(Gt);
        PROCESS_VAL(Ge);
        PROCESS_VAL(Eq);
        PROCESS_VAL(Ne);
        PROCESS_VAL(Equal);
        PROCESS_VAL(PlusEqual);
        PROCESS_VAL(MinusEqual);
        PROCESS_VAL(StarEqual);
        PROCESS_VAL(PowEqual);
        PROCESS_VAL(SlashEqual);
        PROCESS_VAL(BackSlashEqual);
        PROCESS_VAL(PercentEqual);
        PROCESS_VAL(Comment);
        PROCESS_VAL(MultilineComment);
        PROCESS_VAL(EOL);
        PROCESS_VAL(Whitespace);
        PROCESS_VAL(Error);
        PROCESS_VAL(EndOfFile);
    }
#undef PROCESS_VAL
    out << s;
    return out;
}

struct FStringFragment {
    std::vector<Token> inner_tokens;
    std::string_view string_fragment;
    bool is_string;

    explicit FStringFragment(std::string_view view)
        : string_fragment(view), is_string(true) {}

    explicit FStringFragment(std::vector<Token> tokens)
        : inner_tokens(std::move(tokens)), is_string(false) {}

    explicit FStringFragment(std::vector<Token>&& tokens)
            : inner_tokens(std::move(tokens)), is_string(false) {}
};


static const std::unordered_map<std::string_view, TokenKind> KEYWORDS = {
        {"range"sv, TokenKind::Range},
        {"len"sv, TokenKind::Len},
        {"fn"sv, TokenKind::Fn},
        {"let"sv, TokenKind::Let},
        {"global"sv, TokenKind::Global},
        {"break"sv, TokenKind::Break},
        {"as"sv, TokenKind::As},
        {"match"sv, TokenKind::Match},
        {"and"sv, TokenKind::And},
        {"or"sv, TokenKind::Or},
        {"in"sv, TokenKind::In},
        {"if"sv, TokenKind::If},
        {"else"sv, TokenKind::Else},
        {"while"sv, TokenKind::While},
        {"for"sv, TokenKind::For},
        {"fori"sv, TokenKind::ForI},
        {"return"sv, TokenKind::Return},
        {"true"sv, TokenKind::True},
        {"false"sv, TokenKind::False},
        {"nil"sv, TokenKind::Nil},
        {"not"sv, TokenKind::Not}
};

class Token {
    Span span_;
    TokenKind kind_;

    /// The payload of the token. Contents are determined by the kind.
    std::optional<std::variant<
        /// The fragments of an f-string; kind = TokenKind::Fstring.
        std::vector<FStringFragment>,
        /// The contents of a lua block.
        std::string_view
    >> payload;
public:
    explicit Token(Span span, TokenKind kind)
        : span_(span), kind_(kind), payload(std::nullopt) {}

    static Token fstring(
        Span span,
        TokenKind kind,
        std::vector<FStringFragment> fragments
    )
    {
        auto token = Token(span, kind);
        token.payload = std::move(fragments);
        return token;
    }

    static Token lua_block(Span span, std::string_view contents) {
        auto token = Token(span, TokenKind::Lua);
        token.payload = contents;
        return token;
    }

    [[nodiscard]]
    constexpr inline std::string_view lexeme() const {
        return span_.slice();
    }

    [[nodiscard]]
    constexpr inline bool has_payload() const noexcept {
        return payload.has_value();
    }

    [[nodiscard]]
    inline std::string_view get_lua_payload() const {
        return std::get<std::string_view>(payload.value());
    }

    [[nodiscard]]
    inline std::vector<FStringFragment>& get_fstring_payload() {
        return std::get<std::vector<FStringFragment>>(payload.value());
    }

    [[nodiscard]]
    constexpr inline TokenKind kind() const noexcept {
        return kind_;
    }

    [[nodiscard]]
    constexpr inline const Span& span() const noexcept {
        return span_;
    }

    friend std::ostream& operator<<(std::ostream& out, const Token& token) {
        out << "Token {\n    span = " << token.span() << ",\n    kind = " << token.kind()
            << ",\n    payload = " << (token.has_payload() ? "..." : "()") << "\n}";
        return out;
    }
};

class Lexer {
    std::string_view source;
    size_t current;
    size_t line;
    std::vector<Token> tokens;
public:
    explicit Lexer(std::string_view source)
            : source(source), current(0), line(1), tokens(std::vector<Token>{}) {}

    std::optional<std::vector<Token>> lex(error::ErrCtx& ex) noexcept {
        while (!is_at_end()) {
            tokens.push_back(next_token(ex));
        }
        // Push the EOF token
        if (tokens.size() == 0 || tokens.at(tokens.size() - 1).kind() != TokenKind::EndOfFile) {
             tokens.push_back(next_token(ex));
        }
        return ex.had_error() ? std::nullopt : std::optional(std::move(this->tokens));
    }

    Token next_token(error::ErrCtx& ex) noexcept {
        skip_whitespace();

        auto ch = advance();
        if (!ch.has_value()) {
            auto end = source.length();
            return Token(Span(end, end, line, source), TokenKind::EndOfFile);
        }

        size_t start = current - 1;
        size_t tok_line = line;

#define CHOOSE(ch, otherwise, then) kind = match(ch) ? then : otherwise; break;

        TokenKind kind;
        switch (ch.value()) {
            case '(': kind = TokenKind::LeftParen; break;
            case ')': kind = TokenKind::RightParen; break;
            case '{': kind = TokenKind::LeftBrace; break;
            case '}': kind = TokenKind::RightBrace; break;
            case '[': kind = TokenKind::LeftBracket; break;
            case ']': kind = TokenKind::RightBracket; break;
            case ';': kind = TokenKind::Semicolon; break;
            case ':': kind = TokenKind::Colon; break;
            case ',': kind = TokenKind::Comma; break;
            case '@': kind = TokenKind::Variadics; break;
            case '?': CHOOSE('?', TokenKind::Query, TokenKind::DoubleQuery);
            case '-': CHOOSE('=', TokenKind::Minus, TokenKind::MinusEqual);
            case '+': CHOOSE('=', TokenKind::Plus, TokenKind::PlusEqual);
            case '%': CHOOSE('=', TokenKind::Percent, TokenKind::PercentEqual)
            case '<': CHOOSE('=', TokenKind::Lt, TokenKind::Le);
            case '>': CHOOSE('=', TokenKind::Gt, TokenKind::Ge);
            case '\\': CHOOSE('=', TokenKind::BackSlash, TokenKind::BackSlashEqual);
            case '=': {
                if (match('=')) {
                    kind = TokenKind::Eq;
                } else if (match('>')) {
                    kind = TokenKind::FatArrow;
                } else {
                    kind = TokenKind::Equal;
                }
                break;
            }
            case '.':  {
                if (match('.')) {
                    kind = match('.') ? TokenKind::Ellipsis : TokenKind::DoubleDot;
                } else {
                    kind = TokenKind::Dot;
                }
                break;
            }
            case '*': {
                if (match('=')) {
                    kind = TokenKind::StarEqual;
                } else if (match('*')) {
                    kind = match('=') ? TokenKind::PowEqual : TokenKind::Pow;
                } else {
                    kind = TokenKind::Star;
                }
                break;
            };
            case '!': {
                if (match('=')) {
                    kind = TokenKind::Ne;
                } else {
                    kind = TokenKind::Error;
                    ex.error(span(start), "Unexpected character '!' (did you mean '!='?)");
                }
                break;
            }
            case '\"': [[fallthrough]];
            case '\'': return string(ex, ch.value());
            case '/': {
                if (match('/')) {
                    comment();
                    return next_token(ex);
                } else if (match('*')) {
                    multi_line_comment(ex);
                    return next_token(ex);
                } else {
                    CHOOSE('=', TokenKind::Slash, TokenKind::SlashEqual);
                }
                break;
            }
            default: {
                if (is_valid_identifier_start(ch.value())) {
                    return identifier(ex);
                } else if (is_numeric(ch)) {
                    return number();
                }
                kind = TokenKind::Error;
                std::ostringstream s;
                s << "Unexpected character '" << ch.value() << "'";
                ex.error(span(start), s.str());
            }
        }
#undef CHOOSE

        return Token(span(start, tok_line), kind);
    }

    Token string(error::ErrCtx& ex, char quote) {
        auto start = current - 1;
        auto start_line = line;

        while (!is_at_end()) {
            if (check('\\') && peek_next() == quote) {
                advance();
                advance();
            } else if (check(quote)) {
                break;
            } else {
                advance();
            }
        }

        if (!match(quote)) {
            auto span = this->span(start, start_line);
            ex.error(span, "Unterminated string literal");
            return Token(span, TokenKind::Error);
        }

        return Token(span(start, start_line), TokenKind::StringLiteral);
    }

    Token number() noexcept {
        auto start = current - 1;

        while (is_numeric(peek())) {
            advance();
        }

        if (match('.')) {
            while (is_numeric(peek())) {
                advance();
            }
        }

        return Token(span(start), TokenKind::Number);
    }

    Token identifier(error::ErrCtx& ex) noexcept {
        if (previous() == 'f' && match("\'\"")) {
            char quote = previous().value();
            return fstring(ex, quote);
        }

        auto start = current - 1;
        while (is_valid_identifier_part(peek())) {
            advance();
        }

        auto span = this->span(start);
        auto name = span.slice();

        if (name == "lua") {
            return lua_block(ex, start);
        }

        TokenKind kind;
        auto found = KEYWORDS.find(name);
        if (found != KEYWORDS.cend()) {
            kind = found->second;
        } else {
            kind = TokenKind::Identifier;
        }

        return Token(span, kind);
    }

    Token lua_block(error::ErrCtx& ex, size_t start) {
        auto start_line = line;
        skip_whitespace();

        if (!match("{")) {
            ex.error(span(start, start_line), "Missing a `}` after the `lua` keyword");
        }

        auto content_start = current;
        size_t n_opened = 1;

        while (!is_at_end()) {
            char ch = advance().value();
            if (ch == '\n') ++line;
            else if (ch == '{') n_opened += 1;
            else if (ch == '}') {
                if (--n_opened == 0) break;
            }
        }

        if (n_opened > 0) {
            auto span = this->span(content_start - 1, start_line);
            ex.error(span, "Unterminated lua block");
            return Token(span, TokenKind::Error);
        }

        return Token::lua_block(
            span(start, start_line),
            source.substr(content_start, current - content_start - 1)
        );
    }

    Token fstring(error::ErrCtx& ex , char quote) {
        auto start = current - 2;
        auto start_line = line;
        std::vector<FStringFragment> frags{};

        auto prev_fragment_end = current;
        while (!is_at_end()) {
            if (check('\\') && peek_next() == quote) {
                advance();
                advance();
            } else if (check(quote)) {
                break;
            } else if (check('{')) {
                // An escaped interpolation fragment, e.g. \{1 + 2}
                if (previous() == '\\') {
                    auto span = this->span(prev_fragment_end);
                    span.end -= 1;  // exclude the escape
                    frags.emplace_back(span.slice());
                    prev_fragment_end = current;
                // An unescaped interpolation fragment, e.g. {1 + 2}
                } else {
                    // The literal string up to this point
                    auto span = this->span(prev_fragment_end);
                    frags.emplace_back(span.slice());
                    advance(); // consume the {

                    auto frag_start = current;
                    while (peek() != '}' && !is_at_end()) {
                        advance();
                    }

                    span = this->span(frag_start);
                    auto sublexer = Lexer(span.slice());
                    auto frag_tokens = sublexer.lex(ex).value_or(std::vector<Token>{});
                    frag_tokens.pop_back(); // Pop the EOF
                    frags.emplace_back(frag_tokens);
                    prev_fragment_end = current + 1;
                }
            }
            advance();
        }

        if (prev_fragment_end < source.length() - 1) {
            auto span = this->span(prev_fragment_end);
            frags.emplace_back(span.slice());
        }

        if (!match(quote)) {
            auto span = this->span(start, start_line);
            ex.error(span, "Unterminated f-string");
            return Token(span, TokenKind::Error);
        }

        auto span = this->span(start, start_line);
        return Token::fstring(span, TokenKind::FString, std::move(frags));
    }

    /// Scans a single-line comment.
    inline void comment() noexcept {
        while (!match('\n')) {
            advance();
        }
        ++line;
    }

    /// Scans a multi-line comment.
    void multi_line_comment(error::ErrCtx& ex) {
        auto start = current - 2;
        auto start_line = line;

        while (!is_at_end()) {
            if (check('*') && peek_next() == '/') {
                break;
            }

            auto ch = advance();
            if (ch == '\n') {
                ++line;
            }
        }

        if (is_at_end()) {
            ex.error(span(start, start_line), "Unterminated multi-line comment");
        } else {
            advance();
            advance();
        }
    }

    /// Advances the lexer until a non-whitespace character is encountered.
    void skip_whitespace() noexcept {
        while (check(" \n\t\r\f")) {
            if (advance() == '\n') {
                ++line;
            }
        }
    }

    /// Checks if the given character is a valid identifier start character,
    /// i.e. if it matches [aA-zZ_].
    template<typename Char>
    static inline bool is_valid_identifier_start(Char ch) noexcept {
        return (ch >= 'a' && ch <= 'z')
               || (ch >= 'A' && ch <= 'Z')
               || ch == '_';
    }

    /// Checks if the given character is a valid identifier character,
    /// i.e. if it matches [aA-zZ0-9_].
    template<typename Char>
    static inline bool is_valid_identifier_part(Char ch) noexcept {
        return is_valid_identifier_start(ch) || is_numeric(ch);
    }

    /// Checks if the given character is an ascii digit.
    template<typename Char>
    static inline bool is_numeric(Char ch) noexcept {
        return ch >= '0' && ch <= '9';
    }

    /// Creates a new span starting at the given offset and line and ending at the previous character.
    [[nodiscard]]
    inline Span span(size_t start, size_t start_line) const noexcept {
        return Span(start, current, start_line, source);
    }

    /// Creates a new span starting at the given offset and current line and ending at the previous character.
    [[nodiscard]]
    inline Span span(size_t start) const noexcept {
        return Span(start, current, line, source);
    }

    /// Checks if the next character matches the given character.
    [[nodiscard]]
    inline bool check(char c) const noexcept {
        return peek() == c;
    }

    /// Checks if the next character matches any of the given characters.
    [[nodiscard]]
    inline bool check(const std::string& options) const noexcept {
        return std::any_of(options.begin(), options.end(), [this](char ch) { return ch == peek(); });
    }

    /// Consumes the next character if it matches the given character.
    inline bool match(char c) noexcept {
        if (check(c)) {
            advance();
            return true;
        }
        return false;
    }

    /// Consumes the next character if it matches any of the given characters.
    inline bool match(const std::string& options) noexcept {
        if (check(options)) {
            advance();
            return true;
        }
        return false;
    }

    /// Attempts to advance to the next character.
    /// If no more characters are left, returns `std::nullopt`.
    inline std::optional<char> advance() noexcept {
        if (is_at_end()) {
            return std::nullopt;
        }
        return source.at(current++);
    }

    /// Attempts to return the current character.
    /// Returns `std::nullopt` if there is none.
    [[nodiscard]]
    inline std::optional<char> peek() const noexcept {
        if (is_at_end()) {
            return std::nullopt;
        }
        return source.at(current);
    }

    /// Attempts to return the next character.
    /// Returns `std::nullopt` if there is none.
    [[nodiscard]]
    inline std::optional<char> peek_next() const noexcept {
        if (current + 1 == source.length()) {
            return std::nullopt;
        }
        return source.at(current + 1);
    }

    /// Returns the previous character.
    [[nodiscard]]
    inline std::optional<char> previous() const noexcept {
        if (current == 0) {
            return std::nullopt;
        }
        return source.at(current - 1);
    }

    /// Returns true if no characters are left.
    [[nodiscard]]
    inline bool is_at_end() const noexcept {
        return current >= source.length();
    }


};


}

namespace ast {
struct Visitor;
struct AST;
struct Expr;
struct Stmt;
struct Literal;
struct Len;
struct LuaBlock;
struct Variable;
struct GeneratedVariable;
struct FString;
struct Get;
struct GetItem;
struct Call;
struct Unary;
struct Binary;
struct Grouping;
struct ArrayLiteral;
struct DictLiteral;
struct Lambda;
struct ExprStmt;
struct Range;
struct If;
struct For;
struct While;
struct Block;
struct StmtSequence;
struct Return;
struct FuncDecl;
struct VarDecl;
struct Assignment;
struct Break;

struct Node {
    virtual void accept(Visitor& visitor) = 0;
    [[nodiscard]]
    virtual std::string name() const { return typeid(*this).name(); };
};
struct Stmt : public Node {};
struct Expr : public Node {};

using ExprPtr = std::unique_ptr<Expr>;
using StmtPtr = std::unique_ptr<Stmt>;

struct Visitor {
    virtual void visit(AST& ast)  = 0;
    virtual void visit(Expr& expr) = 0;
    virtual void visit(Stmt& stmt) = 0;
    virtual void visit(Literal& lit) = 0;
    virtual void visit(Len& len) = 0;
    virtual void visit(LuaBlock& block) = 0;
    virtual void visit(Variable& var) = 0;
    virtual void visit(GeneratedVariable& var) = 0;
    virtual void visit(FString& fstring) = 0;
    virtual void visit(Get& get) = 0;
    virtual void visit(GetItem& get) = 0;
    virtual void visit(Call& call) = 0;
    virtual void visit(Unary& unary) = 0;
    virtual void visit(Binary& binary) = 0;
    virtual void visit(Grouping& grouping) = 0;
    virtual void visit(ArrayLiteral& arr) = 0;
    virtual void visit(DictLiteral& dict) = 0;
    virtual void visit(Lambda& lambda) = 0;
    virtual void visit(ExprStmt& expr) = 0;
    virtual void visit(If& conditional) = 0;
    virtual void visit(For& loop) = 0;
    virtual void visit(While& loop) = 0;
    virtual void visit(Block& block) = 0;
    virtual void visit(StmtSequence& seq) = 0;
    virtual void visit(Return& ret) = 0;
    virtual void visit(FuncDecl& decl) = 0;
    virtual void visit(VarDecl& decl) = 0;
    virtual void visit(Assignment& ass) = 0;
    virtual void visit(Break& break_) = 0;
};

template<class Base, class Derived>
struct Visitable : public Base {
    Visitable() : Base() {}
    void accept(Visitor& visitor) override {
        static_assert(
            std::is_base_of_v<Visitable<Base, Derived>, Derived>,
            "Derived must derive from Visitable<Base, Derived>"
        );
        if (PPGA_DEBUG) {
            std::cout << "Visiting " << this << " " << this->name() << std::endl;
            if (dynamic_cast<Derived*>(this) == nullptr) {
                std::cerr << "Couldn't cast this = " << this << " to " << typeid(Derived).name() << std::endl;
                std::abort();
            }
        }
        visitor.visit(*static_cast<Derived*>(this));
    }
};

struct FunctionData {
    std::optional<std::string_view> name;
    std::vector<ExprPtr> params;
    StmtPtr body;
};

struct Literal : public Visitable<Expr, Literal> {
    std::string_view value;
    bool is_string{false};
    explicit Literal(std::string_view value) : Visitable(), value(value) {};
    explicit Literal(std::string_view value, bool is_string)
        : Visitable(), value(value), is_string(is_string) {};
};

struct Len : public Visitable<Expr, Len> {
    ExprPtr expr;
    explicit Len(ExprPtr expr) : Visitable(), expr(std::move(expr)) {};
};

struct LuaBlock : public Visitable<Expr, LuaBlock> {
    std::string_view contents;
    explicit LuaBlock(std::string_view contents) : Visitable(), contents(contents) {};
};

struct Variable : public Visitable<Expr, Variable> {
    std::string_view name;
    explicit Variable(std::string_view name) : Visitable(), name(name) {};
};

struct GeneratedVariable : public Visitable<Expr, GeneratedVariable> {
    std::string name;
    explicit GeneratedVariable(std::string&& name) : Visitable(), name(std::move(name)) {};
};

struct FString : public Visitable<Expr, FString> {
     std::vector<ExprPtr> fragments;
     explicit FString(std::vector<ExprPtr>&& fragments) : Visitable(), fragments(std::move(fragments)) {}
};

struct Get : public Visitable<Expr, Get> {
    ExprPtr obj;
    std::string_view attr;
    bool is_static;
    explicit Get(ExprPtr obj, std::string_view attr, bool is_static)
        : Visitable(), obj(std::move(obj)), attr(attr), is_static(is_static) { }
};

struct GetItem : public Visitable<Expr, GetItem> {
    ExprPtr obj;
    ExprPtr item;
    explicit GetItem(ExprPtr obj, ExprPtr item)
        : Visitable(), obj(std::move(obj)), item(std::move(item)) {}
};

struct Call : public Visitable<Expr, Call> {
    ExprPtr callee;
    std::vector<ExprPtr> args;
    explicit Call(ExprPtr callee, std::vector<ExprPtr>&& args)
        : Visitable(), callee(std::move(callee)), args(std::move(args)) {}
};

struct Unary : public Visitable<Expr, Unary> {
    std::string_view op;
    ExprPtr operand;

    explicit Unary(std::string_view op, ExprPtr operand)
        : Visitable(), op(op), operand(std::move(operand)) {}
};

struct Grouping : public Visitable<Expr, Grouping> {
    ExprPtr expr;
    explicit Grouping(ExprPtr expr) : Visitable(), expr(std::move(expr)) {}
};

struct Binary : public Visitable<Expr, Binary> {
    std::string_view op;
    ExprPtr left;
    ExprPtr right;
    explicit Binary(std::string_view op, ExprPtr left, ExprPtr right)
        : Visitable(), op(op), left(std::move(left)), right(std::move(right)) {}
};

struct ArrayLiteral : public Visitable<Expr, ArrayLiteral> {
    std::vector<ExprPtr> values;
    explicit ArrayLiteral(std::vector<ExprPtr>&& values) : Visitable(), values(std::move(values)) {}
};

struct DictLiteral : public Visitable<Expr, DictLiteral> {
    std::vector<std::pair<ExprPtr, ExprPtr>> pairs;
    explicit DictLiteral(std::vector<std::pair<ExprPtr, ExprPtr>>&& pairs)
        : Visitable(), pairs(std::move(pairs)) {}
};

struct Lambda : public Visitable<Expr, Lambda> {
    FunctionData data;
    explicit Lambda(FunctionData&& data) : Visitable(), data(std::move(data)) {}
};

struct ExprStmt : public Visitable<Stmt, ExprStmt> {
    ExprPtr expr;
    explicit ExprStmt(ExprPtr expr) : Visitable(), expr(std::move(expr)) {}
};

struct If : public Visitable<Stmt, If> {
    ExprPtr condition;
    StmtPtr then;
    std::optional<StmtPtr> otherwise;
    bool is_elif{false};

    explicit If(ExprPtr condition, StmtPtr then, std::optional<StmtPtr> otherwise)
        : Visitable(),
          condition(std::move(condition)), then(std::move(then)), otherwise(std::move(otherwise)) {}
};

struct Range {
    std::string_view start;
    std::string_view end;
    std::string_view step;
};

struct For : public Visitable<Stmt, For> {
    bool is_fori;
    std::vector<ExprPtr> vars;
    std::variant<
        Range, // A range
        std::vector<ExprPtr> // A sequence of expressions
    > condition;
    StmtPtr body;

    explicit For(
        bool is_fori,
        std::vector<ExprPtr>&& vars,
        std::variant<Range, std::vector<ExprPtr>>&& condition,
        StmtPtr body
    ) : Visitable(), is_fori(is_fori), vars(std::move(vars)),
        condition(std::move(condition)), body(std::move(body)) {}
};

struct While : public Visitable<Stmt, While> {
    ExprPtr condition;
    StmtPtr body;

    explicit While(ExprPtr condition, StmtPtr body)
        : Visitable(), condition(std::move(condition)), body(std::move(body)) {}
};

struct Block : public Visitable<Stmt, Block> {
    std::vector<StmtPtr> statements;
    bool is_standalone;

    explicit Block(std::vector<StmtPtr>&& statements, bool is_standalone)
        : Visitable(), statements(std::move(statements)), is_standalone(is_standalone) {}
};

/// Used for inserting multiple statements without generating a block.
struct StmtSequence : public Visitable<Stmt, StmtSequence> {
    std::vector<StmtPtr> statements;

    explicit StmtSequence(std::vector<StmtPtr>&& statements)
        : Visitable(), statements(std::move(statements)) {}
};

struct Return: public Visitable<Stmt, Return> {
    std::vector<ExprPtr> values;
    explicit Return(std::vector<ExprPtr>&& values) : Visitable(), values(std::move(values)) {}
};

struct Assignment: public Visitable<Stmt, Assignment> {
    std::string_view op;
    std::vector<ExprPtr> vars;
    ExprPtr value;
    explicit Assignment(std::string_view op, std::vector<ExprPtr>&& vars, ExprPtr value)
        : Visitable(), op(op), vars(std::move(vars)), value(std::move(value)) {}
};

struct Break: public Visitable<Stmt, Break> {
    explicit Break() : Visitable() {}
};

enum class VarKind {
    Local,
    Global
};

struct FuncDecl : public Visitable<Stmt, FuncDecl> {
    FunctionData data;
    VarKind kind;
    explicit FuncDecl(FunctionData&& data, VarKind kind)
        : Visitable(), data(std::move(data)), kind(kind) {}
};

struct VarDecl : public Visitable<Stmt, VarDecl> {
    VarKind kind;
    std::vector<std::string> names;
    std::optional<ExprPtr> initializer;

    explicit VarDecl(VarKind kind, std::vector<std::string>&& names, std::optional<ExprPtr> initializer)
        : Visitable(), kind(kind), names(std::move(names)), initializer(std::move(initializer)) {}
};

enum class MatchPatKind {
    /// Compare the bound variable to the value of the pattern
    Comparison,
    /// Use the value of the pattern as the condition
    Value,
    /// The final else block
    Else,
};

struct AST {
    PPGAConfig config;
    std::vector<StmtPtr> statements;
};


} // ast
namespace utils {
// A helper constant that is always false.
template<class> inline constexpr bool always_false_v = false;

template <typename>
struct deduce_arg_type;

template <typename Return, typename X, typename T>
struct deduce_arg_type<Return (X::*)(T) const> {
    using type = T;
    using return_type = Return;
};

template <typename F>
using arg_type = typename deduce_arg_type<decltype(&F::operator())>::type;

template <typename F>
using return_type = typename deduce_arg_type<decltype(&F::operator())>::return_type;

template <typename Base, typename F, typename Return = std::optional<return_type<std::decay_t<F&&>>>>
constexpr inline Return attempt(Base*& ptr, F&& f) {
    using f_type = std::decay_t<decltype(f)>;
    using p_type = arg_type<f_type>;
    if (auto cp = dynamic_cast<p_type>(ptr); cp != nullptr) {
        return std::optional(std::forward<decltype(f)>(f)(cp));
    }
    return std::nullopt;
}

/// I = len(Fs), we've run out of tuple elements
template <std::size_t I = 0, typename Base, typename Return, typename... Fs>
constexpr inline std::optional<typename std::enable_if<I == sizeof...(Fs), Return>::type>
_dynamic_visitor_tup(Base*, std::tuple<Fs&...>&) {
    return std::nullopt;
}

template <std::size_t I = 0, typename Base, typename Return, typename... Fs>
constexpr inline std::optional<typename std::enable_if<I < sizeof...(Fs), Return>::type>
_dynamic_visitor_tup(Base* ptr, std::tuple<Fs&...>& t) {
    auto fn = std::get<I>(t);
    auto result = attempt(ptr, fn);
    if (result.has_value()) {
        return result;
    }
    return _dynamic_visitor_tup<I + 1, Base, Return, Fs...>(ptr, t);
}

template <typename Return, typename Base, typename... Fs>
constexpr inline std::optional<Return> dynamic_visitor(Base* ptr, Fs... fs) {
    auto t = std::forward_as_tuple(fs...);
    return _dynamic_visitor_tup<0, Base, Return, Fs...>(ptr, t);
}

/* See https://stackoverflow.com/a/29975225 */
// get the first type in a pack, if it exists:
template<class...Ts>
struct first {};

template<class T, class...Ts>
struct first<T, Ts...>{
    using type = T;
};
template<class...Ts>
using first_t = typename first<Ts...>::type;

// build the return type:
template<class T0, class...Ts>
using vector_T =
typename std::conditional<
        std::is_same<T0, void>::value,
        typename std::decay<first_t<Ts...>>::type,
        T0
>::type;
template<class T0, class...Ts>
using vector_t = std::vector<vector_T<T0, Ts...>>;

// make a vector, non-empty arg case:
template<class T0 = void, class... Ts, class R = vector_t<T0, Ts...>>
R make_vector( Ts&&...ts ) {
    R retval;
    retval.reserve(sizeof...(Ts)); // we know how many elements
    // array unpacking trick:
    using discard = int[];
    (void) discard{0, (
            (retval.emplace_back(std::forward<Ts>(ts))),
                    void(),
                    0
    )...};
    return retval; // NRVO!
}
// the empty overload:
template<class T>
std::vector<T> make_vector() {
    return {};
}
} // utils

namespace parser {
using namespace lexer;

template<typename T>
using optional_ref = std::optional<std::reference_wrapper<T>>;

template<typename T>
constexpr inline optional_ref<T> make_optional_ref(T& value) {
    return std::optional{std::ref(value) };
}

/// This is an internal exception. Outside code should never encounter it.
struct parser_exception : public std::logic_error {
    explicit parser_exception() : std::logic_error("") {}
};


class Parser {
    std::vector<lexer::Token> tokens;
    size_t current;
    ppga::PPGAConfig config;
    error::ErrCtx ex;

public:
    Parser(std::vector<lexer::Token>&& tokens, PPGAConfig config)
    : tokens(std::move(tokens)), current(0), config(config) {}

    std::optional<ast::AST> parse(error::ErrCtx& user_ex) {
        auto statements = std::vector<ast::StmtPtr>();

        while (!is_at_end()) {
            try {
                statements.push_back(statement());
            } catch (parser_exception& ignored) {
                synchronize();
                user_ex.extend(this->ex);
                if (PPGA_PARSER_DEBUG && PPGA_PARSER_INSTANTLY_FAIL) break;
            }
        }

        return user_ex.had_error() ? std::nullopt
            : std::optional(ast::AST { this->config, std::move(statements) });
    }

private:
    ast::StmtPtr statement() {
        PPGA_PARSER_LOG(statement);
        if (match<TokenKind::Let, TokenKind::Global>()) {
            return var_declaration();
        } else if (match<TokenKind::LeftBrace>()) {
            return block(true);
        } else if (match<TokenKind::Fn>()) {
            auto name = consume_identifier("Expected a function name after the keyword.");
            auto fn = lambda();
            fn.name = name.lexeme();
            return std::make_unique<ast::FuncDecl>(std::move(fn), ast::VarKind::Local);
        }else if (match<TokenKind::If>()) {
            return if_statement();
        } else if (match<TokenKind::Match>()) {
            return match_statement();
        } else if (match<TokenKind::For>()) {
            return for_statement(false);
        } else if (match<TokenKind::ForI>()) {
            return for_statement(true);
        } else if (match<TokenKind::While>()) {
            return while_statement();
        } else if (match<TokenKind::Return>()) {
            auto expr = std::make_unique<ast::Return>(arguments(TokenKind::Semicolon));
            consume_semicolon("Expected a `;` after `return`");
            return expr;
        } else if (match<TokenKind::Break>()) {
            consume_semicolon("Expected a `;` after `break`");
            return std::make_unique<ast::Break>();
        }
        auto stmt = assignment();
        if (auto expr = dynamic_cast<ast::ExprStmt*>(stmt.get());
            expr != nullptr && dynamic_cast<ast::LuaBlock*>(expr->expr.get()) != nullptr)
        {
            /* allow lua blocks to be unterminated */
        } else {
            consume_semicolon("Expected a `;` after the expression");
        }
        PPGA_PARSER_LOG_EXPR("leaving statement (after assignment())");
        return stmt;
    }

    ast::StmtPtr var_declaration() {
        PPGA_PARSER_LOG(var_declaration);
        ast::VarKind kind;
        switch (previous().kind()) {
            case TokenKind::Let:
                kind = ast::VarKind::Local;
                break;
            case TokenKind::Global:
                kind = ast::VarKind::Global;
                break;
            default:
                assert(false && "Unreachable");
        }
        if (kind == ast::VarKind::Global && match<TokenKind::Fn>()) {
            auto name = consume_identifier("Expected a function name after the keyword.");
            auto fn = lambda();
            fn.name =  name.lexeme();
            return std::make_unique<ast::FuncDecl>(std::move(fn), kind);
        }

        auto names = std::vector<Token>{ consume_identifier("Expected a variable name after the keyword.") };
        while (match<TokenKind::Comma>()) {
            names.push_back(consume_identifier("Expected a variable name after the comma."));
        }

        std::optional<ast::ExprPtr> initializer{ std::nullopt };

        if (match<TokenKind::Equal>()) {
            initializer = expression();
        } else {
            if (kind == ast::VarKind::Global) {
                auto ident = names.at(0);
                error(ident, "Global variables must be assigned a value.");
            }
            if (match<TokenKind::Query>()) {
                error(previous(), "Cannot use `?` without an initializer.");
            }
        }

        auto perform_error_expansion = match<TokenKind::Query>() ? std::optional(previous()) : std::nullopt;
        consume_semicolon("Expected a `;` after the variable declaration");

        if (perform_error_expansion.has_value() && names.size() != 1) {
            error(perform_error_expansion.value(), "Cannot use `?` with more than one variable name.");
            throw parser_exception();
        }

        ast::StmtPtr stmt;
        if (perform_error_expansion.has_value()) {
            auto query = perform_error_expansion.value();
            if (!has_err_block(*initializer.value())) {
                ast::ExprPtr call = std::make_unique<ast::Call>(
                    make_var(constants::ERR_HANDLER_NAME),
                    utils::make_vector(make_var(constants::ERR_CALLBACK_NAME), std::move(initializer.value()))
                );
                initializer = std::move(call);
            }
            // Generate a let statement that initializes the variable to nil
            auto target_var = names.at(0).lexeme();
            auto decl = std::make_unique<ast::VarDecl>(
                    kind,
                    std::vector<std::string>{std::string(target_var)},
                    std::make_unique<ast::Literal>("nil"sv)
            );

            std::ostringstream ok_name;
            ok_name << "_ok_L" << query.span().line << "S" << query.span().start;
            std::ostringstream err_name;
            err_name << "_err_L" << query.span().line << "S" << query.span().start;
            std::vector<ast::ExprPtr> assignment_var;
            assignment_var.emplace_back(make_var(target_var));

            auto block = std::make_unique<ast::Block>(utils::make_vector<ast::StmtPtr>(
                    // Generate the tuple destruction statement:
                    // let ok, err = <initializer>;
                    std::make_unique<ast::VarDecl>(
                            kind,
                            std::vector<std::string>{ok_name.str(), err_name.str()},
                            std::move(initializer)
                    ),
                    // Check if the error is `nil` and return the error if it is
                    std::make_unique<ast::If>(
                            std::make_unique<ast::Binary>(
                                    "!=", make_owned_var(err_name.str()), std::make_unique<ast::Literal>("nil"sv)),
                            std::make_unique<ast::Block>(utils::make_vector<ast::StmtPtr>(
                                std::make_unique<ast::Return>(
                                    utils::make_vector<ast::ExprPtr>(
                                        std::make_unique<ast::Literal>("nil"sv),
                                        make_owned_var(err_name.str())
                                    )
                                )
                            ), false),
                            std::nullopt
                    ),
                    // Assign the ok to the variable
                    std::make_unique<ast::Assignment>(
                        "=",
                        std::move(assignment_var),
                        make_owned_var(ok_name.str())
                    )
            ), true);
            stmt = std::make_unique<ast::StmtSequence>(utils::make_vector<ast::StmtPtr>(std::move(decl), std::move(block) ));
        } else {
            auto owned_names = std::vector<std::string>();
            owned_names.reserve(names.size());
            for (const auto& name : names) {
                owned_names.emplace_back(name.lexeme());
            }
            stmt = std::make_unique<ast::VarDecl>(kind, std::move(owned_names), std::move(initializer.value()));
        }

        return stmt;
    }

    static bool has_err_block(ast::Expr& expr) {
        ast::Expr* node = &expr;

        while (true) {
            if (auto call = dynamic_cast<ast::Call*>(node); call != nullptr) {
                if (
                    auto v = dynamic_cast<ast::GeneratedVariable*>(call->callee.get());
                    v != nullptr && v->name == constants::ERR_HANDLER_NAME
                ) {
                    return true;
                }
                break;
            } else if (auto group = dynamic_cast<ast::Grouping*>(node); group != nullptr) {
                node = group->expr.get();
            } else {
                break;
            }
        }

        return false;
    }

    ast::StmtPtr block(bool is_standalone) {
        PPGA_PARSER_LOG(block);

        std::vector<ast::StmtPtr> statements{};
        while (!check(TokenKind::RightBrace) && !is_at_end()) {
            try {
                statements.push_back(statement());
            } catch (parser_exception&) {
                synchronize();
            }
        }
        consume(TokenKind::RightBrace, "Expected a `}` after the block");
        return std::make_unique<ast::Block>(std::move(statements), is_standalone);
    }

    ast::StmtPtr if_statement() {
        PPGA_PARSER_LOG(if_statement);
        auto condition = expression();
        try_consume(TokenKind::LeftBrace, "Expected a `{` after the if condition");
        auto then = block(false);
        std::optional<ast::StmtPtr> otherwise{};

        if (match<TokenKind::Else>()) {
            if (match<TokenKind::If>()) {
                otherwise = if_statement();
            } else {
                try_consume(TokenKind::LeftBrace, "Expected `{` or an `if` after the `else`.");
                otherwise = block(false);
            }
        }

       return std::make_unique<ast::If>(std::move(condition), std::move(then), std::move(otherwise));
    }

    ast::StmtPtr match_statement() {
        PPGA_PARSER_LOG(match_statement);
        auto keyword = previous();
        auto value = expression();
        std::string bound_var;

        if (match<TokenKind::As>()) {
            consume_identifier("Expected a bound variable name after the `as`.");
            bound_var = previous().lexeme();
        } else if (auto var = dynamic_cast<ast::Variable*>(value.get()); var != nullptr) {
            bound_var = var->name;
        } else {
            std::ostringstream ss;
            ss << "_mbound_L" << keyword.span().line << "S" << keyword.span().start;
            bound_var = ss.str();
        }

        consume(TokenKind::LeftBrace, "Expected a `{` after the match value.");

        std::vector<ast::StmtPtr> arms;
        std::optional<ast::StmtPtr> any_arm;
        bool any_arm_last = false;
        std::optional<lexer::Span> any_arm_span = std::nullopt;

        while (!check(TokenKind::RightBrace) && !is_at_end()) {
            any_arm_last = false;
            auto pattern = expression();
            ast::MatchPatKind kind;

            if (auto pat = dynamic_cast<ast::Variable*>(pattern.get()); pat != nullptr && pat->name == "_") {
                any_arm_span = previous().span();
                kind = ast::MatchPatKind::Else;
            } else {
                kind = infer_pattern_kind(bound_var, *pattern);
            }

            consume(TokenKind::LeftBrace, "Expected a `{` after the arm pattern.");
            auto body = block(false);
            switch (kind) {
                case ast::MatchPatKind::Value:
                    arms.push_back(std::make_unique<ast::If>(std::move(pattern), std::move(body), std::nullopt));
                    break;
                case ast::MatchPatKind::Comparison:
                    arms.push_back(std::make_unique<ast::If>(
                        std::make_unique<ast::Binary>("=="sv, std::move(pattern), make_owned_var(std::string(bound_var))),
                        std::move(body),
                        std::nullopt
                    ));
                    break;
                case ast::MatchPatKind::Else:
                    if (any_arm.has_value()) {
                        error("Only one wildcard arm `_` may be present in a single match statement.");
                        throw parser_exception();
                    } else {
                        any_arm_last = true;
                        any_arm = std::move(body);
                    }
                    break;
            }
        }

        consume(TokenKind::RightBrace, "Expected a `}` after the match statement");

        if (any_arm.has_value() && !any_arm_last) {
            error("The wildcard arm `_` must be the last arm.");
            throw parser_exception();
        }

        // Build a chain of if statements from the arms
        std::optional<ast::StmtPtr> stmt = std::nullopt;
        for (auto it = arms.rbegin(); it != arms.rend(); ++it) {
            if (any_arm.has_value()) {
                any_arm.swap(stmt);
            }

            if (stmt.has_value()) {
                auto maybe_if = dynamic_cast<ast::If*>(it->get());
                assert(maybe_if != nullptr && "An arm must always have a value");
                stmt.swap(maybe_if->otherwise);
                stmt = std::move(*it);
            } else {
                stmt = std::move(*it);
            }
        }

        if (!stmt.has_value()) {
            error(keyword.span(), "A match statement must have at least one arm.");
            throw parser_exception();
        }

        return std::make_unique<ast::Block>(utils::make_vector<ast::StmtPtr>(
            std::make_unique<ast::VarDecl>(
                ast::VarKind::Local,
                std::vector<std::string> { bound_var },
                std::move(value)),
            std::move(stmt.value())
        ),true);
    }

    /// Infers the kind of the given pattern. All patterns that mention the bound variable are value patterns,
    /// all other patterns are comparison patterns.
    ast::MatchPatKind infer_pattern_kind(std::string_view var, ast::Expr& expr) {
        return utils::dynamic_visitor<ast::MatchPatKind>(
            &expr,
            [&var](const ast::Variable* v) {
                return v->name == var ? ast::MatchPatKind::Value : ast::MatchPatKind::Comparison;
            },
            [&var](const ast::GeneratedVariable* v) {
                return v->name == var ? ast::MatchPatKind::Value : ast::MatchPatKind::Comparison;
            },
            [&var, this](const ast::GetItem* item) {
                return (is_value_pat(infer_pattern_kind(var, *item->obj)) ||
                        is_value_pat(infer_pattern_kind(var, *item->item)))
                        ? ast::MatchPatKind::Value : ast::MatchPatKind::Comparison;
            },
            [&var, this](const ast::Call* call) {
                if (is_value_pat(infer_pattern_kind(var, *call->callee))) {
                    return ast::MatchPatKind::Value;
                }
                for (const auto& arg : call->args) {
                    if (is_value_pat(infer_pattern_kind(var, *arg))) {
                        return ast::MatchPatKind::Value;
                    }
                }
                return ast::MatchPatKind::Comparison;
            },
            [&var, this](const ast::Unary* unary) {
                return infer_pattern_kind(var, *unary->operand);
            },
            [&var, this](const ast::Binary* bin) {
                return (is_value_pat(infer_pattern_kind(var, *bin->left)) ||
                        is_value_pat(infer_pattern_kind(var, *bin->right)))
                       ? ast::MatchPatKind::Value : ast::MatchPatKind::Comparison;
            }
        ).value_or(ast::MatchPatKind::Comparison);
    }

    static inline bool is_value_pat(ast::MatchPatKind pat) {
        return pat == ast::MatchPatKind::Value;
    }

    ast::StmtPtr for_statement(bool is_fori) {
        PPGA_PARSER_LOG(for_statement);
        std::vector<ast::ExprPtr> vars = parameters();
        if (vars.empty()) {
            error(previous(), "Expected an identifier after the loop keyword");
            throw parser_exception();
        }
        consume(TokenKind::In, "Expected an `in` after the loop variables");

        std::variant<ast::Range, std::vector<ast::ExprPtr>> condition;
        if (match<TokenKind::Range>()) {
            condition = range();
            if (is_fori) {
                error(previous(), "A range cannot be used with a fori loop.");
                throw parser_exception();
            }
        } else {
            condition = arguments(TokenKind::LeftBrace);
        }

        try_consume(TokenKind::LeftBrace, "Expected a `{` after the loop condition");
        return std::make_unique<ast::For>(
            is_fori,
            std::move(vars),
            std::move(condition),
            block(false)
        );
    }

    ast::Range range() {
        PPGA_PARSER_LOG(range);
        try_consume(TokenKind::LeftParen, "Expected a `(` after `range`");
        auto step = "1"sv;
        auto start = "2"sv;
        auto end = consume(TokenKind::Number, "Expected a range stop value.").lexeme();
        if (match<TokenKind::Comma>()) {
            start = end;
            end = consume(TokenKind::Number, "Expected a range stop value").lexeme();
        }
        if (match<TokenKind::Comma>()) {
            step = consume(TokenKind::Number, "Expected a range step value").lexeme();
        }
        consume(TokenKind::RightParen, "Expected a `)` after the arguments");
        return ast::Range { start, end, step };
    }

    ast::StmtPtr while_statement() {
        PPGA_PARSER_LOG(while_statement);
        auto condition = expression();
        try_consume(TokenKind::LeftBrace, "Expected a `{` after the loop condition");
        return std::make_unique<ast::While>(std::move(condition), block(false));
    }

    ast::StmtPtr assignment() {
        PPGA_PARSER_LOG(assignment);
        auto exprs = utils::make_vector(expression());

        while (match<TokenKind::Comma>()) {
            exprs.push_back(expression());
        }

        if (match<TokenKind::Equal,
                 TokenKind::PlusEqual,
                 TokenKind::StarEqual,
                 TokenKind::SlashEqual,
                 TokenKind::PowEqual>())
        {
            auto span = previous().span();
            auto op = previous().lexeme();
            for (const auto& expr : exprs) {
                // Only variables, attr.accesses, and item[accesses] may be assigned to.
                if (
                    dynamic_cast<ast::Variable*>(expr.get()) == nullptr
                    && dynamic_cast<ast::Get*>(expr.get()) == nullptr
                    && dynamic_cast<ast::GetItem*>(expr.get()) == nullptr
                ) {
                    error(span, "Invalid assignment target");
                    throw parser_exception();
                }
            }
            return std::make_unique<ast::Assignment>(op, std::move(exprs), expression());
        }

        if (exprs.size() > 1) {
            error(
                previous().span(),
                "Comma is allowed only in let/global, assignment, and return statements."
            );
            throw parser_exception();
        }

        PPGA_PARSER_LOG_EXPR("leaving assignment");
        return std::make_unique<ast::ExprStmt>(std::move(exprs.at(0)));
    }

    ast::FunctionData lambda() {
        PPGA_PARSER_LOG(lambda);
        try_consume(TokenKind::LeftParen, "Expected a `(` before the parameter list.");

        std::vector<ast::ExprPtr> params{};
        if (!check(TokenKind::RightParen)) {
            params = parameters();
        }

        consume(TokenKind::RightParen, "Expected a `)` after the parameter list");

        ast::StmtPtr body;
        if (match<TokenKind::FatArrow>()) {
            body = std::make_unique<ast::Block>(
                utils::make_vector<ast::StmtPtr>(std::make_unique<ast::Return>(utils::make_vector(expression()))),
                false
            );
        } else {
            try_consume(TokenKind::LeftBrace, "Expected `{` a `=>` after the parameter list.");
            body = block(false);
        }

        return ast::FunctionData {
            std::nullopt,
            std::move(params),
            std::move(body)
        };
    }

    ast::ExprPtr expression() {
        PPGA_PARSER_LOG(expression);
        if (match<TokenKind::Fn>()) {
            return std::make_unique<ast::Lambda>(lambda());
        }
        return default_op();
    }

    ast::ExprPtr default_op() {
        PPGA_PARSER_LOG(default_op);
        auto expr = logic_or();

        while (match<TokenKind::DoubleQuery>()) {
            auto right = logic_or();
            ast::ExprPtr callee = std::make_unique<ast::Variable>(
                constants::DEFAULT_OP_NAME
            );
            auto args = utils::make_vector<ast::ExprPtr>(std::move(expr), std::move(right));
            expr = std::make_unique<ast::Call>(std::move(callee), std::move(args));
        }


        PPGA_PARSER_LOG_EXPR("leaving default_op");
        return expr;
    }

    ast::ExprPtr logic_or() {
        PPGA_PARSER_LOG(logic_or);
        return parse_binary<&Parser::logic_and, TokenKind::Or>();
    }

    ast::ExprPtr logic_and() {
        PPGA_PARSER_LOG(logic_and);
        return parse_binary<&Parser::equality, TokenKind::And>();
    }

    ast::ExprPtr equality() {
        PPGA_PARSER_LOG(equality);
        return parse_binary<&Parser::comparison, TokenKind::Ne, TokenKind::Eq>();
    }

    ast::ExprPtr comparison() {
        PPGA_PARSER_LOG(comparison);
        return parse_binary<&Parser::addition, TokenKind::Lt, TokenKind::Le, TokenKind::Gt, TokenKind::Ge>();
    }

    ast::ExprPtr addition() {
        PPGA_PARSER_LOG(addittion);
        return parse_binary<&Parser::multiplication, TokenKind::Plus, TokenKind::Minus, TokenKind::DoubleDot>();
    }

    // TODO: remember to fix the backslash operator
    ast::ExprPtr multiplication() {
        PPGA_PARSER_LOG(multiplication);
        return parse_binary<
            &Parser::exponentiation,
            TokenKind::Star,
            TokenKind::Slash,
            TokenKind::BackSlash,
            TokenKind::Percent
        >();
    }

    ast::ExprPtr exponentiation() {
        PPGA_PARSER_LOG(exponentiation);
        return parse_binary<&Parser::unary, &Parser::exponentiation, TokenKind::Pow>();
    }

    ast::ExprPtr unary() {
        PPGA_PARSER_LOG(unary);
        if (match<TokenKind::Minus, TokenKind::Not, TokenKind::Ellipsis>()) {
            auto op = previous().lexeme();
            auto value = unary();
            return std::make_unique<ast::Unary>(op, std::move(value));
        }

        PPGA_PARSER_LOG_EXPR("leaving unary");
        return call();
    }

    ast::ExprPtr call() {
        PPGA_PARSER_LOG(call);
        auto expr = match<TokenKind::Len>() ? finish_len() : primary();

        while (match<TokenKind::LeftParen, TokenKind::Dot, TokenKind::Colon, TokenKind::LeftBracket>()) {
            auto kind = previous().kind();
            switch (kind) {
                case TokenKind::LeftParen:
                    expr = finish_call(std::move(expr));
                    break;
                case TokenKind::LeftBracket:
                    expr = finish_item(std::move(expr));
                    break;
                default:
                    expr = finish_attr(std::move(expr), kind == TokenKind::Colon);
            }

            while (check(TokenKind::Identifier)) {
                if (peek().lexeme() != "err") {
                    break;
                }
                auto _err = advance();
                try_consume(TokenKind::LeftBrace, "Expected a `{` after the `err` in an err block");
                auto body = block(false);
                // Generate an err block
                ast::ExprPtr callback = std::make_unique<ast::Lambda>(ast::FunctionData {
                    std::nullopt,
                    utils::make_vector<ast::ExprPtr>(make_owned_var("err")),
                    std::move(body)
                });
                expr = std::make_unique<ast::Call>(
                    make_var(std::string_view(constants::ERR_HANDLER_NAME)),
                    utils::make_vector<ast::ExprPtr>(
                        std::move(callback),
                        std::move(expr)
                    )
                );
            }
        }

        PPGA_PARSER_LOG_EXPR("leaving call");
        return expr;
    }

    ast::ExprPtr primary() {
        PPGA_PARSER_LOG(primary);
        auto token = advance();

        ast::ExprPtr expr;
        switch (token.kind()) {
            case TokenKind::Nil:
            case TokenKind::True:
            case TokenKind::False:
            case TokenKind::StringLiteral: [[fallthrough]];
            case TokenKind::Number:{
                expr = std::make_unique<ast::Literal>(
                    token.lexeme(),
                    token.kind() == TokenKind::StringLiteral
                );
                break;
            }
            case TokenKind::Identifier: {
                expr = make_var(token.lexeme());
                break;
            }
            case TokenKind::Variadics: {
                expr = make_var("...");
                break;
            }
            case TokenKind::LeftParen: {
                expr = std::make_unique<ast::Grouping>(expression());
                try_consume(TokenKind::RightParen, "Expected a `)` after the expression");
                break;
            }
            case TokenKind::LeftBracket: {
                expr = std::make_unique<ast::ArrayLiteral>(arguments(TokenKind::RightBracket));
                try_consume(TokenKind::RightBracket, "Expected a `]` at the end of the array literal.");
                break;
            }
            case TokenKind::LeftBrace: {
                expr = std::make_unique<ast::DictLiteral>(pairs());
                try_consume(TokenKind::RightBrace, "Expected a `}` at the end of the dict literal");
                break;
            }
            case TokenKind::FString: {
                expr = finish_fstring(token.get_fstring_payload());
                break;
            }
            case TokenKind::Lua: {
                expr = std::make_unique<ast::LuaBlock>(token.get_lua_payload());
                break;
            }
            case TokenKind::EndOfFile: {
                std::stringstream s;
                s << "Reached the end of the script, last see token was "
                  << previous().kind() << " `" <<  previous().lexeme() << "`";
                error(previous(), s.str());
                throw parser_exception();
            }
            default: {
                std::stringstream s;
                s << "Unexpected symbol " << token.kind();
                error(previous(), s.str());
                throw parser_exception();
            }
        }


        PPGA_PARSER_LOG_EXPR("leaving primary");
        return expr;
    }

    ast::ExprPtr finish_fstring(std::vector<FStringFragment>& frags) {
        PPGA_PARSER_LOG(finish_fstring);
        std::vector<ast::ExprPtr> exprs{};

        for (const auto& frag : frags) {
            if (frag.is_string) {
                // Skip empty strings
                if (frag.string_fragment.empty()) continue;
                exprs.push_back(std::make_unique<ast::Literal>(frag.string_fragment, true));
            } else {
                std::vector<Token> tokens_ = frag.inner_tokens;
                Parser parser = Parser(std::move(tokens_), config);
                try {
                    exprs.push_back(parser.expression());
                } catch (parser_exception&) {
                    ex.extend(parser.ex);
                }
            }
        }

        return std::make_unique<ast::FString>(std::move(exprs));
    }

    ast::ExprPtr finish_call(ast::ExprPtr callee) {
        PPGA_PARSER_LOG(finish_call);
        std::vector<ast::ExprPtr> args;
        if (!check(TokenKind::RightParen)) {
            try {
                args = arguments(TokenKind::RightParen);
            } catch (parser_exception& e) {
                try_consume(TokenKind::RightParen, "Expected a `)` after the argument list");
                throw e;
            }
        } else {
            args = std::vector<ast::ExprPtr>{};
        }
        consume(TokenKind::RightParen, "Expected a `)` after the argument list");

        PPGA_PARSER_LOG_EXPR("leaving fstring");
        return std::make_unique<ast::Call>(std::move(callee), std::move(args));
    }

    std::vector<ast::ExprPtr> arguments(TokenKind stop) {
        PPGA_PARSER_LOG(arguments);
        std::vector<ast::ExprPtr> args{};

        if (!check(stop)) {
            args.push_back(expression());
        }

        while (match<TokenKind::Comma>() && !check(stop)) {
            args.push_back(expression());
        }

        PPGA_PARSER_LOG_EXPR("leaving arguments");
        return args;
    }

    std::vector<ast::ExprPtr> parameters() {
        PPGA_PARSER_LOG(parameters);
        std::vector<ast::ExprPtr> params{};

        if (!check(TokenKind::RightParen)) {
            auto name = consume_identifier("Expected an identifier");
            // XXX: the C++ version doesn't parse optional identifiers
            // auto optional = match<TokenKind::Query>()
            params.push_back(std::make_unique<ast::Variable>(name.lexeme()));
        }

        while (match<TokenKind::Comma>() && !check(TokenKind::RightParen)) {
            auto name = consume_identifier("Expected an identifier");
            params.push_back(std::make_unique<ast::Variable>(name.lexeme()));
        }

        PPGA_PARSER_LOG_EXPR("leaving parameters");
        return params;
    }

    std::vector<std::pair<ast::ExprPtr, ast::ExprPtr>> pairs() {
        std::vector<std::pair<ast::ExprPtr, ast::ExprPtr>> pairs{};

        if (!check(TokenKind::RightBrace)) {
            auto key = expression();
            try_consume(TokenKind::Equal, "Expected an `=` after the key");
            auto value = expression();
            pairs.emplace_back(std::move(key), std::move(value));
        }

        while (match<TokenKind::Comma>() && !check(TokenKind::RightBrace)) {
            auto key = expression();
            try_consume(TokenKind::Equal, "Expected an `=` after the key");
            auto value = expression();
            pairs.emplace_back(std::move(key), std::move(value));
        }

        return pairs;
    }

    ast::ExprPtr finish_len() {
        try_consume(TokenKind::LeftParen, "Expected a `(` before the len argument");
        auto expr = expression();
        try_consume(TokenKind::RightParen, "Expected a `)` after the len argument");
        return std::make_unique<ast::Len>(std::move(expr));
    }

    ast::ExprPtr finish_item(ast::ExprPtr obj) {
        auto key = expression();
        try_consume(TokenKind::RightBracket, "Expected a `]` after the item");
        return std::make_unique<ast::GetItem>(std::move(obj), std::move(key));
    }

    ast::ExprPtr finish_attr(ast::ExprPtr obj, bool is_static) {
        auto attr = consume_identifier("Expected an attribute name after the dot.");
        return std::make_unique<ast::Get>(std::move(obj), attr.lexeme(), is_static);
    }

    static inline ast::ExprPtr make_var(std::string_view name) {
        return std::make_unique<ast::Variable>(name);
    }

    static inline ast::ExprPtr make_owned_var(std::string&& name) {
        return std::make_unique<ast::GeneratedVariable>(std::move(name));
    }

    Token& consume_identifier(std::string&& message) {
        if (match<TokenKind::Variadics, TokenKind::Identifier>()) {
            return previous();
        }
        error(std::move(message));
        throw parser_exception();
    }

    template<ast::ExprPtr (Parser::*higher_prec_fn_left)(), ast::ExprPtr (Parser::*higher_prec_fn_right)(), TokenKind... kind>
    inline ast::ExprPtr parse_binary() {
        auto expr = (this->*higher_prec_fn_left)();

        // XXX: Why does the rust version have this is_at_end() check?
        while (match<kind...>() && !is_at_end()) {
            auto op = previous().lexeme();
            auto right = (this->*higher_prec_fn_right)();
            expr = std::make_unique<ast::Binary>(op, std::move(expr), std::move(right));
        }

        return expr;
    }

    template<ast::ExprPtr (Parser::*higher_prec_fn)(), TokenKind... kind>
    inline ast::ExprPtr parse_binary() {
        return parse_binary<higher_prec_fn, higher_prec_fn, kind...>();
    }

    optional_ref<Token> try_consume_semicolon(std::string&& message) noexcept {
        if (check(TokenKind::Semicolon)) {
            while (match<TokenKind::Semicolon>()) {}
            return make_optional_ref(previous());
        }
        error(std::move(message));
        return std::nullopt;
    }

    optional_ref<Token> try_consume(TokenKind kind, std::string&& message) noexcept {
        if (check(kind)) {
            return make_optional_ref(advance());
        }
        error(std::move(message));
        return std::nullopt;
    }

    Token& consume(TokenKind kind, std::string&& message) {
        auto value = try_consume(kind, std::move(message));
        if (value.has_value()) {
            return value.value();
        }
        throw parser_exception();
    }

    Token& consume_semicolon(std::string&& message) {
        auto value = try_consume_semicolon(std::move(message));
        if (value.has_value()) {
            return value.value();
        }
        throw parser_exception();
    }

    void synchronize() {
        advance();
        while (!is_at_end()) {
            if (previous().kind() == lexer::TokenKind::Semicolon) {
                return;
            }
            switch (peek().kind()) {
                case lexer::TokenKind::Fn:
                case lexer::TokenKind::Let:
                case lexer::TokenKind::Global:
                case lexer::TokenKind::For:
                case lexer::TokenKind::ForI:
                case lexer::TokenKind::If:
                case lexer::TokenKind::While:
                case lexer::TokenKind::Return:
                    [[fallthrough]];
                case lexer::TokenKind::LeftBrace:
                    return;
                default:
                    advance();
            }
        }
    }

    [[nodiscard]]
    inline bool check(lexer::TokenKind kind) const noexcept {
        return peek().kind() == kind;
    }

    template<lexer::TokenKind... kinds>
    bool match() noexcept {
        auto curr = peek().kind();
        if (((curr == kinds) || ...)) {
            advance();
            return true;
        }
        return false;
    }

    [[nodiscard]]
    inline lexer::Token& previous() {
        if (current < 1) return tokens.at(current);
        return tokens.at(current - 1);
    }

    inline lexer::Token& advance()  {
        if (current < tokens.size()) ++current;
        return previous();
    }

    [[nodiscard]]
    inline const lexer::Token& peek() const  {
        if (current >= tokens.size()) return tokens.at(tokens.size() - 1);
        return tokens.at(current);
    }

    [[nodiscard]]
    inline bool is_at_end() const noexcept {
        return peek().kind() == lexer::TokenKind::EndOfFile;
    }

    inline void error(std::string&& message) noexcept {
        ex.error(peek().span(), std::move(message));
        PPGA_PARSER_LOG(error_encountered);
        PPGA_PARSER_LOG_EXPR(ex.errors[ex.errors.size() - 1]);
    }

    inline void error(const Token& token, std::string&& message) noexcept {
        ex.error(token.span(), std::move(message));
        PPGA_PARSER_LOG(error_encountered);
        PPGA_PARSER_LOG_EXPR(ex.errors[ex.errors.size() - 1]);
    }

    inline void error(const Span& span, std::string&& message) noexcept {
        ex.error(span, std::move(message));
        PPGA_PARSER_LOG(error_encountered);
        PPGA_PARSER_LOG_EXPR(ex.errors[ex.errors.size() - 1]);
    }
};
} // parser

namespace visitors {
using namespace ppga::ast;

#define INNER(stmt) { ++this->depth; { stmt; } --this->depth; }

class ASTPrinter : public Visitor {
    std::ostringstream ss;
    size_t depth{1};
    size_t indent_size;

public:
    explicit ASTPrinter(size_t indent_size = 2)
        : Visitor(), indent_size(std::max((size_t) 1, indent_size)) {}

    std::string finish() const {
        return ss.str();
    }

    void visit(AST& ast) override {
        for (const auto& stmt : ast.statements) {
            visit(*stmt);
        }
    }

    void visit(Expr& expr) override {
        expr.accept(*this);
    }

    void visit(Stmt& stmt) override {
        stmt.accept(*this);
    }

    void visit(Literal& lit) override {
        ss << "Literal: `" << lit.value << "`\n";
    }

    void visit(Len& len) override {
        ss << "Len\n";
        indent() << "expr: ";
        INNER(visit(*len.expr));
    }

    void visit(LuaBlock&) override {
        ss << "LuaBlock (...)\n";
    }

    void visit(Variable& var) override {
        ss << "Variable: `" << var.name << "`\n";
    }

    void visit(GeneratedVariable& var) override {
        ss << "GeneratedVariable: `" << var.name << "`\n";
    }

    void visit(FString& fstring) override {
        ss << "FString\n";
        for (size_t i = 0; i < fstring.fragments.size(); ++i) {
            indent() << "frag" << i << ": ";
            INNER(visit(*fstring.fragments.at(i)));
        }
    }

    void visit(Get& get) override {
        ss << "Get\n";
        indent() << "is_static: " << (get.is_static ? "true" : "false") << "\n";
        indent() << "attr: " << get.attr << "\n";
        indent() << "obj: ";
        INNER(visit(*get.obj));
    }

    void visit(GetItem& get) override {
        ss << "GetItem\n";
        indent() << "obj: ";  INNER(visit(*get.obj));
        indent() << "item: "; INNER(visit(*get.item));
    }

    void visit(Call& call) override {
        ss << "Call\n";
        indent() << "callee: ";
        INNER(visit(*call.callee));
        for (size_t i = 0; i < call.args.size(); ++i) {
            indent() << "arg" << i << ": ";
            INNER(visit(*call.args.at(i)));
        }
    }

    void visit(Unary& unary) override {
        ss << "Unary `" << unary.op << "`\n";
        indent() << "operand: ";
        INNER(visit(*unary.operand));
    }

    void visit(Binary& binary) override {
        ss << "Binary `" << binary.op << "`\n";
        indent() << "left: ";  INNER(visit(*binary.left));
        indent() << "right: "; INNER(visit(*binary.right));
    }

    void visit(Grouping& grouping) override {
        ss << "Grouping\n";
        indent() << "expr: "; INNER(visit(*grouping.expr));
    }

    void visit(ArrayLiteral& arr) override {
        ss << "ArrayLiteral\n";
        for (size_t i = 0; i < arr.values.size(); ++i) {
            indent() << "val" << i << ": ";
            INNER(visit(*arr.values.at(i)));
        }
    }

    void visit(DictLiteral& dict) override {
        ss << "DictLiteral\n";
        for (size_t i = 0; i < dict.pairs.size(); ++i) {
            indent() << "key" << i << ": "; INNER(visit(*dict.pairs.at(i).first));
            indent() << "val" << i << ": "; INNER(visit(*dict.pairs.at(i).second));
        }
    }

    void visit(Lambda& lambda) override {
        ss << "Lambda\n";
        visit(lambda.data);
    }

    void visit(ExprStmt& expr) override {
        ss << "ExprStmt\n";
        indent() << "expr: ";
        INNER(visit(*expr.expr));
    }

    void visit(If& conditional) override {
        ss << "If\n";
        indent() << "condition: "; INNER(visit(*conditional.condition));
        indent() << "then: ";      INNER(visit(*conditional.then));
        indent() << "else: ";
        if (conditional.otherwise.has_value()) {
            INNER(visit(*conditional.otherwise.value()));
        } else {
            ss << " (none)\n";
        }
    }

    void visit(For& loop) override {
        ss << "For\n";
        indent() << "is_fori: " << (loop.is_fori ? "true" : "false") << "\n";
        indent() << "vars:\n";
        for (size_t i = 0; i < loop.vars.size(); ++i) {
            INNER({
                indent() << "var" << i << ": ";
                INNER(visit(*loop.vars.at(i)));
            });
        }
        indent() << "condition: ";
        std::visit([this](const auto& arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, Range>) {
                indent() << "ForCondition::Range\n";
                INNER({
                    indent() << "start: " << arg.start << "\n";
                    indent() << "step: " << arg.step << "\n";
                    indent() << "stop: " << arg.end << "\n";
                });
            } else if constexpr (std::is_same_v<T, std::vector<ExprPtr>>) {
                indent() << "ForCondition::Exprs\n";
                INNER({
                    for (size_t i = 0; i < arg.size(); ++i) {
                        indent() << "expr" << i << ": ";
                        INNER(visit(*arg.at(i)));
                    }
                });
            } else {
                static_assert(utils::always_false_v<T> , "non-exhaustive visitor!");
            }
        }, loop.condition);
        indent() << "body: "; INNER(visit(*loop.body));
    }

    void visit(While& loop) override {
        ss << "While\n";
        indent() << "condition: "; INNER(visit(*loop.condition));
        indent() << "body: "; INNER(visit(*loop.body));
    }

    void visit(Block& block) override {
        ss << "Block\n";
        indent() << "is_standalone: " << block.is_standalone << "\n";
        indent() << "statements:\n";
        INNER({
            for (const auto& stmt : block.statements) {
                indent();
                INNER(visit(*stmt));
            }
        });
    }

    void visit(StmtSequence& seq) override {
        ss << "StmtSequence\n";
        INNER({
            for (const auto& stmt : seq.statements) {
                indent();
                INNER(visit(*stmt));
            }
        });
    }

    void visit(Return& ret) override {
        ss << "Return\n";
        for (size_t i = 0; i < ret.values.size(); ++i) {
            indent() << "retval" << i << ": ";
            INNER(visit(*ret.values.at(i)));
        }
    }

    void visit(FuncDecl& decl) override {
        ss << "FuncDecl\n";
        indent() << "kind: " << (decl.kind == VarKind::Local ? "local" : "global") << "\n";
        visit(decl.data);
    }

    void visit(VarDecl& decl) override {
        ss << "VarDecl\n";
        indent() << "kind: " << (decl.kind == VarKind::Local ? "VarKind::Local" : "VarKind::Global") << "\n";
        for (size_t i = 0; i < decl.names.size(); ++i) {
            indent() << "name" << i << ": " << decl.names[i] << "\n";
        }
        indent() << "initializer: ";
        if (decl.initializer.has_value()) {
            INNER(visit(*decl.initializer.value()));
        } else {
            indent() << " (none)";
        }
    }

    void visit(Assignment& ass) override {
        ss << "Assignment `" << ass.op << "`\n";
        indent() << "value: "; INNER(visit(*ass.value));
        for (size_t i{}; i < ass.vars.size(); ++i) {
            indent() << "var" << i << ": ";
        INNER(visit(*ass.vars.at(i)));
        }
    }

    void visit(Break&) override {
        ss << "Break";
    }

    void visit(FunctionData& data) {
        indent() << "name: " << data.name.value_or("<anon>") << "\n";
        for (size_t i = 0; i < data.params.size(); ++i) {
            indent() << "param" << i << ": ";
            INNER(visit(*data.params.at(i)));
        }
        indent() << "body: "; INNER(visit(*data.body));
    }

    std::ostringstream& indent() noexcept {
        size_t count = depth * indent_size;
        while (count-- > 0) {
            ss << " ";
        }
        return ss;
    }
};

#define SEP_COMMA(i, arr)    if ((i) != (arr).size() - 1) { ss << ", "; }

class LuaTranspiler : public Visitor {
    std::stringstream ss{};
    size_t depth{0};
    size_t indent_size;
    bool include_std;

public:
    explicit LuaTranspiler(PPGAConfig config)
            : Visitor(),
              indent_size(std::max((size_t) 1, config.indent_size)),
              include_std(config.include_ppga_std) {}

    std::string finish() const {
        return ss.str();
    }

    void visit(AST& ast) override {
        if (this->include_std) {
            ss << "-- PPGA STD SYMBOLS\n";
            ss << constants::DEFAULT_OP_DEFN << "\n";
            ss << constants::ERR_HANDLER_DEFN << "\n";
            ss << constants::ERR_CALLBACK_DEFN << "\n";
            ss << "-- END PPGA STD SYMBOLS\n\n\n";
        }
        for (const auto& stmt : ast.statements) {
            visit(*stmt);
        }
    }

    void visit(Expr& expr) override {
        expr.accept(*this);
    }

    void visit(Stmt& stmt) override {
        stmt.accept(*this);
    }

    void visit(Literal& lit) override {
        if (lit.is_string) {
            ss << "\"";
            if (lit.value.at(0) == '\"' || lit.value.at(0) == '\'') {
                ss << lit.value.substr(1, lit.value.length() - 2);
            } else {
                ss << lit.value;
            }
            ss << "\"";
        } else {
            ss << lit.value;
        }
    }

    void visit(Len& len) override {
        ss << "#("; visit(*len.expr); ss << ")";
    }

    void visit(LuaBlock& lua) override {
        ss << lua.contents; // TODO: do this correctly
    }

    void visit(Variable& var) override {
        ss << var.name;
    }

    void visit(GeneratedVariable& var) override {
        ss << var.name;
    }

    void visit(FString& fstring) override {
        for (size_t i = 0; i < fstring.fragments.size(); ++i) {
            const auto& frag = fstring.fragments[i];
            if (auto lit = dynamic_cast<ast::Literal*>(frag.get()); lit != nullptr && lit->is_string) {
                visit(*frag);
            } else {
                ss << "tostring("; visit(*frag); ss << ")";
            }
            if (i != fstring.fragments.size() - 1) {
                ss << " .. ";
            }
        }
    }

    void visit(Get& get) override {
        visit(*get.obj);
        ss << (get.is_static ? ":" : ".");
        ss << get.attr;
    }

    void visit(GetItem& get) override {
        visit(*get.obj);
        ss << "["; visit(*get.item); ss << "]";
    }

    void visit(Call& call) override {
        visit(*call.callee);
        ss << "(";
        for (size_t i = 0; i < call.args.size(); ++i) {
            visit(*call.args[i]);
            SEP_COMMA(i, call.args);
        }
        ss << ")";
    }

    void visit(Unary& unary) override {
        if (unary.op == "...") {
            visit(*unary.operand);
        } else {
            ss << unary.op << "(";
            visit(*unary.operand);
            ss << ")";
        }
    }

    void visit(Binary& binary) override {
        visit(*binary.left);

        auto op = std::string(binary.op);
        if (binary.op == "\\") op = "//";
        else if (binary.op == "**") op = "^";
        else if (binary.op == "!=") op = "~=";
        ss << " " << op << " ";

        visit(*binary.right);
    }

    void visit(Grouping& grouping) override {
        ss << "("; visit(*grouping.expr); ss << ")";
    }

    void visit(ArrayLiteral& arr) override {
        ss << "{";
        for (size_t i = 0; i < arr.values.size(); ++i) {
            ss << "[" << i << "] = "; visit(*arr.values[i]);
            SEP_COMMA(i, arr.values);
        }
        ss << "}";
    }

    void visit(DictLiteral& dict) override {
        ss << "{";
        if (!dict.pairs.empty()) {
            ss << "\n";
        }
        for (size_t i = 0; i < dict.pairs.size(); ++i) {
            INNER({
                indent() << "["; visit(*dict.pairs[i].first); ss << "] = ";
                visit(*dict.pairs[i].second);
            });
            if (i != dict.pairs.size() - 1) {
                ss << ",\n";
            }
        }
        if (dict.pairs.empty()) {
            ss << "}";
        } else {
            ss << "\n"; indent(); ss << "}";
        }
    }

    void visit(Lambda& lambda) override {
        visit(lambda.data, VarKind::Local);
    }

    void visit(ExprStmt& expr) override {
        indent();
        visit(*expr.expr);
        ss << "\n";
    }

    void visit(If& conditional) override {
        if (!conditional.is_elif) {
            indent();
        }
        ss << "if "; visit(*conditional.condition);
        ss << " then\n"; visit(*conditional.then);

        if (conditional.otherwise.has_value()) {
            const auto& el = conditional.otherwise.value();
            // We need to remove the word `end` written by the previous block.
            // To achieve this, we move the write pointer by 3 characters to the left, thus positioning it
            // right before the `end`:
            //              ,-- seekp
            //    <...> end\n
            //          ^-- seekp - 4
            //
            ss.seekp(-4, std::stringstream::cur);
            // Now we can overwrite it with an `else` block:
            ss << "else";
            if (auto el_val = dynamic_cast<If*>(el.get()); el_val != nullptr) {
                el_val->is_elif = true;
            } else {
                ss << "\n";
            }
            visit(*el);
        }
    }

    void visit(For& loop) override {
       write_newline_if_missing();
       indent() << "for ";
        std::visit([&, this](const auto& arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, Range>) {
                visit(*loop.vars[0]);
                ss << " = " << arg.start << ", " << arg.end << ", " << arg.step;
            } else if constexpr (std::is_same_v<T, std::vector<ExprPtr>>) {
                for (size_t i = 0; i < loop.vars.size(); ++i) {
                    visit(*loop.vars[i]);
                    SEP_COMMA(i, loop.vars);
                }
                ss << " in ";
                bool is_pairs_loop = arg.size() == 1 && loop.vars.size() == 2;
                if (is_pairs_loop) {
                    ss << (loop.is_fori ? "ipairs" : "pairs") << "(";
                }
                for (size_t i = 0; i < arg.size(); ++i) {
                    visit(*arg[i]);
                    SEP_COMMA(i, arg);
                }
                if (is_pairs_loop) ss << ")";
            } else {
                static_assert(utils::always_false_v<T> , "non-exhaustive visitor!");
            }
        }, loop.condition);

        ss << " do\n";
        indent(); visit(*loop.body);
    }

    void visit(While& loop) override {
        write_newline_if_missing();
        indent() << "while "; visit(*loop.condition); ss << " do\n";
        indent(); visit(*loop.body);
    }

    void visit(Block& block) override {
        if (block.is_standalone) {
            indent() << "do\n";
        }
        INNER({
            for (const auto& stmt : block.statements) {
                visit(*stmt);
            }
        });
        indent() << "end\n";
    }

    void visit(StmtSequence& seq) override {
        for (const auto& stmt : seq.statements) {
            visit(*stmt);
        }
    }

    void visit(Return& ret) override {
        indent() << "return ";
        for (size_t i = 0; i < ret.values.size(); ++i) {
            const auto& v = ret.values[i];
            if (auto unop = dynamic_cast<Unary*>(v.get()); unop != nullptr && unop->op == "...") {
                visit(*v);
            } else {
                ss << "("; visit(*v); ss << ")";
            }
            SEP_COMMA(i, ret.values);
        }
        ss << "\n";
    }

    void visit(FuncDecl& decl) override {
        write_newline_if_missing();
        indent();
        visit(decl.data, decl.kind);
        ss << "\n";
    }

    void visit(Assignment& ass) override {
        indent();
        for (size_t i = 0; i < ass.vars.size(); ++i) {
            visit(*ass.vars[i]);
            SEP_COMMA(i, ass.vars);
        }
        if (ass.op == "*==") {
            ss << " = "; visit(*ass.vars.at(0));
            ss << " ^ ";
        } else {
            ss << " " << ass.op << " ";
        }
        visit(*ass.value);
        ss << "\n";
    }

    void visit(VarDecl& decl) override {
        indent();
        if (decl.kind == VarKind::Local) {
            ss << "local ";
        }
        for (size_t i = 0; i < decl.names.size(); ++i) {
            ss << decl.names[i];
            SEP_COMMA(i, decl.names);
        }
        if (decl.initializer.has_value()) {
            ss << " = "; visit(*decl.initializer.value());
        }
        ss << "\n";
    }

    void visit(Break&) override {
        indent() << "break\n";
    }

    void visit(FunctionData& fn, VarKind kind) {
        ss << (fn.name.has_value() && kind == VarKind::Local ? "local " : "")
           << "function " << fn.name.value_or("") << "(";
        for (size_t i{}; i < fn.params.size(); ++i) {
            if (auto param = dynamic_cast<Variable*>(fn.params[i].get()); param != nullptr) {
                ss << (param->name == "@" ? "..." : param->name);
            } else if (auto gen_param = dynamic_cast<GeneratedVariable*>(fn.params[i].get()); gen_param != nullptr) {
                ss << (gen_param->name == "@" ? "..." : gen_param->name);
            } else {
                assert(param != nullptr && gen_param != nullptr && "function param is not a variable");
            }
            SEP_COMMA(i, fn.params);
        }
        ss << ")\n";
        visit(*fn.body);
        if (!fn.name) {
            // Get rid of the newline if this is a lambda
            ss.seekp(-1, std::stringstream::cur);
        }
    }

    void inline write_newline_if_missing() {
        if (ss.peek() != '\n') ss << '\n';
    }

    std::stringstream& indent() noexcept {
        size_t count = depth * indent_size;
        while (count-- > 0) {
            ss << " ";
        }
        return ss;
    }
};


#undef SEP_COMMA
#undef INNER
} // visitors

inline std::optional<ppga::ast::AST> ppga_parse(
    const std::string_view source,
    ppga::error::ErrCtx& ex,
    PPGAConfig config = PPGAConfig{})
{
    auto lexer = ppga::lexer::Lexer(source);
    auto tokens = lexer.lex(ex);
    if (ex.had_error()) {
        return std::nullopt;
    }
    auto parser = ppga::parser::Parser(std::move(tokens.value()), config);
    return parser.parse(ex);
}

inline std::optional<std::string> ppga_to_lua(
    const std::string_view source,
    ppga::error::ErrCtx& ex,
    PPGAConfig config = PPGAConfig{}
)
    noexcept
{
    auto ast = ppga_parse(source, ex, config);
    if (!ast.has_value()) {
        return std::nullopt;
    }

    auto lt = ppga::visitors::LuaTranspiler(config);
    lt.visit(ast.value());
    return lt.finish();
}

inline std::string ppga_to_lua(
    const std::string_view source,
    PPGAConfig config = PPGAConfig{}) /* throws std::runtime_error */
{
    auto ex = ppga::error::ErrCtx{};
    auto result = ppga_to_lua(source, ex, config);
    ex.raise();
    return std::move(result.value());
}
}

#endif //PPGA_SCRIPT_PPGA_HPP
