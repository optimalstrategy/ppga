#ifndef PPGA_SCRIPT_PPGA_HPP
#define PPGA_SCRIPT_PPGA_HPP

#include <cstddef>
#include <string>
#include <cstring>
#include <sstream>
#include <utility>
#include <vector>
#include <optional>
#include <variant>
#include <iostream>
#include <algorithm>

namespace ppga {
namespace constants {
    using PPGANumber = double;
    /// The number of indentation spaces in the resulting lua code.
    static size_t DEFAULT_PPGA_INDENT_SIZE = 4;
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
};
}

namespace error {
struct PPGAError {
    lexer::Span span;
    std::string description;
};

struct ErrCtx {
    std::vector<PPGAError> errors;

    void error(lexer::Span span, std::string&& description) {
        errors.push_back(PPGAError {
            span,
            std::move(description)
        });
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

std::ostream& operator<<(std::ostream& out, const TokenKind kind) {
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

class FStringFragment {
    std::vector<Token> inner_tokens;
    std::string_view string_fragment;
    bool is_string;
public:
    explicit FStringFragment(std::string_view view)
        : string_fragment(view), is_string(true) {}

    explicit FStringFragment(std::vector<Token> tokens)
        : inner_tokens(std::move(tokens)), is_string(false) {}

    explicit FStringFragment(std::vector<Token>&& tokens)
            : inner_tokens(std::move(tokens)), is_string(false) {}
};

class Token {
    Span span;
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
        : span(span), kind_(kind), payload(std::nullopt) {}

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
        return span.slice();
    }

    [[nodiscard]]
    constexpr inline bool has_payload() const noexcept {
        return payload.has_value();
    }

    [[nodiscard]]
    constexpr inline TokenKind kind() const noexcept {
        return kind_;
    }

    static const std::unordered_map<std::string_view, TokenKind> KEYWORDS;
};

class Lexer {
    std::string_view source;
    size_t current;
    size_t line;
    std::vector<Token> tokens;
public:
    explicit Lexer(std::string_view source)
            : source(source), current(0), line(1), tokens(std::vector<Token>{}) {}

    std::vector<Token> lex(error::ErrCtx& ex) noexcept {
        while (!is_at_end()) {
            tokens.push_back(next_token(ex));
        }
        // Push the EOF token
        // tokens.push_back(next_token(ex));
        return std::move(this->tokens);
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
            case '=': CHOOSE('=', TokenKind::Equal, TokenKind::Eq);
            case '-': CHOOSE('=', TokenKind::Minus, TokenKind::MinusEqual);
            case '+': CHOOSE('=', TokenKind::Plus, TokenKind::PlusEqual);
            case '%': CHOOSE('=', TokenKind::Percent, TokenKind::PercentEqual)
            case '<': CHOOSE('=', TokenKind::Lt, TokenKind::Le);
            case '>': CHOOSE('=', TokenKind::Gt, TokenKind::Ge);
            case '\\': CHOOSE('=', TokenKind::BackSlash, TokenKind::BackSlashEqual);
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
        auto found = Token::KEYWORDS.find(name);
        if (found != Token::KEYWORDS.cend()) {
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
                    // Bump the end value to include the character before the }.
                    // This is needed because `end` is exclusive.
                    span.end += 1;
                    auto sublexer = Lexer(span.slice());
                    std::vector<Token> frag_tokens = sublexer.lex(ex);
                    frags.emplace_back(frag_tokens);
                    prev_fragment_end = current + 1;
                }
            }
            advance();
        }

        if (!match(quote)) {
            auto span = this->span(start, start_line);
            ex.error(span, "Unterminated f-string");
            return Token(span, TokenKind::Error);
        }

        if (prev_fragment_end < source.length() - 1) {
            auto span = this->span(prev_fragment_end);
            frags.emplace_back(span.slice());
        }

        auto span = this->span(start, start_line);
        return Token::fstring(span, TokenKind::FString, std::move(frags));
    }

    /// Scans a single-line comment.
    inline void comment() noexcept {
        while (!match('\n')) {
            advance();
        }
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

const std::unordered_map<std::string_view, TokenKind> Token::KEYWORDS = {
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
    {"not"sv, TokenKind::Nil}
};
}

namespace ast {
template<class T>
class Visitor;

template<class T>
struct Node {
    virtual T accept(Visitor<T>& visitor) = 0;
};

template<class T>
struct Stmt : public Node<T> {};

template<class T>
struct Expr : public Node<T> {};

template<class T>
struct FunctionData {
    std::optional<std::string_view> name;
    std::vector<Expr<T>> params;
    std::unique_ptr<Stmt<T>> body;
};

template<class T>
struct Literal : public Expr<T> {
    std::string_view value;
};

template<class T>
struct Len : public Expr<T> {
    std::unique_ptr<Expr<T>> expr;
};

template<class T>
struct LuaBlock : public Expr<T> {
    std::string_view contents;
};

template<class T>
struct Variable : public Expr<T> {
    std::string_view name;
};

template<class T>
struct GeneratedVariable : public Expr<T> {
    std::string name;
};

template<class T>
struct Param : public Expr<T> {
    std::string_view name;
};

template<class T>
struct FString : public Expr<T> {
     std::vector<Expr<T>> fragments;
}

template<class T>
struct Get : public Expr<T> {
    std::unique_ptr<Expr<T>> obj;
    std::string_view attr;
    bool is_static;
};

template<class T>
struct GetItem : public Expr<T> {
    std::unique_ptr<Expr<T>> obj;
    std::unique_ptr<Expr<T>> item;
};

template<class T>
struct Call : public Expr<T> {
    std::unique_ptr<Expr<T>> callee;
    std::vector<Expr<T>> args;
};

template<class T>
struct Unary : public Expr<T> {
    std::string_view op;
    std::unique_ptr<Expr<T>> operand;
};

template<class T>
struct Grouping : public Expr<T> {
    std::unique_ptr<Expr<T>> expr;
};

template<class T>
struct Binary : public Expr<T> {
    std::string_view op;
    std::unique_ptr<Expr<T>> left;
    std::unique_ptr<Expr<T>> right;
};

template<class T>
struct ArrayLiteral : public Expr<T> {
    std::vector<Expr<T>> values;
};

template<class T>
struct DictLiteral : public Expr<T> {
    std::vector<std::pair<Expr<T>, Expr<T>>> pairs;
};

template<class T>
struct Lambda : public Expr<T> {
    std::unique_ptr<FunctionData<T>> lambda;
};

template<class T>
struct ExprStmt : public Stmt<T> {
    std::unique_ptr<Expr<T>> expr;
};

template<class T>
struct If : public Stmt<T> {
    std::unique_ptr<Expr<T>> condition;
    std::unique_ptr<Stmt<T>> then;
    std::optional<std::unique_ptr<Expr<T>>> otherwise;
};

struct Range {
    constants::PPGANumber start;
    constants::PPGANumber end;
    constants::PPGANumber step;
};

template<class T>
struct For : public Stmt<T> {
    bool is_fori;
    std::vector<Expr<T>> vars;
    std::variant<
        Range, // A range
        std::vector<Expr<T>> // A sequence of expressions
    > condition;
};

template<class T>
struct While : public Stmt<T> {
    std::unique_ptr<Expr<T>> condition;
    std::unique_ptr<Stmt<T>> body;
};

template<class T>
struct Block : public Stmt<T> {
    std::vector<Expr<T>> statements;
    bool is_standalone;
};

/// Used for inserting multiple statements without generating a block.
template<class T>
struct StmtSequence : public Stmt<T> {
    std::vector<Expr<T>> statements;
};

template<class T>
struct Return: public Stmt<T> {
    std::vector<Expr<T>> values;
};

template<class T>
struct Assignment: public Stmt<T> {
    std::vector<Expr<T>> vars;
    std::string_view op;
    std::unique_ptr<Expr<T>> value;
};

enum class VarKind {
    Local,
    Global
};

template <class T>
struct FuncDecl : public Stmt<T> {
    FunctionData<T> data;
    VarKind kind;
};

template<class T>
struct VarDecl : public Stmt<T> {
    VarKind kind;
    std::vector<std::string> names;
    std::optional<std::unique_ptr<Expr<T>>> initializer;
};

enum class MatchPatKind {
    /// Compare the bound variable to the value of the pattern
    Comparison,
    /// Use the value of the pattern as the condition
    Value,
    /// The final else block
    Else,
};

template<class T>
struct AST  {
    PPGAConfig config;
    std::vector<Stmt<T>> statements;
};

template<class T>
class Visitor {
public:
    virtual T visit(AST<T>& ast)  = 0;
    virtual T visit(Expr<T>& expr) { expr.accept(*this); }
    virtual T visit(Stmt<T>& stmt) { stmt.accept(*this); }
    virtual T visit(If<T>& stmt) = 0;
    virtual T visit(Literal<T>& lit) = 0;
    virtual T visit(ExprStmt<T>& stmt) = 0;
};
}

namespace parser {
class Parser {
    std::vector<lexer::Token> tokens;
    size_t current;
    ppga::PPGAConfig config;

public:
    Parser(std::vector<lexer::Token>&& tokens, PPGAConfig config)
    : tokens(std::move(tokens)), current(0), config(config) {}

    std::optional<ast::AST>
};
}


std::string ppga_to_lua(const std::string& source, PPGAConfig ) {
    auto ex = ppga::error::ErrCtx{};
    auto lexer = ppga::lexer::Lexer(source);
    auto tokens = lexer.lex(ex);
    for (const auto& tok : tokens) {
        std::cout << "Token: " << tok.kind() << " \"" << tok.lexeme() << "\"\n";
    }
    return "";
}

}

#endif //PPGA_SCRIPT_PPGA_HPP
