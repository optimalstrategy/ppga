# PPGA Script
PPGA Script is a scripting language that transpiles to Lua. It provides a more familiar C-style syntax and syntactic sugar, designed to reduce the amount of boilerplate when writing scripting commands in lua.

See [tour.ppga](tour.ppga) for a quick tour.

## Binary Installation
* You may clone the repo and build from source, then use the scripts `ppga.sh` and `ppga.bat`.

OR

* You may install the binary with `cargo install` from this repo:

    ```bash
    $ cargo install --git https://github.com/OptimalStrategy/ppga/ --features=build-binary
    ```

## Library
There's two implementations of the transpiler, in [Rust](ppga) and [C++](./ppga-cpp/README.md).

## Syntax Highlighting
There's a syntax highlighting plugin for vscode. It maybe installed from the [vscode-ppga](vscode-ppga) directory.


## Language Reference

<table>
<thead>
<tr>
<th>Feature</th>
<th>Example</th>
<th>Generated Lua</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td>
Literals
</td>
<td>
<pre lang="lua">1;
3.141592; 
true; 
false; 
nil; 
"a string";</pre></td>
<td>
<pre lang="lua">
1
3.141592
true
false
nil
"a string"
</pre>
</td>
<td>
None.
</td>
</tr>
<tr>
<td>Array and Dict literals</td>
<td><pre lang="lua">
// Arrays use python-style syntax and are initialized from 0.
let arr = [1, 2, 3];

// Dicts are similar to Lua but don't require the `[]`
let dict = {1 = 2, 3 = 4};

// Indexing uses the [] syntax: 
let empty = {};
empty["string"] = "hello";
</pre></td>
<td><pre lang="lua">
local arr = {[0] = 1, [1] = 2, [2] = 3}

local dict = {
    [1] = 2,
    [3] = 4
}

local empty = {}
empty["string"] = "hello"
</pre></td>
<td>None.</td>
</tr>
<tr>
<td>F-strings</td>
<td><pre lang="lua">
print(f"{a} + {b} = {a + b}");
print(f"\{escaped}");
</pre></td>
<td><pre lang="lua">
print(tostring(a) .. " + " .. tostring(b) .. " = " .. tostring(a + b))
print("{escaped}")
</pre></td>
<td>An interpolated string expression that compiles to concatenation of string literals and <code>tostring()</code> calls. A backslash can be used to escape formatter brackets: <code>\{}</code>.</td>
</tr>
<tr>
<td>Arithmetic Expressions</td>
<td><pre lang="lua">
print(1 + 2 * 3 / 4 ** 5 % 10);
</pre></td>
<td><pre lang="lua">
print(1 + 2 * 3 / 4 ^ 5 % 10)
</pre></td>
<td>None.</td>
</tr>
<tr>
<td>Integer Division</td>
<td><pre lang="lua">print(1 \ 2);</pre></td>
<td><pre lang="lua">print(1 // 2)</pre></td>
<td>Integer division operator.</td>
</tr>
<tr>
<td>Comparison and Equality operators</td>
<td><pre lang="lua">
print(3 < 4, 5 <= 6, 8 > 7, 9 >= 8, 10 != 11, 7 == 7);
</pre></td>
<td><pre lang="lua">
print(3 < 4, 5 <= 6, 8 > 7, 9 >= 8, 10 ~= 11, 7 == 7)
</pre></td>
<td>None.</td>
</tr>
<tr>
<td>Logic operators </td>
<td><pre lang="lua">print(true and false or true);</pre></td>
<td><pre lang="lua">print(true and false or true)</pre></td>
<td>None.</td>
</tr>
<tr>
<td>Concatenation Operator</td>
<td><pre lang="lua">
print("a" .. "b");
</pre></td>
<td><pre lang="lua">
print("a" .. "b")
</pre></td>
<td>This operator is the same as in lua. Reusing `+` for concatenation is not possible without rolling out a type system.</td>
</tr>
<tr>
<td>Default Operator</td>
<td><pre lang="lua">print(a ?? b);</pre></td>
<td><pre lang="lua">
local function __PPGA_INTERNAL_DEFAULT(x, default) 
    if x ~= nil then return (x) end
    return (default)
end
<br>
print(__PPGA_INTERNAL_DEFAULT(a, b))
</pre></td>
<td>This operator is similar to `??` in C#. If `a` is not `nil`, its value will be returned, otherwise, the `b` value will be returned. This feature requires the PPGA internals included. </td>
</tr>
<tr>
<td>Variable Declarations</td>
<td><pre lang="lua">
let a; 
global b = 4;
</pre></td>
<td><pre lang="lua">
local a
b = 4
</pre></td>
<td>Let bindings correspond to `local` lua variables, while `global` ones transpile to variables without a binding keyword. A `global` variable must be initialized at declaration.</td>
</tr>
<tr>
<td>Function Declarations </td>
<td><pre lang="lua">
global fn f() {}
fn g() {}
fn h(x) => x * x
</pre></td>
<td><pre lang="lua">
function f()
end
<br>
local function g()
end
<br>
local function h(x)
    return (x * x)
end
</pre></td>
<td>All functions are `local` by default. The `global` keyword may be used to make them global. 
The "fat arrow" syntax can be used if the function's body is a single expression.</td>
</tr>
<tr>
<td>Lambda Expressions</td>
<td><pre lang="lua">
print(fn(y, f) {});
print(fn(x) => x * x);
</pre></td>
<td><pre lang="lua">
print(function (y, f)
    end)
print(function (x)
        return (x * x)
    end)
</pre></td>
<td>Lambdas use the same syntax as named functions, except that they don't need an identifier.</td>
</tr>
<tr>
<td>Rest Arguments / Variadics</td>
<td><pre lang="lua">
fn f(a, @) {
    print(a, @);
}
</pre></td>
<td><pre lang="lua">
local function f(a, ...)
    print(a, ...)
end
</pre></td>
<td>Rest arguments use the `@` symbol and transpile to `...`.</td>
</tr>
<tr>
<td>Ellipsis / Unpacking</td>
<td><pre lang="lua">
fn f(x) {
    fn packed() {
        return x, 5;
    }

    if not x {
        // returns the result of packed as a
        return packed();  // => return (packed())
    }

    // unpacks the result of packed() as two values
    return ...packed();  // => return table.unpack({packed()})
}
</pre></td>
<td><pre lang="lua">
local function f(x)
    local function packed()
        return (y), (5)
    end

    if not(x) then
        return (packed())
    end

    return __PPGA_INTERNAL_UNPACK(packed())
end
</pre></td>
<td>Strips the parentheses and unpacks the given expression with `table.unpack`.</td>
</tr>
<tr>
<td>For Loop (ranges)</td>
<td><pre lang="lua">
// From 0 to 3
for i in range(3) {
    print(i);
}

// From 2 to 4
for i in range(2, 4) { 
    print(i);
}

// From 0 to 10 with step = 2
for i in range(0, 10, 2) { 
    print(i);
}
</pre></td>
<td><pre lang="lua">
for i = 0, 3, 1 do
    print(i)
end

for i = 2, 4, 1 do
    print(i)
end

for i = 0, 10, 2 do
    print(i)
end
</pre></td>
<td>For-range loops transpile to Lua range loops</td>
</tr>
<tr>
<td>For Loop (containers)</td>
<td><pre lang="lua">
let container = [1, 2, 3];
container["string"] = "hello";

// Table iteration, uses pairs
for key, value in container {
    print(key, value);
}

// Array iteration, uses ipairs
fori idx, value in container {
    print(idx, value);
}
</pre></td>
<td><pre lang="lua">
local container = {[0] = 1, [1] = 2, [2] = 3}
container["string"] = "hello"

for key, value in pairs(container) do
    print(key, value)
end

for idx, value in ipairs(container) do
    print(idx, value)
end
</pre></td>
<td>For-in loops transpile to `pairs` or `ipairs` depending on the keyword used.</td>
</tr>
<tr>
<td>Error Propagation with `?` and `err` Blocks</td>
<td><pre lang="lua">
fn may_fail(fail) {
    if fail {
        return nil, "error";
    }
    return "success", nil;
}

fn main() {
    // The ? operator simplifies Go-style error handling.
    // By default, this will make the whole program crash if an error is encountered.
    let ok = may_fail(false)?;
    print(f"First result: {ok}");

    // Sometimes it is desirable to log or try to recover from the error.
    // An err block may be used for this purpose:
    let ok = may_fail(true) err {
        print(f"An error has occurred: {err}");
        return ...recovery();
    }?;
    return ok;
}
</pre></td>
<td><pre lang="lua">
local function may_fail(fail)
    if fail then
        return (nil), ("error")
    end
    return ("success"), (nil)
end

local function main()
    local ok = nil
    do
        local _ok_L10S283, _err_L10S283 = __PPGA_INTERNAL_HANDLE_ERR(__PPGA_INTERNAL_DFLT_ERR_CB, may_fail(false))
        if _err_L10S283 ~= nil then
            return (nil), (_err_L10S283)
        end
        ok = _ok_L10S283
    end
    print("First result: " .. tostring(ok))


    local ok = nil
    do
        local _ok_L18S562, _err_L18S562 = __PPGA_INTERNAL_HANDLE_ERR(function (err)
                print("An error has occurred: " .. tostring(err))
                return (unpack(recovery()))
            end, may_fail(true))
        if _err_L18S562 ~= nil then
            return (nil), (_err_L18S562)
        end
        ok = _ok_L18S562
    end
    return (ok)
end
</pre></td>
<td>...</td>
</tr>
</tbody>
</table>






