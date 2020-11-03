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
<td>WIP</td>
<td><pre lang="lua">
...
</pre></td>
<td><pre lang="lua">
...
</pre></td>
<td>...</td>
</tr>
</tbody>
</table>






