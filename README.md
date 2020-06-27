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

## Syntax Highlighting
There's a syntax highlighting plugin for vscode. It maybe installed from the [vscode-ppga](vscode-ppga) directory.
