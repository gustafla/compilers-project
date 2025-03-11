# Compilers Course Project

A compiler implementation for the [University of Helsinki Spring 2025 Compilers -course](https://hy-compilers.github.io/spring-2025) language.

All basic features and some advanced features are implemented:
- Arithmetic, booleans
- Conditionals and `while`-loops
- Blocks
- Variables
- Type checking
- `break` and `continue` -statements
- Functions

Supported backends:
- x86_64
- Tree-walk interpreter

See [todo.md](todo.md) for other features.

## Building

Building the project requires Rust 1.85.0 or newer.
The build system is `cargo`, which is usually bundled with Rust installations.

To build release binaries of the compiler and the interpreter, run
```
cargo build --release
```
The `compiler` and `interpreter` binaries can be found in `target/release/`.

## Running the compiler

The compiler requires binutils (specifically the `as` and `ld` tools).
To just run the compiler, you can use the cargo command
```
cargo run --release --
```

Append the compiler's arguments after `--`, like so
```
cargo run --release -- --help
```
The above command prints the usage guide and a list of available options.

For example, to compile [programs/test.program](programs/test.program)
and print verbose logs of the compilation, run
```
cargo run --release -- -v programs/test.program
```

By default (without the `-o` argument), the compiler outputs a statically linked
binary to the current working directory, with the same file name as the first
input file, but with file extension removed. So the previous example output the
executable to `./test`.

## Running the interpreter

The interpreter has no system dependencies. Example usage:
```
cargo run --release --bin interpreter -- programs/million.program
```

## Testing

This repository has unit tests for the [tokenizer](src/tokenizer/tests.rs) and
the [parser](src/parser/tests.rs), and [generated](build.rs) end to end tests,
which compile and run programs in the [programs](programs)-directory.

## License

* **[`src/asm/stdlib.s`](src/asm/stdlib.s)**: This file is provided by Martin Pärtel and licensed under the [MIT License](https://opensource.org/license/mit).
* **All other files**: Other source code and files within this project are licensed under the GNU General Public License v3.0 (GPL-3.0), as detailed in the [LICENSE](LICENSE) file.
