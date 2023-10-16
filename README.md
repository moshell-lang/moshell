# Moshell

Moshell is a modern shell scripting language with a static type system.
This repository hosts the source code from the parser to the interpreter and its standard library.

## Motivation

The shell is a powerful tool, but it's also a very old one.
While having a permissive language can be a good thing, it makes it harder to statically analyze a shell script for errors.

Due to the highly interpreted nature of the Shell, escaping and quoting arguments can be a pain to get right.
Moshell resolves arguments before evaluating them, so references to variables do not need to be escaped.

Arithmetic evaluation traditionally needs to be made explicit to be differentiated from a command, where Moshell allows direct arithmetic evaluation if not at the beginning at a statement.

Moshell also comes with different data types, such as `Int`, `Float`, `Bool` or `String`. This allows for clearer and more elegant code and avoids arithmetic between incompatible types.

## Installation

Moshell is available from different sources:

- You can download a prebuilt nightly binary with `curl -L moshell.dev/setup.sh | sh`.
- If you have a Rust toolchain, CMake and a C++20 compiler installed, you can build a [development version](#build-and-run).
- Docker users can run a prebuilt image with `docker run -it ghcr.io/moshell-lang/moshell:master`.

## Build and Run

You need a stable Rust compiler to build Moshell frontend on a GNU/Linux system.
The MSRV is the latest stable version of Rust.

The VM needs a C++20 compiler and a CMake 3.15+ installation. Its build script will automatically be called by Cargo.
GCC starting from version 10 and Clang starting from version 11 are supported.

```sh
cargo run # Run the interactive prompt
cargo run -- <file> # Run a file
```

You can also build a release binary:
```sh
cargo build --release
./target/release/moshell # Run the interactive prompt
./target/release/moshell <file> # Run a file
```

You can export the `MOSHELL_STD` environment variable to specify a path to the standard library.

```sh
export MOSHELL_STD=/path/to/moshell/lib
```

## Examples

Here's some Bash vs Moshell comparisons.

Note that Moshell examples cannot currently be run as is.

### Iterating over arguments

#### Bash

```bash
user=''
port=0
while [[ $# -ne 0 ]]; do
   case "$1" in
        -u|--user)
             user="$2"
             shift
             ;;
        -port|--port)
             if [[ "$2" =~ ^[0-9]+$ ]]; then
                 port="$2"
             else
                 echo "Invalid port" >&2
                 exit 1
             fi
             shift
             ;;
        *)
             echo "Unknown argument $1" >&2
             exit 1
             ;;
   esac
   shift
done

if [ -z "$user" ] || [ "$port" -eq 0 ]; then
    echo "No user or port specified" >&2
    exit 1
fi
```

### Moshell

Moshell functions can return values rather than an exitcode. This allows the `shift` operation to be redefined to return the shifted argument then shift arguments to the left.

As everything is text in a shell, values of a certain type can be _parsed_ as another type using methods on `String`.

The `$(expr)` syntax substitutes the stdout of the underlying expression and the `{expr}` will substitute the return value of the expression.  
Another point is that substitution is automatically protected, thus a `$x` and `$(...)` expression is equivalent to bash `"$1"` and `"$(...)"` syntax.

```scala
var user = ''
var port = 0
while [ $# ] match {shift} { // calls the shift function and substitutes its return value
     -u | --user    => user = shift()
     -port | --port => port = shift().parse_int().expect("Invalid port")
     $arg           => panic "Unknown argument $arg"
}

if $user.is_empty() || $port == 0 {
    panic('No user specified')
}
```


## Current state

Moshell is a project in its early stages.

- [x] Lexer + Parser
  - [x] Standard shell expressions
  - [x] Control flow
  - [x] Type hints
  - [x] Array indexing and ranges
  - [x] User defined structures
  - [ ] User defined enums
- [x] Static analysis
  - [x] Symbol resolution
  - [x] Imports resolution
  - [x] Qualified names
  - [x] Primitive type checking
  - [x] Built-in primitive type operations
  - [ ] Detailed error reports *(in progress)*
  - [x] Reefs (library support)
  - [x] Generic types
  - [ ] Standard types (`Option[T]`, `Result[A, E]`, `Iterable[T]`...)
  - [ ] User defined structures *(in progress)*
- [x] Bytecode compiler and interpreter
  - [x] Spawn processes and use typed variables
  - [x] Control flow
  - [x] Function calls
  - [x] Panic
  - [x] Dynamic memory handling (Garbage Collector)
  - [X] Vectors *(partial)*
  - [ ] Closures
- [x] REPL
  - [x] Visualize AST and IR
  - [x] Visualize Bytecode
  - [ ] Display diagnostics *(partial)*
  - [x] Symbol reuse
  - [ ] Shell-like prompt *(partial)*
