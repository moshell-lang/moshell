# Moshell

Moshell is a modern shell scripting language with a static type system.

## Motivation

The shell is a powerful tool, but it's also a very old one.
While having a permissive language can be a good thing, it makes it harder to statically analyze a shell script for errors.

Due to the highly interpreted nature of the Shell, escaping and quoting arguments can be a pain to get right.
Moshell resolves arguments before evaluating them, so references to variables do not need to be escaped.

Arithmetic evaluation traditionally needs to be made explicit to be differentiated from a command, where Moshell allows direct arithmetic evaluation if not at the beginning at a statement.

Moshell also comes with different data types, such as `Int`, `Float`, `Bool` or `String`. This allows for clearer and more elegant code and avoids arithmetic between incompatible types.

## Current state

Moshell is a project in its early stages.

- [x] [Lexer + Parser](https://github.com/orgs/moshell-lang/projects/1)
    - [x] Standard shell expressions
    - [x] Control flow
    - [x] Type hints
    - [ ] Array indexing and ranges *(in progress)*
    - [ ] User defined types
- [x] [Static analysis](https://github.com/orgs/moshell-lang/projects/2)
    - [x] Symbol resolution
    - [x] Imports resolution
    - [x] Qualified names
    - [x] Primitive type checking
    - [ ] Built-in primitive type operations *(in progress)*
    - [ ] Detailed error reports *(in progress)*
    - [ ] List types
    - [ ] Standard types (`Option[T]`, `Result[A, E]`, `Iterable[T]`...)
    - [ ] User defined types
- [x] Bytecode compiler and interpreter
    - [x] Spawn processes and use typed variables
    - [ ] Control flow
    - [ ] Function calls
    - [ ] Bytecode loading
    - [ ] Closures
- [x] REPL
    - [x] Visualize AST and IR
    - [ ] Display diagnostics *(partial)*
    - [ ] Symbol reuse
    - [ ] Shell-like prompt

This repository hosts the source code from the parser to the interpreter.

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
while [ $# ] match shift() { // calls the shift function and substitutes its return value
     -u | --user    => user = {shift}
     -port | --port => port = {shift}.parse_int().expect("Invalid port")
     $arg           => panic "Unknown argument $arg"
}

if $user.is_empty() || $port == 0 {
    panic 'No user specified'
}
```
