# Moshell

Moshell is a modern shell scripting language with a static typesystem.

## Motivation

The shell is a powerful tool, but it's also a very old one.
While having a permissive language can be a good thing, it makes it harder to statically analyze a shell script for errors.

Due to the highly interpreted nature of the Shell, escaping and quoting arguments can be a pain to get right.
Moshell resolves arguments before evaluating them, so references to variables do not need to be escaped.

Arithmetic evaluation traditionally needs to be made explicit to be differentiated from a command, where Moshell allows direct arithmetic evaluation if not at the beginning at a statement.

Moshell also comes with different data types, such as `int`, `float`, `bool` or `any`. This allows for clearer and more elegant code and avoids arithmetic between incompatible types.

## Current state

Moshell is a project in its early stages.

- [x] Lexer
- [ ] [Parser](https://github.com/orgs/moshell-lang/projects/1) *(in progress)*
- [ ] IR / Type checking
- [ ] Interpreter
- [ ] REPL

This repository hosts the source code of the interpreter and standard library.

## Examples

Here's some Bash vs Moshell comparisons.

Note that Moshell examples cannot currently be run as is.

### Iterating over arguments 

Bash

```bash
ALL=false
AMOUNT=1
while [ $# -ne 0 ]; do
   case "$1" in
      -a|--all) 
          ALL=true
          ;;
      -n)
          if ! grep -E "^[0-9]+$" <<< "$1"; then 
              echo "argument after -n is not an int"
              exit 1
          fi
          AMOUNT="$2"
          shift
          ;;
       *)
          echo "unknown argument $1" && exit 1
   esac
   shift
done
```

Moshell 

Moshell functions can return values rather than an exitcode. This allows the `shift` operation to be redefined to return the shifted argument then shift arguments to the left.

As everything is text in a shell, values of a certain type can be _parsed_ as another type using the `parse[T]` function.

The `$(expr)` syntax substitutes the stdout of the underlying expression and the `@(expr)` will substitute the return value of the expression.  
Another point is that substitution is automatically protected, thus a `$x` and `$(...)` expression is equivalent to bash `"$1"` and `"$(...)"` syntax.

```scala
var all: bool = false
var amount = 1 // infered type: int
// no need to manually protect $1
while [ $1 ] match {shift} // calls the shift function and substitutes its return value
     -a | --all => all = true
     -n         => amount = parse[int] @(shift) || crash "argument after -n is not an int" 
     $n         => crash "unknown argument $n"
} 
```
