# Moshell
Moshell is a modern shell scripting language with a static typesystem.

This repository hosts the source code of the interpreter and standard library.

The moshell project is currently under development and thus have simple / partial features.

## examples
Here's some bash vs moshell comparisons

### Iterating over arguments 
bash 
```bash
while [ "$1" ]; do
   case "$1" in
      -a|--all) 
          ALL=true
          ;;
      -n)
          if [ ! grep -E "^[0-9]+$" <<< "$1" ]; then 
              echo "argument after -n is not an int"
              exit 1
          fi
          AMMOUNT="$2"
          ;;
       *)
          echo "unknown argument $1" && exit 1
   esac
   shift
done
```

Moshell 

Moshell is a statically typed language with functions that can return values rather than an exitcode. this allows the `shift` operation to be redefined to return the shifted argument then shift arguments to the left.

As everything is text in a shell, values of a certain type can be _parsed_ as another type using the `parse[T]` function.

Moshell brings a new kind of substitution, which is value substitution.  
like the `$(expr)` syntax which substitutes the stdout of the underlying expression, the `@(expr)` will substitute the return value of the expression.  
Another point is that substitution is automatically protected, thus a `$x` and `$(...)` expression is equivalent to bash `"$1"` and `"$(...)"` syntax.

```scala
var all: bool = false
var amount = 1 // infered type: int
// no need to manually protect $1
while [ $1 ] match @(shift) { // calls the shift function and substitutes is return value
     -a | --all => all = true
     -n         => amount = parse[int] @(shift) || crash "argument after -n is not an int" 
     $n         => crash "unknown argument $n"
} 
```

