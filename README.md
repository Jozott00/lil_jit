# Lil JIT

An excuse to play with JIT compilers. 🐣

## Features

- A parser build with `nom` for a self designed language.
- Some error checking like missing main function on the AST (but no typechecking because the language doesn't require any).
- A custom architecture independent Assembly like IR (Intermediate representation).
- Fast register allocation with LSRA.
- Code compilation to binary encoded 64-Bit Arm.

## Build and Run

```bash
cargo run examples/fib.txt
```

## Lil-lang

Since the main objective is to play with JIT compilers the language is quite simple.

There are variables, conditions, loop, functions and recursion.
However, all values are 32 bit integer and all functions return an integer.

### Example

```
fn fib(n) {
    if n <= 0: return 0
    if n <= 2: return 0
    return fib(n - 1) + fib(n - 2)
}
    
fn main() {
    showtextln("The first 100 fibonacci numbers:")
    for let i = 0; i < 100; i=i+1: showln(fib(i))
}
```

While there are no strings as values in the language there is are two exception which are the builtin
functions `showtext` and `showtextln` which allow to pass them text at compile-time.

Like many C-like languages Lil-lang has blocks with curly braces but if only a single expression is needed they can be
replaced by a double-colon. Therefore these two statements are equally:

```
if age < 18 {
    return 0
}

if age < 18: return 0
```

Functions can only be defined in the global scope.