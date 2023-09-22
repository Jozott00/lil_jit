# Lil JIT

An excuse to play with JIT compilers. 🐣

## Build and Run

```bash
cargo run
```

## Lil-lang

Since the main objective is to play with JIT compilers the language is quite simple.

There are variables, conditions, loop, functions and recursion. 
However, all values are 32 bit integer and all functions return an integer.

### Example
```rust
fn fib(n) {
    if n <= 0: return 0
    if n <= 2: return 0
    return fib(n - 1) + fib(n - 2)
}
    
fn main() {
    showtextln("The first 100 fibonacci numbers:")
    for let i = 0; i < 100; i++: showln(fib(i))
}
```

While there are no strings as values in the language there is are two exception which are the builtin functions `showtext` and `showtextln` which allow to pass them text at compile-time.

Like many C-like languages Lil-lang has blocks with curly braces but if only a single expression is needed they can be replaced by a double-colon. Therefore these two statements are equally:

```rust
if age < 18 {
    return 0
}

if age < 18: return 0
```

Functions can only be defined in the global scope.