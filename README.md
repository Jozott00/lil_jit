
# LiL (Little Lang) JIT

An excuse to play with JIT compilers. 🐣

The JIT compiler targets ARM64 and compiles functions on the first invocation.

## Features

- A parser built with `nom` for a self-designed language.
- Some error checking like missing the main function on the AST or variable scoping (but no type-checking because the language doesn't require any).
- A custom architecture independent Assembly like IR (Intermediate representation).
- Constant folding on IR
- Constant propagation optimization on IR
- Fast register allocation with LSRA.
    - Callee-saved registers for variables with lifespan across function calls
    - Caller-saved registers for variables that do not live across function calls
    - Uses heuristic-based spilling if no more registers are available
- Code compilation to binary encoded 64-bit Arm.
    - Uses our self-written assembler library [ArmouredRust](https://github.com/Jozott00/ARMoured_rust)
  - Using Arm64 standard ABI
  - Using stub for calling non-compiled functions

## Build and Run

### On ARM64 with rust toolchain installed
```bash
cargo run examples/mandelbrot.lil
```

### Using Docker
We provide a Docker image, representing the lil_jit compiler executable. This allows
users, working on non-ARM64 computers, executing LiL programs on the `lil_jit`.

You may either download the prebuilt docker image or build it on your own using the Dockerfile.

**Notes for non ARM64 architectures:**
- Docker requires an emulator such as QEMU to build and run the image.
- The performance is significantly worse than on native ARM64


#### Short Version (Prebuild)
The `lil_jit.sh` can be used as it was the actual `lil_jit` executable.
```
$ sh lil_jit.sh example/mandelbrot.lil
```
By running the bash script, you download the docker image from the docker hub repository and then run it.
Additionally all necessary volume mounts are done for you and arguments are propagated to the docker container. After the execution, the created container gets automatically removed.

Showcase of running `lil_jit` on an x86_64 computer:

https://github.com/Jozott00/lil_jit/assets/12057307/5f249c6a-d200-4ebf-9036-ba6dc3a6643b


#### Manually
*Note: Building the docker image includes compiling the lil_jit compiler, which takes several minutes 
on non-ARM64 architectures.* 
```
$ docker build -t lil_jit .
$ docker run --rm --plateform linux/arm64 -v <lil_file_path>:/app/<lil_file_path> lil_jit <lil_file_path>
```

## The Little Lang

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

While there are no strings as values in the language there are two exceptions, which are the builtin
functions `showtext` and `showtextln` which allow to pass them text at compile-time.

Like many C-like languages LiL-lang has blocks with curly braces but if only a single expression is needed they can be
replaced by a double-colon. Therefore these two statements are equally:

```
if age < 18 {
    return 0
}

if age < 18: return 0
```

Functions can only be defined in the global scope.
