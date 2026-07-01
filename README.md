# zern

A very cool language

## Features
* Clean indentation-based syntax
* Compiles to x86-64 Assembly
* No libc required!
* Produces tiny static executables (11KB for `hello.zr`)
* Has static typing, [UFCS](https://en.wikipedia.org/wiki/Uniform_function_call_syntax), variadics, dynamic arrays, hashmaps, DNS resolver, etc.

## Syntax
```rust
func main[] : i64
    answer := os.urandom_i64()->abs() % 100

    while true
        io.println("Guess a number: ")
        guess := io.read_line()->parse_i64()

        if guess == answer
            io.println("You win!")
            break
        else if guess < answer
            io.println("Too low!")
        else
            io.println("Too high!")
```

## Quickstart
```
cargo install --git https://git.ton1.dev/toni/zern
zern -r hello.zr
```
