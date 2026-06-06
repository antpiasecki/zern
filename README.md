# zern

A very cool language

## Features
* Clean indentation-based syntax
* Compiles to x86_64 Assembly
* Growing [standard library](https://git.ton1.dev/toni/zern/src/branch/main/src/std)
* Produces tiny static executables (11KB for `hello.zr`)
* No libc required!
* Has type inference, variadics, dynamic arrays, hashmaps, DNS resolver, etc.

## Syntax
```rust
func main[] : i64
    answer := math.abs(os.urandom_i64()) % 100

    while true
        io.println("Guess a number: ")
        guess := str.parse_i64(io.read_line())

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
