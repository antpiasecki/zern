# zern

A very cool language

## Features

- Clean indentation-based syntax
- Compiles to x86-64 Assembly
- No libc required!
- Produces tiny static executables (11KB for `hello.zr`)
- Has static typing, [UFCS](https://en.wikipedia.org/wiki/Uniform_function_call_syntax), generics, variadics, dynamic arrays, hashmaps, DNS resolver, etc.

## Syntax

```rust
include "$/io.zr"

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

Download the `zern-v<version>-linux-x86_64.tar.gz` archive from the project
[releases](https://git.ton1.dev/toni/zern/releases) and unpack it:

```sh
sudo tar -xzf zern-v*-linux-x86_64.tar.gz -C /usr/local
```

Make sure `/usr/local/bin` is in your `PATH`.

```sh
zern -h
```

## Building from source

```sh
git clone https://git.ton1.dev/toni/zern
cd zern
cargo build --release
./target/release/zern -h
```
