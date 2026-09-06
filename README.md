# zern

A very cool language

## Features

- Clean indentation-based syntax
- Targets x86-64 Linux and Windows
- No libc required!
- Produces tiny static executables (11KB for `hello.zr`)
- Has static typing, [UFCS](https://en.wikipedia.org/wiki/Uniform_function_call_syntax), custom allocator, macros, variadics, dynamic arrays, hashmaps, DNS resolver, etc.

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

```sh
git clone https://git.ton1.dev/toni/zern
cd zern
# make sure /usr/local/bin is in your PATH or set PREFIX
./install.sh
zern
```
