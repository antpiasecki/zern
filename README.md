# zern

A very cool language

## Features
* Clean indentation-based syntax
* Compiles to x86_64 Assembly
* ~~No libc required~~ (SOON; still used for `malloc,realloc,free,snprintf,system,gethostbyname`)
* Produces tiny static executables (~50KB with musl)
* Sometimes works
* Has the pipe operator

## Syntax
```rust
func main[] : I64
    let answer: I64 = math.abs(os.urandom()) % 100

    while true
        io.println("Guess a number: ")
        let guess: I64 = io.read_stdin() |> str.trim() |> str.parse_i64()

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
cargo install --git https://github.com/antpiasecki/zern
zern -m -r hello.zr
```