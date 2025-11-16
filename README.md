# zern

A very cool language

## Features
* Clean indentation-based syntax
* Compiles to x86_64 Assembly
* Sometimes works
* Has the pipe operator

## Syntax
```rust
func main[] : I64
    let answer: I64 = math.abs(math.urandom()) % 100

    while true
        io.print("Guess a number: ")
        let guess: I64 = io.read_stdin() |> str.trim() |> str.parse_i64()

        if guess == answer
            io.print("You win!")
            break
        else if guess < answer
            io.print("Too low!")
        else
            io.print("Too high!")
```

## Quickstart
```
cargo install --git https://github.com/antpiasecki/zern
zern -m -r hello.zr
```