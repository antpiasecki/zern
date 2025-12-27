# zern

A very cool language

## Features
* Clean indentation-based syntax
* Compiles to x86_64 Assembly
* ~~No libc required~~ (SOON; still used for memory allocation and DNS resolution)
* Produces tiny static executables (~30KB with musl)
* Sometimes works
* Has the pipe operator

## Syntax
```rust
func main[] : i64
    let answer: i64 = math.abs(os.urandom()) % 100

    while true
        io.println("Guess a number: ")
        let guess: i64 = io.read_line() |> str.trim() |> str.parse_i64()

        if guess == answer
            io.println("You win!")
            break
        else if guess < answer
            io.println("Too low!")
        else
            io.println("Too high!")
```

```rust
func square[x: i64] : i64
    return x * x

func sum[a: i64, b: i64] : i64
    return a + b

func main[] : i64
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
    |> alg.map(^square)
    |> alg.reduce(^sum, 0)
    |> io.println_i64()
```

## Quickstart
```
cargo install --git https://github.com/antpiasecki/zern
zern -m -r hello.zr
```