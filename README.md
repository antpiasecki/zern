# zern

A very cool language

## Huh?
* Clean indentation-based syntax
* Compiles to x86_64 Assembly
* Almost works
* Has the pipe operator

## Syntax
```rust
func fib[n: I64] : I64
    if n <= 1
        return n
    return fib(n-2) + fib(n-1)

func main[] : I64
    for i in 0..20
        fib(i) |> I64.to_string() |> print()
```