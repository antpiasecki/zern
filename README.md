# mot

A very cool language

## Syntax
```go
func fib[U32 n] : U32
    if n <= 1
        return n
    return fib(n-2) + fib(n-1)

func main[]
    for i in 0..20
        fib(i) |> U32.to_string() |> IO.print()
```