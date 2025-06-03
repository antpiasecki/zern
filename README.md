# zern

A very cool language

## Huh?
* Indentation-based syntax
* Compiled to x86_64 Assembly
* Sometimes works

## Syntax
```swift
func fib[n: I64] : I64
    if n <= 1
        return n
    return fib(n-2) + fib(n-1)

func main[] : I64
    for i in 0:20
        fib(i) |> I64.to_string() |> print()
```