# TAILCALLS

* Assign parameters to temp variables when they are consumed after being "passed"
* Decision targets
* Nested functions?
* Mutually recursive functions?
* Tests: See [fable-tailcalls](https://github.com/tpetricek/fable-tailcalls/blob/master/test.fsx)

```fsharp
let rec factorial aux n =
  if n = 0 then aux
  else factorial (aux * n) (n - 1)
```