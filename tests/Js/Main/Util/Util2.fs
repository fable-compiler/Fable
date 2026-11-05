namespace Fable.Tests.Util2

open System

// Check files with no root module compile properly
type Helper =
    static member Format(pattern: string, [<ParamArray>] args: obj[]) =
        String.Format(pattern, args)

type Helper2 =
    // Check that project references from folders work
    static member CreateArray() =
        Fable.Tests.Spaces.Helper.CreateArray()

type Helper3(i: int) =
    member x.Value = string i

type H = Helper3


module Extensions =
    type String with
        member inline x.StartsWithIgnoreCase(value) =
            x.StartsWith(value, StringComparison.OrdinalIgnoreCase)

module DynamicImports =
    let single (a: int) = a + 1
    let tupled (a: int, b: string) = string a + b
    let curried (a: int) (b: string) = string a + b
    let tupledThenCurried (a: int, b: string) (c: int) = string a + b + string c
    let namedLikeModule (m: int) (n: int) = m * n
    let takesFunction (f: int -> int -> int) (x: int) = f x x
    let takesUnit () = "unit"
    let value = 42
