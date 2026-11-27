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
    let curried3 (a: int) (b: string) (c: bool) = string a + b + (if c then "t" else "f")
    let takesFnSingle (f: int -> int -> int) = f 3 4
    let takesFn3 (f: int -> int -> int -> int) = f 1 2 3
    let takesFnOption (f: (int -> int -> int) option) =
        match f with
        | Some f -> f 1 2
        | None -> -1
    let genericPair (a: 'T) (b: 'U) = (a, b)
    let genericToString (a: 'T) (b: 'T) = string a + string b
    let weird (``m$``: int) (n: int) = ``m$`` * n
    let returnsFn (a: int) : int -> int = fun b -> a + b
    let fnValue: int -> int -> int =
        let add a b = a + b
        add

    type DynamicRecord = { A: int; B: int }
    type DynamicMutable = { mutable X: int }
    let curriedRec (r: DynamicRecord) (s: string) = string r.A + string r.B + s
    let curriedArr (a: int[]) (s: string) = string a.Length + s

    let mutable partialModuleState = 1
    let mutable fullModuleState = 1
    let mutable perCallModuleState = 1

    type DynamicStatic =
        static member Params([<ParamArray>] xs: int[]) = xs.Length
        static member Gen<'T>(a: 'T, b: 'T) = [ a; b ]

    type DynamicInstance(x: int) =
        member _.Inst(a: int) = x + a
        member _.Inst2(a: int, b: int) = x + a + b

    let dynamicInstance = DynamicInstance(1)
