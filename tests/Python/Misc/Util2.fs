namespace Fable.Tests.Util2

open System

// Check files with no root module compile properly
type Helper =
    static member Format(pattern: string, [<ParamArray>] args: obj[]) =
        String.Format(pattern, args)

type Helper3(i: int) =
    member x.Value = string i

type H = Helper3

// Lowercase type name at the root of the file: the Python class declaration and
// its references from other files must agree on the name
type shape(size: int) =
    member _.Size = size

type Direction = | Up


module Extensions =
    type String with
        member inline x.StartsWithIgnoreCase(value) =
            x.StartsWith(value, StringComparison.OrdinalIgnoreCase)