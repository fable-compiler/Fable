namespace Fable.Tests.Util2

open System

// // Check files with no root module compile properly
// type Helper =
//     static member Format(pattern: string, [<ParamArray>] args: obj[]) =
//         String.Format(pattern, args)
type Helper = String

// type Helper2 =
//     // Check that project references from folders work
//     static member CreateArray() =
//         Fable.Tests.Spaces.Helper.CreateArray()

type Helper3(i: int) =
    member x.Value = string i

type H = Helper3


module Extensions =
    type String with
        member inline x.StartsWithIgnoreCase(value) =
            x.StartsWith(value, StringComparison.OrdinalIgnoreCase)