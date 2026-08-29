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
    // Overloaded constructors are emitted as module-level names with an overload
    // suffix (e.g. `shape__ctor_Z<hash>`) and imported across files. The declaration
    // and the import must agree on the suffix casing too, not just the base name.
    new(label: string) = shape(label.Length)
    member _.Size = size

type Direction = | Up


module Extensions =
    type String with
        member inline x.StartsWithIgnoreCase(value) =
            x.StartsWith(value, StringComparison.OrdinalIgnoreCase)