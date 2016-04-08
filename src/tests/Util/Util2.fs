namespace Fable.Tests.Util2

open System

// Check files with no root module compile properly
type Helper =
    static member Format(pattern: string, [<ParamArray>] args: obj[]) =
        String.Format(pattern, args)
    
type Helper2 =
    // Check that project references from folders work
    static member CreateClampedArray() =
        Fable.Tests.Clamp.Helper.CreateClampedArray()