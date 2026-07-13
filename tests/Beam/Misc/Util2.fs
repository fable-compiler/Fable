namespace Fable.Tests.Util2

open System

// Check files with no root module compile properly
type Helper =
    static member Format(pattern: string, [<ParamArray>] args: obj[]) =
        String.Format(pattern, args)

type Helper3(i: int) =
    member x.Value = string i

type H = Helper3

// Reflection fixtures declared away from ReflectionTests.fs, so that reflecting over them
// exercises the cross-module path: the type info is a remote call to the reflection function
// generated in this file's module, rather than a local call.
type CrossFileRecord = { Name: string; Count: int }

type CrossFileTree =
    | CrossFileLeaf of int
    | CrossFileBranch of CrossFileTree * CrossFileTree

type CrossFileGeneric<'T> = { Item: 'T; Rest: CrossFileGeneric<'T> option }


module Extensions =
    type String with
        member inline x.StartsWithIgnoreCase(value) =
            x.StartsWith(value, StringComparison.OrdinalIgnoreCase)
