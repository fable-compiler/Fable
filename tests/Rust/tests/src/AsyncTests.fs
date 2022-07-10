module Fable.Tests.AsyncTests

open Util.Testing

[<Fact>]
let shouldExecPrim () =
    let z = async { return 3 }
    let comp = async {
        let x = 1
        let y = 2
        let! z = z
        return x + y + z
    }
    let t = Async.StartAsTask comp
    t.Result |> equal 6

[<Fact>]
let shouldExecSynchronously () =
    let y = async { return 3 }
    let comp = async {
        let x = 1
        let! y = y
        return x + y
    }
    let t = Async.RunSynchronously comp
    t |> equal 4

[<Fact>]
let shouldConvertTaskToASyncAndEvalCorrectly () =
    let t = task { return 1 } |> Async.AwaitTask
    t |> Async.RunSynchronously |> equal 1

// [<Fact>]
// let shouldExecAsParallelStructurallyCorrect () =
//     let t = Async.Parallel [
//         async { return 1 }
//         async { return 2 }
//     ]
//     t |> Async.RunSynchronously |> Array.sum |> equal 3

// [<Fact>]
// let shouldMarshalMutOverAsyncClosureCorrectly () =
//     let mutable x = 1
//     let comp = async {
//         let! z = async { return 3 }
//         x <- x + 1
//         return x + z
//     }
//     let t = Async.StartAsTask comp
//     t.Result |> equal 5

[<Fable.Core.Rust.ReferenceType(Fable.Core.Rust.PointerType.Arc)>]
type ArcRecord = {
    A: int
}

[<Fact>]
let shouldCorrectlyScopeArcRecord () =
    let a = { A = 3 }
    let ab = async { return a }
    let comp = async {
        let x = 2
        let! a = ab
        let y = a.A
        return x + y
    }

    let t = Async.StartAsTask comp
    t.Result |> equal 5

open System.Threading.Tasks
[<Fact>]
let shouldExecuteTask () =
    let a = Task.FromResult 1
    let b = Task.FromResult 2
    let comp = task {
        let! a = a
        let! b = b
        return a + b
    }
    comp.Result |> equal 3