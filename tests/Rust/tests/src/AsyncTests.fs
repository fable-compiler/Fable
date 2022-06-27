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

[<Fable.Core.Rust.ReferenceType(Fable.Core.Rust.RefType.Arc)>]
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

// open System.Threading.Tasks
// [<Fact>]
// let shouldExecutedTask () =
//     let a = Task.FromResult 1
//     let b = Task.FromResult 2
//     let comp = task {
//         let! a = a
//         let! b = b
//         return a + b
//     }
//     comp.Result |> equal 3