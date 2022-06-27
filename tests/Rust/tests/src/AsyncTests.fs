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