module Fable.Tests.ParallelTest

open Fable.Tests.Util
open Util.Testing

// --- Array.Parallel ---

[<Fact>]
let ``test Array.Parallel.map works`` () =
    let arr = [| 1; 2; 3; 4; 5 |]
    let result = Array.Parallel.map (fun x -> x * 2) arr
    result |> equal [| 2; 4; 6; 8; 10 |]

[<Fact>]
let ``test Array.Parallel.mapi works`` () =
    let arr = [| 10; 20; 30 |]
    let result = Array.Parallel.mapi (fun i x -> i + x) arr
    result |> equal [| 10; 21; 32 |]

[<Fact>]
let ``test Array.Parallel.init works`` () =
    let result = Array.Parallel.init 5 (fun i -> i * i)
    result |> equal [| 0; 1; 4; 9; 16 |]

[<Fact>]
let ``test Array.Parallel.iter completes`` () =
    // Note: iter spawns processes, so we can't test mutable side effects
    // from the parent process. Just verify it completes without error.
    let arr = [| 1; 2; 3; 4; 5 |]
    Array.Parallel.iter (fun _x -> ()) arr

[<Fact>]
let ``test Array.Parallel.iteri completes`` () =
    let arr = [| 10; 20; 30 |]
    Array.Parallel.iteri (fun _i _x -> ()) arr

[<Fact>]
let ``test Array.Parallel.collect works`` () =
    let arr = [| 1; 2; 3 |]
    let result = Array.Parallel.collect (fun x -> [| x; x * 10 |]) arr
    result |> equal [| 1; 10; 2; 20; 3; 30 |]

[<Fact>]
let ``test Array.Parallel.choose works`` () =
    let arr = [| 1; 2; 3; 4; 5; 6 |]
    let result = Array.Parallel.choose (fun x -> if x % 2 = 0 then Some (x * 10) else None) arr
    result |> equal [| 20; 40; 60 |]

[<Fact>]
let ``test Array.Parallel.map preserves order`` () =
    let arr = [| 1 .. 100 |]
    let result = Array.Parallel.map (fun x -> x) arr
    result |> equal arr

[<Fact>]
let ``test Array.Parallel.map with string transformation`` () =
    let arr = [| "hello"; "world"; "test" |]
    let result = Array.Parallel.map (fun (s: string) -> s.ToUpper()) arr
    result |> equal [| "HELLO"; "WORLD"; "TEST" |]

[<Fact>]
let ``test Array.Parallel.init with zero length`` () =
    let result = Array.Parallel.init 0 (fun i -> i)
    result |> equal [||]
