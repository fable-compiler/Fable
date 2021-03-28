module Fable.Tests.Seqs

open Util.Testing
open Fable.Tests.Util

#if FABLE_COMPILER
#else
open Xunit
#endif


[<Fact>]
let ``test Seq.length works`` () =
    let xs = [1.; 2.; 3.; 4.]
    Seq.length xs
    |> equal 4
