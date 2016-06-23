[<NUnit.Framework.TestFixture>] 
module Fable.Tests.ResizeArrays
open NUnit.Framework
open Fable.Tests.Util

[<Test>]
let ``ResizeArray zero creation works``() =
    let li = ResizeArray<float>()
    equal 0 li.Count

[<Test>]
let ``ResizeArray zero creation with size works``() =
    let li = ResizeArray<string>(5)
    equal 0 li.Count

[<Test>]
let ``ResizeArray creation with seq works``() =
    let li = ResizeArray<_>(seq{1..5})
    Seq.sum li
    |> equal 15

[<Test>]
let ``ResizeArray casting to seq works``() =
    let xs = ResizeArray<_>(seq{1..5}) :> seq<_>
    Seq.sum xs
    |> equal 15

[<Test>]
let ``ResizeArray iteration works``() =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
    let acc = ref 0.
    for i in li do
       acc := !acc + i
    equal 15. !acc

[<Test>]
let ``ResizeArray iteration with index works``() =
    let li = ResizeArray<_>()
    for i = 1 to 4 do
       li.Add(i)
    let mutable x = 0
    for i = 0 to li.Count - 1 do
       x <- x + li.[i]
    equal 10 x

[<Test>]
let ``ResizeArray folding works``() =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
    li |> Seq.fold (fun acc item -> acc + item) 0.
    |> equal 15.

[<Test>]
let ``ResizeArray.Count works``() =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
    equal 5 li.Count

[<Test>]
let ``ResizeArray indexer getter works``() =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
    equal 2. li.[1]

[<Test>]
let ``ResizeArray indexer setter works``() =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
    li.[3] <- 10.
    equal 10. li.[3]

[<Test>]
let ``ResizeArray.Clear works``() =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
    li.Clear()
    equal 0 li.Count

[<Test>]
let ``ResizeArray.Add works``() =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
    li.Add(6.)
    equal 6 li.Count

[<Test>]
let ``ResizeArray.AddRange works``() =
    let li = ResizeArray<_>()
    li.AddRange [1;2;3]
    equal 3 li.Count

[<Test>]
let ``ResizeArray.Contains works``() =
    let li = ResizeArray<_>()
    li.Add("ab")
    li.Add("ch")
    li.Contains("ab") |> equal true
    li.Contains("cd") |> equal false

[<Test>]
let ``ResizeArray.IndexOf works``() =
    let li = ResizeArray<_>()
    li.Add("ch")
    li.Add("ab")
    li.IndexOf("ab") |> equal 1
    li.IndexOf("cd") |> equal -1

[<Test>]
let ``ResizeArray.Remove works``() =
    let li = ResizeArray<_>()
    li.Add("ab")
    li.Add("ch")
    li.Remove("ab") |> equal true
    li.Remove("cd") |> equal false

[<Test>]
let ``ResizeArray.RemoveAt works``() =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
    li.RemoveAt(2)
    equal 4. li.[2]

[<Test>]
let ``ResizeArray.Insert works``() =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li.Insert(2, 8.)
    equal 8. li.[2]

[<Test>]
let ``ResizeArray.ReverseInPlace works``() =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.) 
    li.Reverse()
    equal 2. li.[3]

[<Test>]
let ``ResizeArray.SortInPlace works``() =
    let li = ResizeArray<_>()
    li.Add("Ana"); li.Add("Pedro"); li.Add("Lucía"); li.Add("Paco")
    li.Sort()
    equal "Paco" li.[2]
    let li2 = ResizeArray [1;3;10;2]
    li2.Sort()
    equal 2 li2.[1]

[<Test>]
let ``ResizeArray.SortInPlaceWith works``() =
    let li = ResizeArray<_>()
    li.Add(3.); li.Add(6.); li.Add(5.); li.Add(4.); li.Add(8.)
    li.Sort(fun x y -> if x > y then -1 elif x < y then 1 else 0)
    equal 4. li.[3]

[<Test>]
let ``ResizeArray.ToArray works``() =
    let li = ResizeArray<_>()
    li.Add(3.); li.Add(6.); li.Add(5.); li.Add(4.); li.Add(8.)
    equal 5 li.Count
    let ar = li.ToArray()
    Array.length ar |> equal li.Count
    ar.[0] <- 2.
    equal 3. li.[0]
    equal 2. ar.[0]
