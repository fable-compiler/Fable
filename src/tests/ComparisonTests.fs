[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Comparison
open System
open NUnit.Framework
open Fable.Tests.Util
open System.Collections.Generic

[<Test>]
let ``Array equality works``() =  
    let xs1 = [| 1; 2; 3 |]
    let xs2 = [| 1; 2; 3 |]
    let xs3 = [| 1; 2; 4 |]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)

[<Test>]
let ``Tuple equality works``() =  
    let xs1 = ( 1, 2, 3 )
    let xs2 = ( 1, 2, 3 )
    let xs3 = ( 1, 2, 4 )
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)

[<Test>]
let ``List equality works``() =  
    let xs1 = [ 1; 2; 3 ]
    let xs2 = [ 1; 2; 3 ]
    let xs3 = [ 1; 2; 4 ]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)

[<Test>]
let ``Set equality works``() =  
    let xs1 = Set [ 1; 2; 3 ]
    let xs2 = Set [ 1; 2; 3 ]
    let xs3 = Set [ 1; 2; 4 ]
    let xs4 = Set [ 3; 2; 1 ]
    let xs5 = Set [ 1; 2; 3; 1 ]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal true (xs1 = xs4)
    equal false (xs1 <> xs5)
    
[<Test>]
let ``Map equality works``() =  
    let xs1 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
    let xs2 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
    let xs3 = Map [ ("a", 1); ("b", 2); ("c", 4) ]
    let xs4 = Map [ ("c", 3); ("b", 2); ("a", 1) ]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal true (xs1 = xs4)
    
type UTest = A of int | B of int

[<Test>]
let ``Union equality works``() =  
    let u1 = A 2
    let u2 = A 2
    let u3 = A 4
    let u4 = B 2
    equal true (u1 = u2)
    equal false (u1 = u3)
    equal true (u1 <> u3)
    equal false (u1 <> u2)
    equal false (u1 = u4)
    Object.ReferenceEquals(u1, u1) |> equal true
    Object.ReferenceEquals(u1, u2) |> equal false

[<CustomEquality; CustomComparison>]
type UTest2 =
    | String of string
#if DOTNETCORE
    override x.GetHashCode() = x.GetHashCode()
    override x.Equals(yobj) =
       match yobj with
         | :? UTest2 as y ->
            match x, y with
            | String s1, String s2 -> (s1 + s1) = s2
         | _ -> false
#endif
    interface System.IEquatable<UTest2> with
        member x.Equals(y) =
            match x, y with
            | String s1, String s2 -> (s1 + s1) = s2
    interface System.IComparable with
        member x.CompareTo(yobj) =
            match yobj with
            | :? UTest2 as y ->
                match x, y with
                | String s1, String s2 -> compare (s1 + s1) s2
#if DOTNETCORE
            | _ -> invalidArg "yobj" "cannot compare values of different types"
#else
            | _ -> -1
#endif

[<Test>]
let ``Union custom equality works``() =  
    let u1 = String "A"
    let u2 = String "A"
    let u3 = String "AA"
    equal false (u1 = u2)
    equal true (u1 = u3)

type RTest = { a: int; b: int }

[<Test>]
let ``Record equality works``() =  
    let r1 = { a = 1; b = 2 }
    let r2 = { a = 1; b = 2 }
    let r3 = { a = 1; b = 4 }
    equal true (r1 = r2)
    equal false (r1 = r3)
    equal true (r1 <> r3)
    equal false (r1 <> r2)
    Object.ReferenceEquals(r1, r1) |> equal true
    Object.ReferenceEquals(r1, r2) |> equal false

[<ReferenceEquality>]
type RTest2 = { a2: int; b2: int }

[<Test>]
let ``Record reference equality works``() =  
    let r1 = { a2 = 1; b2 = 2 }
    let r2 = { a2 = 1; b2 = 2 }
    equal false (r1 = r2)

type Test(i: int) =
    member x.Value = i
    override x.GetHashCode() = i
    override x.Equals(yobj) =
       match yobj with
         | :? Test as y -> y.Value + 1 = x.Value
         | _ -> false    
    interface System.IComparable with
        member x.CompareTo(yobj) =
            match yobj with
            | :? Test as y -> compare (y.Value + 1) x.Value
#if DOTNETCORE
            | _ -> invalidArg "yobj" "cannot compare values of different types"
#else
            | _ -> -1
#endif

    interface System.IEquatable<Test> with
        member x.Equals(y) =
            y.Value + 1 = x.Value
            
[<Test>]
let ``Equality with objects implementing IEquatable works``() =  
    let c1 = Test(5)
    let c2 = Test(4)
    let c3 = Test(5)
    equal true (c1 = c2)
    equal false (c1 = c3)
    equal true (c1 <> c3)
    equal false (c1 <> c2)
    Object.ReferenceEquals(c1, c1) |> equal true
    Object.ReferenceEquals(c1, c2) |> equal false

[<Test>]
let ``Array comparison works``() =  
    let xs1 = [| 1; 2; 3 |]
    let xs2 = [| 1; 2; 3 |]
    let xs3 = [| 1; 2; 4 |]
    let xs4 = [| 1; 2; 2 |]
    let xs5 = [| 1; 2 |]
    let xs6 = [| 1; 2; 3; 1 |]
    equal 0 (compare xs1 xs2)
    equal -1 (compare xs1 xs3)
    equal true (xs1 < xs3)
    equal 1 (compare xs1 xs4)
    equal false (xs1 < xs4)
    equal 1 (compare xs1 xs5)
    equal true (xs1 > xs5)
    equal -1 (compare xs1 xs6)
    equal false (xs1 > xs6)

[<Test>]
let ``Tuple comparison works``() =  
    let xs1 = ( 1, 2, 3 )
    let xs2 = ( 1, 2, 3 )
    let xs3 = ( 1, 2, 4 )
    let xs4 = ( 1, 2, 2 )
    equal 0 (compare xs1 xs2)
    equal -1 (compare xs1 xs3)
    equal true (xs1 < xs3)
    equal 1 (compare xs1 xs4)
    equal false (xs1 < xs4)

[<Test>]
let ``List comparison works``() =  
    let xs1 = [ 1; 2; 3 ]
    let xs2 = [ 1; 2; 3 ]
    let xs3 = [ 1; 2; 4 ]
    let xs4 = [ 1; 2; 2 ]
    let xs5 = [ 1; 2 ]
    let xs6 = [ 1; 2; 3; 1 ]
    equal 0 (compare xs1 xs2)
    equal -1 (compare xs1 xs3)
    equal true (xs1 < xs3)
    equal 1 (compare xs1 xs4)
    equal false (xs1 < xs4)
    equal 1 (compare xs1 xs5)
    equal true (xs1 > xs5)
    equal -1 (compare xs1 xs6)
    equal false (xs1 > xs6)

[<Test>]
let ``Set comparison works``() =  
    let xs1 = Set [ 1; 2; 3 ]
    let xs2 = Set [ 1; 2; 3 ]
    let xs3 = Set [ 1; 2; 4 ]
    let xs4 = Set [ 1; 2; 2 ]
    let xs5 = Set [ 1; 2 ]
    let xs6 = Set [ 1; 2; 3; 1 ]
    equal 0 (compare xs1 xs2)
    equal -1 (compare xs1 xs3)
    equal true (xs1 < xs3)
    equal 1 (compare xs1 xs4)
    equal false (xs1 < xs4)
    equal 1 (compare xs1 xs5)
    equal true (xs1 > xs5)
    equal 0 (compare xs1 xs6)
    
[<Test>]
let ``Map comparison works``() =  
    let xs1 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
    let xs2 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
    let xs3 = Map [ ("a", 1); ("b", 2); ("c", 4) ]
    let xs4 = Map [ ("a", 1); ("b", 2); ("c", 2) ]
    let xs5 = Map [ ("a", 1); ("b", 2) ]
    let xs6 = Map [ ("a", 1); ("b", 2); ("c", 3); ("d", 1) ]
    equal 0 (compare xs1 xs2)
    equal -1 (compare xs1 xs3)
    equal true (xs1 < xs3)
    equal 1 (compare xs1 xs4)
    equal false (xs1 < xs4)
    equal 1 (compare xs1 xs5)
    equal true (xs1 > xs5)
    equal -1 (compare xs1 xs6)
    equal false (xs1 > xs6)

[<Test>]
let ``Union comparison works``() =  
    let u1 = A 2
    let u2 = A 2
    let u3 = A 4
    let u4 = A 1
    let u5 = B 2
    equal 0 (compare u1 u2)
    equal -1 (compare u1 u3)
    equal true (u1 < u3)
    equal 1 (compare u1 u4)
    equal false (u1 < u4)
    (compare u1 u5) = 0 |> equal false

[<Test>]
let ``Union custom comparison works``() =  
    let u1 = String "A"
    let u2 = String "A"
    let u3 = String "AA"
    equal 0 (compare u1 u3)
    equal true (compare u1 u2 > 0)
    
[<Test>]
let ``Record comparison works``() =  
    let r1 = { a = 1; b = 2 }
    let r2 = { a = 1; b = 2 }
    let r3 = { a = 1; b = 4 }
    equal 0 (compare r1 r2)
    (compare r1 r3) = 0 |> equal false
    
[<Test>]
let ``Comparison with objects implementing IComparable works``() =  
    let c1 = Test(5)
    let c2 = Test(4)
    let c3 = Test(5)
    equal 0 (compare c1 c2)
    equal 1 (compare c1 c3)
    equal true (c1 > c3)    

[<Test>]
let ``max works with primitives``() =  
    max 1 2 |> equal 2
    Math.Max(1, 2) |> equal 2
    max "a" "b" |> equal "b"
        
[<Test>]
let ``max works with records``() =
    let r1 = {a=1; b=1}
    let r2 = {a=1; b=2}
    max r1 r2 |> equal r2

[<Test>]
let ``max with objects implementing IComparable works``() =  
    let c1 = Test(5)
    let c2 = Test(5)
    max c1 c2 |> equal c1
    
[<Test>]
let ``min works with primitives``() =  
    min 1 2 |> equal 1
    Math.Min(1, 2) |> equal 1
    min "a" "b" |> equal "a"
        
[<Test>]
let ``min works with records``() =
    let r1 = {a=1; b=1}
    let r2 = {a=1; b=2}
    min r1 r2 |> equal r1
    
[<Test>]
let ``min with objects implementing IComparable works``() =  
    let c1 = Test(5)
    let c2 = Test(5)
    min c1 c2 |> equal c2

[<Test>]
let ``isNull works with primitives``() =  
    isNull null |> equal true
    isNull "" |> equal false
    isNull "0" |> equal false
    isNull "hello" |> equal false
      
[<Test>]
let ``isNull works with objects``() =
    let s1: String = null
    isNull s1 |> equal true

    let s2: String = "hello";
    isNull s2 |> equal false

    
