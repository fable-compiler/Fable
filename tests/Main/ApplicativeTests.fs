[<Util.Testing.TestFixture>]
module Fable.Tests.Applicative
open System
open System.Collections.Generic
open Util.Testing
open Fable.Tests.Util

let zipUnsorted (arr1:_[]) (arr2:_[]) =
  let d1 = dict arr1
  let d2 = dict arr2
  let res = ResizeArray<_>()
  for kv1 in d1 do
    let v2 =
      if d2.ContainsKey(kv1.Key) then Some(d2.[kv1.Key])
      else None
    res.Add(kv1.Key, (Some kv1.Value, v2))
  for kv2 in d2 do
    if not (d1.ContainsKey(kv2.Key)) then
      res.Add(kv2.Key, (None, Some kv2.Value))
  Array.ofSeq res

let isSortedUsing test proj (arr:_[]) =
  let rec loop i =
    if i = arr.Length then true
    else test (proj arr.[i-1]) (proj arr.[i]) && loop (i+1)
  arr.Length = 0 || loop 1

let zipSorted (arr1:('k*'v1)[]) (arr2:('k*'v2)[]) =
  let mutable i1 = 0
  let mutable i2 = 0
  let inline (<.) (a:'k) (b:'k) = compare a b < 0
  let inline eq (a:'k) (b:'k) = compare a b = 0
  let res = ResizeArray<_>()
  while i1 < arr1.Length && i2 < arr2.Length do
    let (k1, v1), (k2, v2) = arr1.[i1], arr2.[i2]
    if eq k1 k2 then
      res.Add(k1, (Some v1, Some v2))
      i1 <- i1 + 1
      i2 <- i2 + 1
    elif k1 <. k2 then
      res.Add(k1, (Some v1, None))
      i1 <- i1 + 1
    elif k2 <. k1 then
      res.Add(k2, (None, Some v2))
      i2 <- i2 + 1
  while i1 < arr1.Length do
    let k1, v1 = arr1.[i1]
    res.Add(k1, (Some v1, None))
    i1 <- i1 + 1
  while i2 < arr2.Length do
    let k2, v2 = arr2.[i2]
    res.Add(k2, (None, Some v2))
    i2 <- i2 + 2
  Array.ofSeq res

let zipAny (arr1:('k*'v1)[]) (arr2:('k*'v2)[]) =
  let inline (<=.) (a:'k) (b:'k) = compare a b <= 0
  let inline (>=.) (a:'k) (b:'k) = compare a b >= 0
  if isSortedUsing (<=.) fst arr1 && isSortedUsing (<=.) fst arr2 then zipSorted arr1 arr2
  elif isSortedUsing (>=.) fst arr1 && isSortedUsing (>=.) fst arr2 then Array.rev (zipSorted (Array.rev arr1) (Array.rev arr2))
  else zipUnsorted arr1 arr2

type Result<'s, 'f> =
    | Ok of 's
    | Error of 'f

    static member (>>=) (r: Result<'t, 'e>, f: 't -> Result<'u, 'e>) : Result<'u, 'e> =
        match r with
        | Error e -> Error e
        | Ok v -> f v

    static member (<^>) (f: 't -> 'u, r: Result<'t, 'e>) : Result<'u, 'e> =
        r >>= (f >> Ok)

    static member (<*>) (f: 't -> 'u, r: Result<'t, 'e>) : Result<'u, 'e> =
        failwith "This shouldn't be called"

    static member (<*>) (f: Result<('t -> 'u), 'e>, r: Result<'t, 'e>) : Result<'u, 'e> =
        f >>= fun f -> f <^> r

[<Test>]
let ``Infix applicative can be generated``() =
    let r = Ok 1
    let a = Ok string
    let r' = match a <*> r with
             | Ok x -> x
             | _ -> failwith "expected Ok"
    equal "1" r'

let inline apply (a:'a) (b:'b) =
    a <*> b

[<Test>]
let ``Infix applicative with inline functions can be generated``() =
    let r = Ok 1
    let a = Ok string
    let r' = match apply a r with
             | Ok x -> x
             | _ -> failwith "expected Ok"
    equal "1" r'

type Foo1(i) =
    member x.Foo() = i
    member x.Foo(j) = i + j

type Foo2(i) =
    member x.Foo(j) = (i + j) * 2

let inline foo< ^t when ^t : (member Foo : int -> int)> x i =
    (^t : (member Foo : int -> int) (x, i))

[<Test>]
let ``Local inline typed lambdas work``() =
    let inline localFoo (x:^t) = foo x 5
    let x1 = Foo1(2)
    let x2 = Foo2(2)
    equal 7 <| localFoo x1
    equal 14 <| localFoo x2

[<Test>]
let ``Local inline values work``() =
    let res = zipAny [|("a",1);("b",2)|] [|("c",5.);("a",4.)|]
    res.Length |> equal 3
    res.[0] |> fst |> equal "a"
    res.[0] |> snd |> equal (Some 1, Some 4.)
    res.[1] |> fst |> equal "b"
    res.[1] |> snd |> equal (Some 2, None)
    res.[2] |> fst |> equal "c"
    res.[2] |> snd |> equal (None, Some 5.)

open Aether
open Aether.Operators

let Lens_get (g, _) = fun o -> g o
let Lens_set (_, s) = fun i o -> s i o
let Lens_map (g, s) = fun f o -> s (f (g o)) o

let chars : Isomorphism<string, char[]> =
    (fun x -> x.ToCharArray ()), (fun x -> String (x))

let rev : Isomorphism<char[], char[]> =
    Array.rev, Array.rev

let inline (=!) x y = equal y x

[<Test>]
let ``Lens.get returns correct values`` () =
    Lens_get fst_ ("Good","Bad") =! "Good"

[<Test>]
let ``Lens.set sets value correctly`` () =
    Lens_set fst_ "Good" ("Bad",()) =! ("Good",())

[<Test>]
let ``Lens.map modifies values correctly`` () =
    Lens_map fst_ (fun x -> x + x) ("Good",()) =! ("GoodGood",())

[<Test>]
let ``Ismorphism composition over a lens gets value`` () =
    Lens_get (fst_ >-> chars) ("Good",()) =! [| 'G'; 'o'; 'o'; 'd' |]

[<Test>]
let ``Ismorphism composition over a lens sets value`` () =
    Lens_set (fst_ >-> chars) [| 'G'; 'o'; 'o'; 'd' |] ("Bad",()) =! ("Good",())

[<Test>]
let ``Ismorphism composition over a lens gets value over multiple isomorphisms`` () =
    Lens_get (fst_ >-> chars >-> rev) ("dooG",()) =! [| 'G'; 'o'; 'o'; 'd' |]

[<Test>]
let ``Ismorphism composition over a lens sets value over multiple isomorphisms`` () =
    Lens_set (fst_ >-> chars >-> rev) [| 'd'; 'o'; 'o'; 'G' |] ("Bad",()) =! ("Good",())

let mutable mutableValue = 0

let moduleValueReturnsLambda =
    mutableValue <- 5
    fun () -> mutableValue * 2

let moduleMethodReturnsLambda i =
    mutableValue <- i
    fun j -> mutableValue * j

[<Test>]
let ``Module values/methods returning lambdas work``() =
    moduleValueReturnsLambda() |> equal 10
    moduleMethodReturnsLambda 7 9 |> equal 63
    // mutableValue has changed so this produces a different result
    moduleValueReturnsLambda() |> equal 14

let mutable mutableValue2 = 0

type LambdaFactory() =
    member x.ClassPropertyReturnsLambda =
        mutableValue2 <- 5
        fun i -> mutableValue2 * i
    member x.ClassMethodReturnsLambda y =
        mutableValue2 <- y
        fun z -> mutableValue2 * z

[<Test>]
let ``Class properties/methods returning lambdas work``() =
    let x = LambdaFactory()
    x.ClassPropertyReturnsLambda 5 |> equal 25
    x.ClassMethodReturnsLambda 2 8 |> equal 16
    // Class properties are actually methods,
    // so this should still give the same result
    x.ClassPropertyReturnsLambda 5 |> equal 25

[<Test>]
let ``Local values returning lambdas work``() =
    let mutable mutableValue = 0
    let localValueReturnsLambda =
        mutableValue <- 5
        fun () -> mutableValue * 2
    let localFunctionReturnsLambda i =
        mutableValue <- i
        fun j -> mutableValue * j
    localValueReturnsLambda() |> equal 10
    localFunctionReturnsLambda 7 9 |> equal 63
    // mutableValue has changed so this produces a different result
    localValueReturnsLambda() |> equal 14

let genericLambdaArgument f = f 42
let genericLambdaArgument2 f g = f (fun x -> g)

[<Test>]
let ``Generic lambda arguments work``() =
    genericLambdaArgument (fun x y -> x + y) 3 |> equal 45
    genericLambdaArgument ((+) 1) |> equal 43
    genericLambdaArgument2 (fun f -> f 1) 3 |> equal 3
    genericLambdaArgument2 (fun f -> f 1 2) id |> equal 2

[<Test>]
let ``Generic lambda arguments work locally``() =
    let genericLambdaArgument f = f 42
    genericLambdaArgument (+) 3 |> equal 45
    genericLambdaArgument (fun x -> x + 1) |> equal 43

    let genericLambdaArgument2 f g = f (fun x -> g)
    genericLambdaArgument2 (fun f -> f 1) 3 |> equal 3
    genericLambdaArgument2 (fun f -> f 1 2) id |> equal 2

let partialApplication(f: int->int->int) =
    let f2 = f 1
    let f3 = fun x y -> x - y
    let f3' = (*)
    let f4 = f3 2
    let f4' = f3' 3
    f2 7 + f4 8 + f4' 9

[<Test>]
let ``Lambdas can be partially applied``() =
    partialApplication (+) |> equal 29

[<Test>]
let ``Flattened lambdas can be composed``() = // See #704
    let f = (+) >> id
    List.foldBack f [1;2;3;4] 0
    |> equal 10

type ImplicitType<'a,'b> =
    | Case1 of 'a
    | Case2 of 'b
    static member op_Implicit(x:'a) = ImplicitType.Case1 x
    static member op_Implicit(x:'b) = ImplicitType.Case2 x

let inline (!+) (x:^t1) : ^t2 = ((^t1 or ^t2) : (static member op_Implicit : ^t1 -> ^t2) x)

let implicitMethod (arg: ImplicitType<string, int>) (i: int) =
    match arg with
    | ImplicitType.Case1 _ -> 1
    | ImplicitType.Case2 _ -> 2

[<Test>]
let ``TraitCall can resolve overloads with a single generic argument``() =
    implicitMethod !+"hello" 5 |> equal 1
    implicitMethod !+6       5 |> equal 2

[<Test>]
let ``NestedLambdas``() =
    let mutable m = 0
    let f i =
        m <- i
        fun j ->
            m <- m + j
            fun k ->
                m <- m + k
                fun u ->
                    m + u
    let f2 = f 1
    let f3 = f2 2
    let f4 = f3 3
    f4 4 |> equal 10
    let f5 = f 6 7 8
    f5 9 |> equal 30

[<Test>]
let ``More than two lambdas can be nested``() =
    let mutable mut = 0
    let f x =
        mut <- mut + 1
        fun y z ->
            mut <- mut + 1
            fun u w ->
                x + y + z + u + w
    f 1 2 3 4 5 |> equal 15
    let f2 = f 3 4 5 6
    f2 7 |> equal 25

[<Test>]
let ``Multiple nested lambdas can be partially applied``() =
    let mutable mut = 0
    let f x y z =
        mut <- mut + 1
        fun u ->
            mut <- mut + 1
            fun w ->
                x + y + z + u + w
    let f2 = f 1 2
    f2 3 4 5 |> equal 15

open Microsoft.FSharp.Core.OptimizedClosures

[<Test>]
let ``Partial application of optimized closures works``() =
  let mutable m = 1
  let f x = m <- m + 1; (fun y z -> x + y + z)
  let f = FSharpFunc<_,_,_,_>.Adapt(f)
  let r = f.Invoke(1, 2, 3)
  equal 2 m
  equal 6 r

[<Test>]
let ``No errors because references to missing unit args``() =
    let foofy str =
        fun () -> "foo" + str
    let f1 = foofy "bar"
    f1 () |> equal "foobar"

type ArityRecord = { arity2: int->int->string }

[<Test>]
let ``Arity is checked also when constructing records``() =
    let f i j = (i * 2) + (j * 3)
    let r = { arity2 = fun x -> f x >> fun y -> sprintf "foo%i" y }
    r.arity2 4 5 |> equal "foo23"

type RecordB = {
    A: string
    B: bool
}
with
    static member New = {
        A = ""
        B = false
    }

    // Aether
    static member A_ : Lens<RecordB, string> = (fun x -> x.A), (fun value x -> { x with A = value })
    static member B_ : Lens<RecordB, bool> = (fun x -> x.B), (fun value x -> { x with B = value })

type RecordA = {
    RecordB: RecordB
}
with
    static member New = {
        RecordB = RecordB.New
    }

    // Aether
    static member RecordB_ : Lens<RecordA, RecordB> = (fun x -> x.RecordB), (fun value x -> { x with RecordB = value })

type Action<'model> =
    | InputChanged of Id: string * Value: string * Lens<'model, string>
    | CheckboxChanged of Id: string * Value: bool * Lens<'model, bool>

// type Action =
//     | InputChanged of Id: string * Value: string * Lens<RecordB, string>
//     | CheckboxChanged of Id: string * Value: bool * Lens<RecordB, bool>
with
    override x.ToString () =
        match x with
        | InputChanged (id, value, _) -> sprintf "InputChanged (%s, %s)" id value
        | CheckboxChanged (id, value, _) -> sprintf "CheckboxChanged (%s, %b)" id value

let makeInput<'model> id (model: 'model) (lens: Lens<'model, string>) =
// let makeInput id (model: RecordB) (lens: Lens<RecordB, string>) =
    Optic.get lens model

let makeCheckbox<'model> id (model: 'model) (lens: Lens<'model, bool>) =
// let makeCheckbox id (model: RecordB) (lens: Lens<RecordB, bool>) =
    Optic.get lens model

let view (model: RecordA) =
    let subModel = Optic.get RecordA.RecordB_ model
    makeInput<RecordB> "A" subModel RecordB.A_,
    makeCheckbox<RecordB> "B" subModel RecordB.B_
    // makeInput "A" subModel RecordB.A_,
    // makeCheckbox "B" subModel RecordB.B_

let update (model: RecordA) action =
    match action with
    | InputChanged (id, value, lens) ->
        Optic.set (RecordA.RecordB_ >-> lens) value model
    | CheckboxChanged (id, value, lens) ->
        Optic.set (RecordA.RecordB_ >-> lens) value model


[<Test>]
let ``Aether with generics works``() = // See #750
    let a = { RecordB = {A= "foo"; B=true} }
    let input, checkbox = view a
    input |> equal "foo"
    checkbox |> equal true
    let a2 = InputChanged("abc", "bar", RecordB.A_) |> update a
    let input2, checkbox2 = view a2
    input2 |> equal "bar"
    checkbox2 |> equal true

type Id = Id of string

type Ideable =
    { Id: Id; Name: string }
    with override this.ToString() = this.Name

let inline replaceById< ^t when ^t : (member Id : Id)> (newItem : ^t) (ar: ^t[]) =
    Array.map (fun (x: ^t) -> if (^t : (member Id : Id) newItem) = (^t : (member Id : Id) x) then newItem else x) ar

[<Test>]
let ``Trait calls work with record fields``() =
    let ar = [| {Id=Id"foo"; Name="Sarah"}; {Id=Id"bar"; Name="James"} |]
    replaceById {Id=Id"ja"; Name="Voll"} ar |> Seq.head |> string |> equal "Sarah"
    replaceById {Id=Id"foo"; Name="Anna"} ar |> Seq.head |> string |> equal "Anna"

let doNothing () = ()

[<Test>]
let ``Unit expression arguments are not removed``() =
    let mutable x = 0
    let foo i =
        x <- i
    doNothing <| foo 5
    equal 5 x


let curry (fn: 'a -> 'b -> 'c) =
  let first = fun (a: 'a) ->
    let second = fun (b: 'b) ->
      let result = fn a b
      result
    second
  first

[<Test>]
let ``Basic currying works``() =
    let plus = curry (+)
    let result = plus 2 3
    equal 5 result
    equal 5 (plus 2 3)
    equal 5 ((curry (+)) 2 3)

let applyTup2 f1 f2 x =
  let a = f1 x
  let b = f2 x
  (a,b)

let inline applyTup2Inline f1 f2 x =
  let a = f1 x
  let b = f2 x
  (a,b)

let mutable mutableValue3 = 0

let mutateAndLambdify x =
    mutableValue3 <- x
    (fun _ -> x)

[<Test>]
let ``CurriedLambda don't delay side effects unnecessarily``() = // See #996
      let a, b = applyTup2 id mutateAndLambdify 2685397
      sprintf "%A" mutableValue3 |> equal "2685397"
      let a2, b2 = applyTup2Inline id mutateAndLambdify 843252
      sprintf "%A" mutableValue3 |> equal "843252"
      let a3, b3 =
          let a = id 349787
          let b = mutateAndLambdify 349787
          (a,b)
      sprintf "%A" mutableValue3 |> equal "349787"

module Types =
    let inline flip f a b = f b a

    type StringField =
        { Value : string }

        static member Empty =
            { Value = "" }

    let setValue value stringField =
        { stringField with Value = value }

    type Model =
        { Email : StringField }

    let setEmail email model =
        { model with Email = email }

    let asEmailIn =
        flip setEmail

module State =
    open Types

    let update email (model: Types.Model) =
        model.Email
        |> setValue email
        |> asEmailIn model

[<Test>]
let ``Point-free style with multiple arguments works``() = // See #1041
    let initialValue = { Types.Email = Types.StringField.Empty }
    let m = State.update "m" initialValue
    m.Email.Value |> equal "m"


module CurriedApplicativeTests =

    module Option =
        let apply x f =
            match (x, f) with
            | Some x, Some f -> Some (f x)
            | _ -> None

        module Operators =
            let inline (<*>) m x = apply x m

    open Option.Operators

    [<Test>]
    let ``Option.apply (<*>) non-curried`` () =
        let f x = x + 1
        let r = Some f <*> Some 2
        r |> equal (Some 3)

    [<Test>]
    let ``Option.apply (<*>) auto curried`` () =
        let f x y = x + y
        let r = Some f <*> Some 2 <*> Some 3
        r |> equal (Some 5)

    [<Test>]
    let ``Option.apply (<*>) manually curried workaround`` () =
        let f x =
            let f' = fun y -> x + y
            f'
        let r = Some f <*> Some 2 <*> Some 3
        r |> equal (Some 5)

type StringEnvironment<'a> = string -> 'a

[<Test>]
let ``Function generic type alias works`` () : unit = // See #1121
    let five = fun _ -> 5

    let bind (x : StringEnvironment<'a>) (f : 'a -> StringEnvironment<'b>) : StringEnvironment<'b> =
        fun environment ->
            f (x environment) environment

    bind five (fun i -> five) "environment"
    |> equal 5

[<Test>]
let ``Function generic type alias works II`` () : unit =
    let three = fun _ -> 3

    let bind (x : string -> 'a) (f : 'a -> string -> 'b) : string -> 'b =
        fun environment ->
            f (x environment) environment

    bind three (fun i -> three) "environment"
    |> equal 3

type Node(parent: HTMLElement option) =
  member __.parentElement: HTMLElement = parent.Value

and Element(w, h, parent) =
  inherit Node(parent)
  member __.clientWidth: int = w
  member __.clientHeight: int = h

and HTMLElement(w, h, ?parent) =
  inherit Element(w, h, parent = parent)

let getElement(): Element =
  upcast HTMLElement(0, 1, HTMLElement(1, 0, HTMLElement(2, 2)))

[<Test>]
let ``Closures generated by casts work`` () = // See #1150
  let rec loop (current : Element) width height =
    let w = current.clientWidth
    let h = current.clientHeight
    if w > 0 && h > 0 then
      w, h
    else
      loop current.parentElement w h
  let element = getElement()
  let result = loop element 0 0
  equal (2,2) result

let foo2 a b c d = a, b + c d
let bar2 a = foo2 1 a
let baz = bar2 2 (fun _ -> 3) ()
let baz2 =
    let b2 = bar2 2
    let b3 = b2 (fun _ -> 3)
    b3 ()

[<Test>]
let ``Applying to a function returned by a member works``() =
    equal (1,5) baz
    equal (1,5) baz2

[<Test>]
let ``Applying to a function returned by a local function works``() =
    let foo a b c d = a , b + c d
    let bar a = foo 1 a
    let baz = bar 2 (fun _ -> 3) ()
    equal (1,5) baz

let mutable counter = 0
let next () =
  let result = counter
  counter <- counter + 1
  result

let adder () =
  let add a b = a + b
  add (next())

let add = adder ()

[<Test>]
let ``Partially applied functions don't duplicate side effects``() = // See #1156
    add 1 + add 2 + add 3 |> equal 6

[<Test>]
let ``Partially applied functions don't duplicate side effects locally``() =
    let mutable counter = 0
    let next () =
      let result = counter
      counter <- counter + 1
      result
    let adder () =
      let add a b = a + b
      add (next())
    let add = adder ()
    add 1 + add 2 + add 3 |> equal 6

type Foo3() =
    let mutable z = 5
    member __.GetLambda() =
        fun x y -> x + y + z
    member __.GetCurriedLambda() =
        fun x ->
            z <- z + 3
            fun y -> x + y + z

[<Test>]
let ``Partially applied lambdas capture this``() =
    let foo = Foo3()
    let f = foo.GetLambda()
    let f2 = f 2
    f2 3 |> equal 10

[<Test>]
let ``Partially applied curried lambdas capture this``() =
    let foo = Foo3()
    let f = foo.GetCurriedLambda()
    let f2 = f 2
    f2 4 |> equal 14

