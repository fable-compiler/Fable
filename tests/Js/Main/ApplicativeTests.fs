module Fable.Tests.Applicative

open System
open Util.Testing

let inline (|HasLength|) x =
  fun () -> (^a: (member Length: int) x)

let inline length (HasLength f) = f()

let lengthWrapper (xs:'a list) = length xs
let lengthFixed = length [|1; 2; 3|]

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

let inline applyInline (a:'a) (b:'b) =
    a <*> b

let sideEffectsCounter =
    let mutable i = 0
    fun () ->
        i <- i + 1
        i

type SideEffects =
    { a     : int
      b     : int
      c     : int
      d     : int
      count : int }
    static member New(n) =
        { a     = n
          b     = n * 2
          c     = n * 3
          d     = n * 4
          count = sideEffectsCounter() } // <== should only be called once per invocation

    member        this.MethodN (v:bool) = { this with a = this.a * if v then 2 else 3 }
    member inline this.MethodI (v:bool) = { this with a = this.a * if v then 2 else 3 }
    member        this.MethodN ()       = { this with a = this.a * 10                 }
    member inline this.MethodI ()       = { this with a = this.a * 10                 }

let inline getFirstName x = (^T : (member FirstName : string) x)
let inline getLastName x = (^T : (member LastName : string) x)

let inline getFirstAndLastName x =
    (^T : (member FirstName : string) x) + " " + (^T : (member LastName : string) x)

let inline getFullName x =
    let lname = getLastName x
    let fname = getFirstName x
    lname + " " + fname

let inline addSomething1 (x: ^T) = (^T : (member Add : int -> string) (x, 3))
let inline addSomething2 (x: ^T) = (^T : (member Add : string -> string) (x, "abc"))

// This code doesn't compile in F# unless `addSomething2` requires a static member or a member with a different name
// let inline testAddSomething (x: ^T when ^T : (member Add : int -> string) and ^T : (member Add2 : string -> string)) =
//     let y = add1 x
//     let z = add2 x
//     y + z

let tests1 = [
    testCase "Picks the right witness" <| fun () ->
        getFullName {| FirstName = "Alfonso"; LastName = "Horigome" |}
        |> equal "Horigome Alfonso"

    testCase "Picks the right witness II" <| fun () ->
        getFirstAndLastName {| FirstName = "Alfonso"; LastName = "Horigome" |}
        |> equal "Alfonso Horigome"

    testCase "Infix applicative can be generated" <| fun () ->
        let r = Ok 1
        let a = Ok string
        match a <*> r with
        | Ok x -> equal "1" x
        | _ -> failwith "expected Ok('1')"

    testCase "Infix applicative with inline functions can be generated" <| fun () ->
        let r = Ok 1
        let a = Ok string
        match applyInline a r with
        | Ok x -> equal "1" x
        | _ -> failwith "expected Ok('1')"

    testCase "Infix applicative with inline composed functions can be generated" <| fun () ->
        let r = Ok 1
        let a = Ok (string >> int)
        match applyInline a r with
        | Ok x -> equal 1 x
        | _ -> failwith "expected Ok(1)"

    testCase "Infix applicative with even more inline functions can be generated" <| fun () ->
        let r = Ok (fun x -> x + 1)
        let a = Ok (fun f x -> f x)
        match applyInline a r with
        | Ok addOne -> equal 2 (addOne 1)
        | _ -> failwith "expected Ok(addOne) where addOne(1) = 2"

    testCase "Inline code doesn't call many times function with side effects" <| fun () -> // See #1321
        SideEffects.New(5).MethodN(false).MethodN(true).MethodN().count |> equal 1
        SideEffects.New(5).MethodN(false).MethodN(true).MethodN().count |> equal 2
        SideEffects.New(5).MethodI(false).MethodI(true).MethodI().count |> equal 3
        SideEffects.New(5).MethodI(false).MethodI(true).MethodI().count |> equal 4
]

type Foo1(i) =
    member x.Foo() = i
    member x.Foo(j) = i + j

type Foo2(i) =
    member x.Foo(j) = (i + j) * 2

let inline foo< ^t when ^t : (member Foo : int -> int)> x i =
    (^t : (member Foo : int -> int) (x, i))

let tests2 = [
    testCase "Local inline typed lambdas work" <| fun () ->
        let inline localFoo (x:^t) = foo x 5
        let x1 = Foo1(2)
        let x2 = Foo2(2)
        equal 7 <| localFoo x1
        equal 14 <| localFoo x2

    testCase "Local inline values work" <| fun () ->
        let res = zipAny [|("a",1);("b",2)|] [|("c",5.);("a",4.)|]
        res.Length |> equal 3
        res.[0] |> fst |> equal "a"
        res.[0] |> snd |> equal (Some 1, Some 4.)
        res.[1] |> fst |> equal "b"
        res.[1] |> snd |> equal (Some 2, None)
        res.[2] |> fst |> equal "c"
        res.[2] |> snd |> equal (None, Some 5.)

    testCase "Local inline lambdas work standalone" <| fun () -> // See #1234
        let mutable setInternalState = Some(fun (_:obj) -> ())
        let withReact () =
          let mutable lastModel = Some 2
          let setState () =
            let inline notEqual a = not <| obj.ReferenceEquals (1, a)
            match setInternalState with
            | Some _ ->
                let hasUpdate = Option.map notEqual lastModel |> Option.defaultValue true
                hasUpdate
            | None ->
                false
          setState()
        withReact() |> equal true
]

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

let tests3 = [
    testCase "Lens.get returns correct values" <| fun () ->
        Lens_get fst_ ("Good","Bad") =! "Good"

    testCase "Lens.set sets value correctly" <| fun () ->
        Lens_set fst_ "Good" ("Bad",()) =! ("Good",())

    testCase "Lens.map modifies values correctly" <| fun () ->
        Lens_map fst_ (fun x -> x + x) ("Good",()) =! ("GoodGood",())

    testCase "Ismorphism composition over a lens gets value" <| fun () ->
        Lens_get (fst_ >-> chars) ("Good",()) =! [| 'G'; 'o'; 'o'; 'd' |]

    testCase "Ismorphism composition over a lens sets value" <| fun () ->
        Lens_set (fst_ >-> chars) [| 'G'; 'o'; 'o'; 'd' |] ("Bad",()) =! ("Good",())

    testCase "Ismorphism composition over a lens gets value over multiple isomorphisms" <| fun () ->
        Lens_get (fst_ >-> chars >-> rev) ("dooG",()) =! [| 'G'; 'o'; 'o'; 'd' |]

    testCase "Ismorphism composition over a lens sets value over multiple isomorphisms" <| fun () ->
        Lens_set (fst_ >-> chars >-> rev) [| 'd'; 'o'; 'o'; 'G' |] ("Bad",()) =! ("Good",())
]

let mutable mutableValue = 0

let moduleValueReturnsLambda =
    mutableValue <- 5
    fun () -> mutableValue * 2

let moduleMethodReturnsLambda i =
    mutableValue <- i
    fun j -> mutableValue * j

let mutable mutableValue2 = 0

type LambdaFactory() =
    member x.ClassPropertyReturnsLambda =
        mutableValue2 <- 5
        fun i -> mutableValue2 * i
    member x.ClassMethodReturnsLambda y =
        mutableValue2 <- y
        fun z -> mutableValue2 * z

let genericLambdaArgument f = f 42
let genericLambdaArgument2 f g = f (fun x -> g)

let partialApplication(f: int->int->int) =
    let f2 = f 1
    let f3 = fun x y -> x - y
    let f3' = fun x y -> x * y
    let f4 = f3 2
    let f4' = f3' 3
    f2 7 + f4 8 + f4' 9

let tests4 = [
    testCase "Module values/methods returning lambdas work" <| fun () ->
        moduleValueReturnsLambda() |> equal 10
        moduleMethodReturnsLambda 7 9 |> equal 63
        // mutableValue has changed so this produces a different result
        moduleValueReturnsLambda() |> equal 14

    testCase "Class properties/methods returning lambdas work" <| fun () ->
        let x = LambdaFactory()
        x.ClassPropertyReturnsLambda 5 |> equal 25
        x.ClassMethodReturnsLambda 2 8 |> equal 16
        // Class properties are actually methods,
        // so this should still give the same result
        x.ClassPropertyReturnsLambda 5 |> equal 25

    testCase "Local values returning lambdas work" <| fun () ->
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

    testCase "Generic lambda arguments work" <| fun () ->
        genericLambdaArgument (fun x y -> x + y) 3 |> equal 45
        genericLambdaArgument ((+) 1) |> equal 43
        genericLambdaArgument2 (fun f -> f 1) 3 |> equal 3

    testCase "Generic lambda arguments work with multi-arity subargument" <| fun () ->
        // TODO: Add test also with expected arity for f > 1
        genericLambdaArgument2 (fun f -> f 1 2) id |> equal 2

    testCase "Generic lambda arguments work locally" <| fun () ->
        let genericLambdaArgument f = f 42
        genericLambdaArgument (+) 3 |> equal 45
        genericLambdaArgument (fun x -> x + 1) |> equal 43

        let genericLambdaArgument2 f g = f (fun x -> g)
        genericLambdaArgument2 (fun f -> f 1) 3 |> equal 3

    testCase "Lambdas can be partially applied" <| fun () ->
        partialApplication (+) |> equal 29

    testCase "Flattened lambdas can be composed" <| fun () -> // See #704
        let f = (+) >> id
        List.foldBack f [1;2;3;4] 0
        |> equal 10
]

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

type ArityRecord = { arity2: int->int->string }

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
// with
//     override x.ToString () =
//         match x with
//         | InputChanged (id, value, _) -> sprintf "InputChanged (%s, %s)" id value
//         | CheckboxChanged (id, value, _) -> sprintf "CheckboxChanged (%s, %b)" id value

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

type Item2 = static member inline Invoke value = (^t : (member Item2 : _) value)

type Ideable2 =
    { Id: string; Name: string }

type Id = Id of string

type Ideable =
    { Id: Id; Name: string }
    // with override this.ToString() = this.Name

let inline replaceById< ^t when ^t : (member Id : Id)> (newItem : ^t) (ar: ^t[]) =
    Array.map (fun (x: ^t) -> if (^t : (member Id : Id) newItem) = (^t : (member Id : Id) x) then newItem else x) ar

let inline getId (x: ^a when ^a:(member Id : string)) =
    (^a : (member Id : string) x)

let inline wrapId x =
    let id = getId x
    "<<<" + id + ">>>"

type Parseable = Parseable with static member Parse (_: string) = Parseable

type Parse =

    static member inline Parse (_: ^R, _: obj  ) = fun (x:string) -> (^R: (static member Parse : _ -> ^R) x)
    static member inline Parse (_: ^R, _: Parse) = fun (x:string) -> (^R: (static member Parse : _ * _ -> ^R) (x, Globalization.CultureInfo.InvariantCulture))

    static member inline Parse (_: 'T when 'T : enum<_>, _: Parse) = fun (x: string) ->
        (match Enum.TryParse (x) with
            | (true, v) -> v
            | _         -> invalidArg "value" ("Requested value '" + x + "' was not found.")
        ) : 'enum

    static member Parse (_: bool              , _: Parse) = fun (x:string) -> Boolean.Parse (x)
    static member Parse (_: char              , _: Parse) = fun  x -> Char.Parse (x)
    static member Parse (_: string            , _: Parse) = id : string->_
    static member Parse (_: Text.StringBuilder, _: Parse) = fun  x -> new Text.StringBuilder (x: string)

    static member inline Invoke (value: string) =
        let inline call_2 (a: ^a, b: ^b) = ((^a or ^b) : (static member Parse : _*_ -> _) b, a)
        let inline call (a: 'a) = fun (x: 'x) -> call_2 (a, Unchecked.defaultof<'r>) x : 'r
        call Unchecked.defaultof<Parse> value

let inline parse (value: string) = Parse.Invoke value

let doNothing () = ()

let curry (fn: 'a -> 'b -> 'c) =
  let first = fun (a: 'a) ->
    let second = fun (b: 'b) ->
      let result = fn a b
      result
    second
  first

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

type Grandchild =
    { x : int option }
    static member x_ : Prism<Grandchild, int> =
        (fun p -> p.x),
        (fun x p ->
            match p with
            | p when p.x.IsSome -> { p with x = Some x }
            | p -> p)

type Child =
    { g : Grandchild }
    static member g_ : Lens<Child, Grandchild> = (fun p -> p.g), (fun x p -> { p with g = x })

type Parent =
    { c : Child option }
    static member c_ : Prism<Parent, Child> =
        (fun p -> p.c),
        (fun x p ->
            match p with
            | p when p.c.IsSome -> { p with c = Some x }
            | p -> p)

let tests5 = [
    testCase "TraitCall can resolve overloads with a single generic argument" <| fun () ->
        implicitMethod !+"hello" 5 |> equal 1
        implicitMethod !+6       5 |> equal 2

    testCase "NestedLambdas" <| fun () ->
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

    testCase "More than two lambdas can be nested" <| fun () ->
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

    testCase "Multiple nested lambdas can be partially applied" <| fun () ->
        let mutable mut = 0
        let f x y z =
            mut <- mut + 1
            fun u ->
                mut <- mut + 1
                fun w ->
                    x + y + z + u + w
        let f2 = f 1 2
        f2 3 4 5 |> equal 15

    // TODO
    // open Microsoft.FSharp.Core.OptimizedClosures

    // testCase "Partial application of optimized closures works" <| fun () ->
    //   let mutable m = 1
    //   let f x = m <- m + 1; (fun y z -> x + y + z)
    //   let f = FSharpFunc<_,_,_,_>.Adapt(f)
    //   let r = f.Invoke(1, 2, 3)
    //   equal 2 m
    //   equal 6 r

    testCase "No errors because references to missing unit args" <| fun () ->
        let foofy str =
            fun () -> "foo" + str
        let f1 = foofy "bar"
        f1 () |> equal "foobar"

    testCase "Arity is checked also when constructing records" <| fun () ->
        let f i j = (i * 2) + (j * 3)
        let r = { arity2 = fun x -> f x >> fun y -> sprintf "foo%i" y }
        r.arity2 4 5 |> equal "foo23"

    testCase "Aether with generics works" <| fun () -> // See #750
        let a = { RecordB = {A= "foo"; B=true} }
        let input, checkbox = view a
        input |> equal "foo"
        checkbox |> equal true
        let a2 = InputChanged("abc", "bar", RecordB.A_) |> update a
        let input2, checkbox2 = view a2
        input2 |> equal "bar"
        checkbox2 |> equal true

    testCase "Aether works II" <| fun () -> // See #2101
        let x : Parent = { c = Some { g = { x = Some 5 } } }
        let y : Parent = { c = None }
        let z : Parent = { c = Some { g = { x = None } } }
        let grandchild = Parent.c_ >?> Child.g_ >?> Grandchild.x_
        x ^. grandchild |> equal (Some 5)
        y ^. grandchild |> equal None
        z ^. grandchild |> equal None

    testCase "Trait calls work with tuples" <| fun () ->
        Item2.Invoke (1,2,3) |> equal 2

    testCase "Trait calls work with record fields" <| fun () ->
        let ar = [| {Id=Id"foo"; Name="Sarah"}; {Id=Id"bar"; Name="James"} |]
        replaceById {Id=Id"ja"; Name="Voll"} ar |> Seq.head |> fun x -> equal "Sarah" x.Name
        replaceById {Id=Id"foo"; Name="Anna"} ar |> Seq.head |> fun x -> equal "Anna" x.Name

    testCase "Nested trait calls work" <| fun () -> // See #2468
        let i: int  = parse "123"
        let b: bool = parse "true"
        let p: Parseable = parse ""
        let h : DateTimeOffset = parse "2011-03-04T15:42:19+03:00"
        equal 123 i
        equal true b
        equal Parseable p
        equal (DateTimeOffset(2011, 3, 4, 15, 42, 19, TimeSpan.FromHours(3.))) h

    testCase "Inline local function can call another inline function with trait call" <| fun _ ->
        let inline wrapIdLocal x =
            let id = getId x
            "<<<" + id + ">>>"
        let a = wrapId { Ideable2.Id="ABC"; Name="OOOO"}
        let b = wrapId {| Id = "xyz" |}
        let c = wrapIdLocal { Ideable2.Id="ABC"; Name="EEEE"}
        let d = wrapIdLocal {| Id = "xyz" |}
        equal "<<<ABC>>>" a
        equal "<<<xyz>>>" b
        equal "<<<ABC>>>" c
        equal "<<<xyz>>>" d

    testCase "Unit expression arguments are not removed" <| fun () ->
        let mutable x = 0
        let foo i =
            x <- i
        doNothing <| foo 5
        equal 5 x

    testCase "Basic currying works" <| fun () ->
        let plus = curry (+)
        let result = plus 2 3
        equal 5 result
        equal 5 (plus 2 3)
        equal 5 ((curry (+)) 2 3)

    testCase "CurriedLambda don't delay side effects unnecessarily" <| fun () -> // See #996
          let a, b = applyTup2 id mutateAndLambdify 2685397
          sprintf "%A" mutableValue3 |> equal "2685397"
          let a2, b2 = applyTup2Inline id mutateAndLambdify 843252
          sprintf "%A" mutableValue3 |> equal "843252"
          let a3, b3 =
              let a = id 349787
              let b = mutateAndLambdify 349787
              (a,b)
          sprintf "%A" mutableValue3 |> equal "349787"
]

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

type StringEnvironment<'a> = string -> 'a

let tests6 = [
    testCase "Point-free style with multiple arguments works" <| fun () -> // See #1041
        let initialValue = { Types.Email = Types.StringField.Empty }
        let m = State.update "m" initialValue
        m.Email.Value |> equal "m"

    testCase "Function generic type alias works" <| fun () -> // See #1121
        let five = fun _ -> 5

        let bind (x : StringEnvironment<'a>) (f : 'a -> StringEnvironment<'b>) : StringEnvironment<'b> =
            fun environment ->
                f (x environment) environment

        bind five (fun i -> five) "environment"
        |> equal 5

    testCase "Function generic type alias works II" <| fun () ->
        let three = fun _ -> 3

        let bind (x : string -> 'a) (f : 'a -> string -> 'b) : string -> 'b =
            fun environment ->
                f (x environment) environment

        bind three (fun i -> three) "environment"
        |> equal 3

    testCase "Piping to an alias of a function which returns a function works" <| fun () -> // See #2657
        let f a b = sprintf $"{a} {b}"

        let f_option = Some f
        let defaultValue x = Option.defaultValue x

        let functionA = f_option |> Option.defaultValue f
        functionA "functionA" "works" |> equal "functionA works"

        let functionB = defaultValue f f_option
        functionB "functionB" "works" |> equal "functionB works"

        let functionC = f_option |> defaultValue f
        functionC "functionC" "works" |> equal "functionC works"

        let functionC x = (f_option |> defaultValue f) x
        functionC "functionC" "works" |> equal "functionC works"

        let functionC x y = (f_option |> defaultValue f) x y
        functionC "functionC" "works" |> equal "functionC works"

    testCase "Piping to an alias of a function which returns a function works II" <| fun () -> // See #2657
        let f a b = sprintf $"{a} {b}"

        let getFunction x y = y
        let getFunctionAlias = getFunction

        let functionA = f |> getFunction 1
        functionA "functionA" "works" |> equal "functionA works"

        let functionB = getFunctionAlias 1 f
        functionB "functionB" "works" |> equal "functionB works"

        let functionC = f |> getFunctionAlias 2
        functionC "functionC" "works?" |> equal "functionC works?"
]

module CurriedApplicative =
    type Option2<'T> =
        | Some2 of 'T
        | None2

    module Option =
        let apply x f =
            match (x, f) with
            | Some x, Some f -> Some (f x)
            | _ -> None

        let apply2 x f =
            match (x, f) with
            | Some2 x, Some2 f -> Some2 (f x)
            | _ -> None2

        module Operators =
            let inline (<*>) m x = apply x m
            let inline (<**>) m x = apply2 x m

    open Option.Operators

    let apply3 (f: 'a->'a->'b->'c) (a: 'a) =
        f a a

    let changeArity (f: 'a->'a->'a->'b) (a: 'a) =
        apply3 f a

    let inline changeArityInlined (f: 'a->'a->'a->'b) (a: 'a) =
        apply3 f a

    let addModule5 h i j k l = h + i + j + k + l

    let tests = [
        testCase "Option.apply (<*>) non-curried" <| fun () ->
            let f x = x + 1
            let r = Some f <*> Some 2
            r |> equal (Some 3)

        testCase "Option.apply (<*>) auto curried" <| fun () ->
            let f x y = x + y
            let r = Some f <*> Some 2 <*> Some 3
            r |> equal (Some 5)

        testCase "Option.apply (<**>) auto curried" <| fun () ->
            let f x y = x + y
            let r = Some2 f <**> Some2 2 <**> Some2 3
            r |> equal (Some2 5)

        testCase "Option.apply (<*>) manually curried workaround" <| fun () ->
            let f x =
                let f' = fun y -> x + y
                f'
            let r = Some f <*> Some 2 <*> Some 3
            r |> equal (Some 5)

        testCase "Currying/uncurrying works" <| fun () ->
            let addLocal x y z u v = x + y + z + u + v
            let f1 = changeArity addModule5 2
            let f2 = changeArity addLocal 2
            let f3 = changeArityInlined addModule5 2
            let f4 = changeArityInlined addLocal 2
            f1 1 2 3 |> equal 10
            f2 1 2 3 |> equal 10
            f3 1 2 3 |> equal 10
            f4 1 2 3 |> equal 10
    ]

type Node(parent: HTMLElement option) =
  member _.parentElement: HTMLElement = parent.Value

and Element(w, h, parent) =
  inherit Node(parent)
  member _.clientWidth: int = w
  member _.clientHeight: int = h

and HTMLElement(w, h, ?parent) =
  inherit Element(w, h, parent = parent)

let getElement(): Element =
  upcast HTMLElement(0, 1, HTMLElement(1, 0, HTMLElement(2, 2)))

let foo2 a b c d = a, b + c d
let bar2 a = foo2 1 a
let baz = bar2 2 (fun _ -> 3) ()
let baz2 =
    let b2 = bar2 2
    let b3 = b2 (fun _ -> 3)
    b3 ()

let mutable counter = 0
let next () =
  let result = counter
  counter <- counter + 1
  result

let useAdder adder x y z =
    let add = adder ()
    add x + add y + add z

let adder () =
  let add a b = a + b
  add (next())

let ADD = adder ()

type Foo3() =
    let mutable z = 5
    member _.GetLambda() =
        fun x y -> x + y + z
    member _.GetCurriedLambda() =
        fun x ->
            z <- z + 3
            fun y -> x + y + z

let apply f x =
    match f, x with
    | Some f, Some x -> Some (f x)
    | _ -> None

let maybeApply f a b =
    match f with
    | Some f -> f a b
    | None -> b

type FooRec = { myFunction: int->int->int->int }

type BarRec = { fn : unit -> string -> int  }
let getStrLen (x: unit) (y: string) = y.Length

let apply3 f x y z = f x y z

let add2 a b = a + b
let add3 a b c = a + b + c
let add4 a b c d = a+b+c+d

module Pointfree =
    let (<!>) = Option.map
    let (<*>) = apply
    let y = add2 <!> Some 1 <*> Some 2

    let x = add3 <!> Some 40 <*> Some 1 <*> Some 1

module Pointful =
    let (<!>) f x = Option.map f x
    let (<*>) f x = apply f x
    let x = add3 <!> Some 40 <*> Some 1 <*> Some 1

    let testFunctionOptions () =
        let add1 = add4 <!> Some 1
        let thenAdd2 = add1 <*> Some 2
        let thenAdd3 = thenAdd2 <*> Some 3
        let sum = thenAdd3 <*> Some 4
        equal (Some 10) sum

module Results =
    open FSharp.Core

    let applyResults (f : Result<_, unit>) (x : Result<_, unit>) =
        match f, x with
        | Ok f, Ok x -> Ok (f x)
        | _ -> Result.Error ()

    let (<!>) f x = Result.map f x
    let (<*>) f x = applyResults f x

    let testOperatorsWith3Args () =
        let sum = add3 <!> Ok 1 <*> Ok 2 <*> Ok 3
        equal (Ok 6) sum

#if FABLE_COMPILER
open Thoth.Json.Decode
#endif

type User =
    { Id : int
      Name : string
      Email : string
      Followers : int }
    static member Create id email name followers =
        { Id = id
          Name = name
          Email = email
          Followers = followers }

type PrimaryConstructorUncurrying(f) =
    member val Value = f "a" "b"

type Fun = Fun of (int -> int -> int list)

type BaseClass (f: string -> string -> string) =
  member _.MakeString a b = f a b

type AddString () =
  inherit BaseClass (fun a b -> a + b)

type BaseClass2 (f: string -> string -> string) =
  member _.MakeString a b = f a b

type AddString2 (f: string -> string -> string) =
  inherit BaseClass2 (fun a b -> f a b + " - " + f b a)

type IAddFn = int -> int -> int
type IAdder =
    abstract Add: IAddFn

module private PseudoElmish =
    let justReturn2 (f: 'a->'b->'c) = f
    let justReturn3 (f: 'a->'b->'c->'d) = f

    type Dispatch<'msg> = 'msg -> unit
    type SetState<'model, 'msg> = 'model -> Dispatch<'msg> -> unit
    type Program<'model, 'msg> = { setState: SetState<'model, 'msg> }

    let mutable doMapRunTimes = 0
    let mutable setStateAccumulated = ""

    let reset() =
        doMapRunTimes <- 0
        setStateAccumulated <- ""

    let map (mapSetState: SetState<'model, 'msg> -> SetState<'model, 'msg>) (program: Program<'model, 'msg>) =
        { setState = mapSetState (program.setState) }

    let doMap (setState: SetState<'model, 'msg>): SetState<'model, 'msg> =
        doMapRunTimes <- doMapRunTimes + 1
        setState

    let withChanges (program: Program<'model,'msg>) =
        program |> map doMap

    let mapWithPreviousCurrying2 (mapSetState: SetState<'model, 'msg> -> SetState<'model, 'msg>) (program: Program<'model, 'msg>) =
        let mapSetState = justReturn2 mapSetState
        { setState = mapSetState (program.setState) }

    let withChangesAndCurrying2 (program: Program<'model,'msg>) =
        program |> mapWithPreviousCurrying2 doMap

    let mapWithPreviousCurrying3 (mapSetState: SetState<'model, 'msg> -> SetState<'model, 'msg>) (program: Program<'model, 'msg>) =
        let mapSetState = justReturn3 mapSetState
        // let mapSetState = justReturn3 (fun x y z -> mapSetState x y z)
        { setState = mapSetState (program.setState) }

    let withChangesAndCurrying3 (program: Program<'model,'msg>) =
        program |> mapWithPreviousCurrying3 doMap

    let testProgram: Program<string, string> = {
        setState = (fun m d -> setStateAccumulated <- setStateAccumulated + m)
    }

let mul x y = x * y

let addOne (add: int->int->int) x = add 1 x
let pointFree_addOne = addOne
let fortyTwo x y = x + "42" + y
let fortyTwo2 x y () z = 42 + (FSharp.Core.Operators.int x) + (FSharp.Core.Operators.int y) + z
let wrap someFun () () = someFun "4" "6"
let doesWork someFun = wrap someFun
let doesNotWork = wrap

type RFoo<'a,'b> = { foo: 'a -> 'b }
type RFoo2<'a> = { foo: 'a }

let applyFooInRecord (f: RFoo<'a,'b>) (a: 'a) = f.foo a
let applyFooInRecord2 (f: RFoo2<'a -> 'b>) (a: 'a) = f.foo a
let applyFooInRecord3 (f: RFoo2<'a -> 'b -> 'c>) (a: 'a) (b: 'b) = f.foo a b
let applyFooInAnonRecord (f: {| foo: 'a -> 'b |}) (a: 'a) = f.foo a
let applyFooInAnonRecord3 (f: {| foo: 'a -> 'b -> 'c |}) (a: 'a) (b: 'b) = f.foo a b

type Wrapper() =
    member _.doesNotWorki = wrap
    static member doesNotWork = wrap
    member _.doesNotWorki2 = wrap
    static member doesNotWork2 = wrap

type Fn = bool -> int -> string

type Thing =
    | In of Fn
    | Out of Fn

let findThing (things:Thing list) =
    let folder (a : Fn option) t =
        match t with
        | In x -> Some x // Found an In, set accumulator
        | _ -> a // pass accumulator through unchanged

    things |> List.fold folder None  // Searching for an "In x"

type First<'t> = First of Option<'t> with
    static member get_Zero () = First None : First<'t>
    static member run (First a) = a        : 't option

type Const<'t,'u> = Const of 't with
    static member inline Return (_: 'Y) = Const LanguagePrimitives.GenericZero : Const<'X,'Y>
    static member run (Const a) = a

let tests7 = [
    testCase "Currying/uncurrying works with imported functions" <| fun () -> // See #3397
        let r1 = apply3 Util.curriedAdder 4 5 6
        let r2 = apply3 Util.curriedAdder 5 6 7
        r1.result |> equal 16
        r2.result |> equal 19
        let r1 = apply3 Util.curriedAdder2 4 5 6
        let r2 = apply3 Util.curriedAdder2 5 6 7
        r1.result |> equal 16
        r2.result |> equal 19

    testCase "Ident of uncurried inner lambdas is uncurried too" <| fun () -> // See #3415
        let copy1 xs =
            let copySnd x y = x + y
            ignore copySnd // just to prevent inlining
            copySnd 3 xs |> id

        // copySnd is hoisted in release mode
        let copy2 xs =
            let copy x =
                let copySnd x y = x + y
                ignore copySnd // just to prevent inlining
                copySnd 3 x
            copy xs |> id

        copy1 5 |> equal 8
        copy2 5 |> equal 8

    testCase "SRTP with ActivePattern works" <| fun () ->
        (lengthWrapper []) |> equal 0
        (lengthWrapper [1;2;3;4]) |> equal 4
        lengthFixed |> equal 3

    testCase "SRTP with inlined functions relying on generic info works" <| fun () -> // See #2135
        let t2: int option =
            Error 1
            |> fun x ->  (^``Applicative<'T>`` : (static member Return : ^T -> ^``Applicative<'T>``) x)
            // |> Const.Return
            |> Const.run
            |> First.run

        equal None t2

    testCase "Closures generated by casts work" <| fun () -> // See #1150
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

    testCase "Applying to a function returned by a member works" <| fun () ->
        equal (1,5) baz
        equal (1,5) baz2

    testCase "Applying to a function returned by a local function works" <| fun () ->
        let foo a b c d = a , b + c d
        let bar a = foo 1 a
        let baz = bar 2 (fun _ -> 3) ()
        equal (1,5) baz

    testCase "Partially applied functions don't duplicate side effects" <| fun () -> // See #1156
        let result = ADD 1 + ADD 2 + ADD 3
        result |> equal 6
        counter |> equal 1

    testCase "Partially applied functions don't duplicate side effects locally" <| fun () ->
        let mutable counter = 0
        let next () =
            let result = counter
            counter <- counter + 1
            result
        let adder () =
            let add a b = a + b
            add (next())
        let add = adder ()
        let result = add 1 + add 2 + add 3
        result |> equal 6
        counter |> equal 1

    testCase "Partially applied functions don't duplicate side effects locally II" <| fun () ->
        let mutable counter = 0
        let next () =
            let result = counter
            counter <- counter + 1
            result
        let adder () =
            let add a b = a + b
            add (next())
        let result = useAdder adder 1 2 3
        result |> equal 6
        counter |> equal 1

    testCase "Partially applied lambdas capture this" <| fun () ->
        let foo = Foo3()
        let f = foo.GetLambda()
        let f2 = f 2
        f2 3 |> equal 10

    testCase "Partially applied curried lambdas capture this" <| fun () ->
        let foo = Foo3()
        let f = foo.GetCurriedLambda()
        let f2 = f 2
        f2 4 |> equal 14

    testCase "Curried function options work" <| fun () ->
        maybeApply (Some (*)) 5 6 |> equal 30
        maybeApply None 5 6 |> equal 6

    // See https://github.com/fable-compiler/Fable/issues/1199#issuecomment-347101093
    testCase "Applying function options works"
        Pointful.testFunctionOptions

    testCase "Point-free and partial application work" <| fun () -> // See #1199
        equal Pointfree.x Pointful.x

    // See https://github.com/fable-compiler/Fable/issues/1199#issuecomment-345958891
    testCase "Point-free works when passing a 2-arg function" <| fun () ->
        Pointfree.y |> equal (Some 3)

    testCase "Functions in record fields are uncurried" <| fun () ->
        let r = { myFunction = fun x y z -> x + y - z }
        r.myFunction 4 4 2 |> equal 6
        // If the function record field is assigned
        // to a variable, just curry it
        let mutable f = r.myFunction
        f 4 4 2 |> equal 6
        apply3 r.myFunction 5 7 4 |> equal 8
        apply (r.myFunction 1 1 |> Some) (Some 5) |> equal (Some -3)

    testCase "Uncurried functions in record fields can be partially applied" <| fun () ->
        // Test also non-record functions just in case
        let result = getStrLen () "hello"
        let partiallyApplied = getStrLen ()
        let secondResult = partiallyApplied "hello"
        equal 5 result
        equal 5 secondResult

        let record = { fn = getStrLen }
        let recordResult = record.fn () "hello"
        let recordPartiallyApplied = record.fn ()
        let recordSecondResult = recordPartiallyApplied "hello"
        equal 5 recordResult
        equal 5 recordSecondResult

    // See https://github.com/fable-compiler/Fable/issues/1199#issuecomment-347190893
    testCase "Applicative operators work with three-argument functions"
        Results.testOperatorsWith3Args

    testCase "partialApply works with tuples" <| fun () ->
        let sum x (y,z) =
            x + y + z
        let li =
            [1,2; 3,4; 5,6]
            |> List.map (sum 10)
        List.sum li |> equal 51

    testCase "Arguments of implicit constructors are uncurried too" <| fun () -> // See #1441
        let f1 x y = if x = y then 0 else 1
        let f2 x y = if x = y then 1 else 0
        PrimaryConstructorUncurrying(f1).Value |> equal 1
        PrimaryConstructorUncurrying(f2).Value |> equal 0

    testCase "Union case function fields are properly uncurried/curried" <| fun () -> // See #1445
        let (Fun f) = Fun (fun x y -> [x; y])
        let xs = f 3 4
        List.sum xs |> equal 7

    testCase "Lambdas with tuple arguments don't conflict with uncurrying" <| fun () -> // See #1448
        let revert xs =
            let rec rev ls (a,b) acc =
                match ls with
                | [] -> acc
                | h::t -> rev t (a,b) (h::acc)
            rev xs (0, 0) []
        let res = revert [2;3;4]
        equal 3 res.Length
        equal 4 res.Head

    testCase "Uncurrying works for base constructors" <| fun () -> // See #1458
        let str = AddString()
        str.MakeString "foo" "bar" |> equal "foobar"

    testCase "Uncurrying works for base constructors II" <| fun () -> // See #1459
        let str = AddString2 (fun a b -> "Prefix: " + a + b)
        str.MakeString "a" "b" |> equal "Prefix: ab - Prefix: ba"

    testCase "Sequence of functions is uncurried in folding" <| fun () ->
        let vals = [(4,5); (6,7)]
        let ops  = [(+); (-)]
        (-5, vals, ops) |||> List.fold2 (fun acc (v1,v2) op -> acc * op v1 v2)
        |> equal 45

#if FABLE_COMPILER
    testCase "Composing methods returning 2-arity lambdas works" <| fun _ ->
        let infoHelp version =
            match version with
            | 4 -> succeed 1
            | 3 -> succeed 1
            | _ -> fail <| "Trying to decode info, but version " + (version.ToString()) + "is not supported"

        let info : Decoder<int> =
            field "version" int
            |> andThen infoHelp

        decodeString info """{ "version": 3, "data": 2 }"""
        |> equal (FSharp.Core.Ok 1)

    testCase "Applying curried lambdas to a module value works" <| fun _ ->
        let expected =
            FSharp.Core.Ok(User.Create 67 "user@mail.com" "" 0)

        let userDecoder =
            decode User.Create
                |> required "id" int
                |> required "email" string
                |> optional "name" string ""
                |> hardcoded 0 // `hardcoded` is compiled as module value

        let actual =
            decodeString
                userDecoder
                """{ "id": 67, "email": "user@mail.com" }"""

        equal expected actual
#endif

    testCase "failwithf is not compiled as function" <| fun () ->
        let makeFn value =
            if value then
                // When one of the branches is a function
                // failwithf can be compiled to a function
                // because of optimizations
                fun x -> x + x
            else
                failwithf "Boom!"

        let mutable x = ""
        try
            // It should fail even if `f` is not called
            let f = makeFn false
            ()
        with ex -> x <- ex.Message
        equal "Boom!" x

    testCase "Partial Applying caches side-effects" <| fun () -> // See #1836
        PseudoElmish.reset()
        let changedProgram = PseudoElmish.withChanges PseudoElmish.testProgram
        changedProgram.setState "Foo" (fun _ -> ())
        changedProgram.setState "Bar" (fun _ -> ())
        PseudoElmish.doMapRunTimes |> equal 1
        PseudoElmish.setStateAccumulated |> equal "FooBar"

        PseudoElmish.reset()
        let changedProgram = PseudoElmish.withChangesAndCurrying3 PseudoElmish.testProgram
        changedProgram.setState "Foo" (fun _ -> ())
        changedProgram.setState "Bar" (fun _ -> ())
        PseudoElmish.doMapRunTimes |> equal 1
        PseudoElmish.setStateAccumulated |> equal "FooBar"

        PseudoElmish.reset()
        let changedProgram = PseudoElmish.withChangesAndCurrying2 PseudoElmish.testProgram
        changedProgram.setState "Foo" (fun _ -> ())
        changedProgram.setState "Bar" (fun _ -> ())
        PseudoElmish.doMapRunTimes |> equal 1
        PseudoElmish.setStateAccumulated |> equal "FooBar"

    testCase "Partial Applying locally caches side-effects" <| fun () -> // See #1836
        let mutable doMapRunTimes = 0
        let mutable setStateAccumulated = ""

        let inline map mapSetState (program: PseudoElmish.Program<'model, 'msg>) =
            { PseudoElmish.setState = mapSetState (program.setState) }

        let changedProgram =
            { PseudoElmish.setState = (fun m d -> setStateAccumulated <- setStateAccumulated + m) }
            |> map (fun setState ->
                doMapRunTimes <- doMapRunTimes + 1
                setState)

        changedProgram.setState "Foo" (fun _ -> ())
        changedProgram.setState "Bar" (fun _ -> ())
        changedProgram.setState "Baz" (fun _ -> ())

        doMapRunTimes |> equal 1
        setStateAccumulated |> equal "FooBarBaz"

    testCase "Fix lambda uncurry/curry semantic #1836" <| fun () ->
        let map (mapSetState: int -> (int -> unit))  =
            mapSetState 1

        let mutable doMapCalled = 0
        let mutable doMapResultCalled = 0
        let doMap (i:int) : (int -> unit) =
            doMapCalled <- doMapCalled + 1
            (fun j -> doMapResultCalled <- doMapResultCalled + 1)

        let setState = map (doMap)
        setState 1
        setState 2
        doMapCalled |> equal 1
        doMapResultCalled |> equal 2

    testCase "OptimizedClosures.FSharpFunc<_,_,_>.Adapt works" <| fun () -> // See #1904
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt (fun x y -> x + y)
        f.Invoke(3, 4) |> equal 7
        let f2 = OptimizedClosures.FSharpFunc<_,_,_>.Adapt mul
        f2.Invoke(3, 4) |> equal 12

    testCase "Arguments passed to point-free function are uncurried" <| fun () -> // See #1959
        let x = pointFree_addOne (+) 2
        let y = addOne (+) 2
        equal 3 x
        equal 3 y

    testCase "fold produces incorrect output when state is a function with arity > 1" <| fun () -> // See #2035
        let folder state value x y = value :: state x y
        let state x y = [x; y]
        let d = List.fold folder state [0..3]
        d 42 42 |> equal [3; 2; 1; 0; 42; 42]
        let s = Seq.fold folder state [0..3]
        s 15 20 |> equal [3; 2; 1; 0; 15; 20]

    testCase "Assigning a function with arity > 1 to a scoped mutable variable #2046" <| fun _ ->
        let mutable state = fortyTwo
        state "a" "b" |> equal "a42b"
        state <- fun x y -> x + "32" + y
        state "a" "b" |> equal "a32b"

    testCase "Uncurrying works with generic records returning lambdas" <| fun () ->
        applyFooInRecord { foo = fun x y -> x ** y } 5. 2. |> equal 25.
        let f = applyFooInRecord { foo = fun x y z -> x ** y + z } 5.
        f 3. 2. |> equal 127.
        applyFooInRecord2 { foo = fun x y -> x ** y } 5. 2. |> equal 25.
        let f = applyFooInRecord2 { foo = fun x y z -> x ** y + z } 5.
        f 3. 2. |> equal 127.
        applyFooInRecord3 { foo = fun x y z -> x ** y - z } 5. 2. 1. |> equal 24.
        let f = applyFooInRecord3 { foo = fun x y z -> x ** y - z } 5.
        f 3. 3. |> equal 122.

    testCase "Uncurrying works with generic anonymous records returning lambdas" <| fun () ->
        applyFooInAnonRecord {| foo = fun x y -> x ** y |} 5. 2. |> equal 25.
        let f = applyFooInAnonRecord {| foo = fun x y -> x ** y |} 5.
        f 3. |> equal 125.

        applyFooInAnonRecord3 {| foo = fun x y z -> x ** y - z |} 5. 2. 1. |> equal 24.
        let f = applyFooInAnonRecord3 {| foo = fun x y z -> x ** y - z |} 5.
        f 3. 3. |> equal 122.

    testCase "Curried functions being mangled via DU, List.fold and match combination #2356" <| fun _ ->
        let testData = [ In (fun b i -> "fly"); Out (fun b i -> "fade")]

        let test = match findThing testData with
                            | Some f -> f true 1
                            | None -> "nothing"
        test |> equal "fly"

    testCase "Option uncurrying #2116" <| fun _ ->
        let optionFn = Some (fun x y -> x + y)

        let list = List.choose id [optionFn]
        List.length list |> equal 1
        let x =
            match list with
            | [f] -> f 3 4
            | _ -> -1
        equal 7 x

    // See https://github.com/fable-compiler/Fable/issues/2436#issuecomment-919165092
    testCase "Functions passed to an object expression are uncurried" <| fun () ->
        let mutable d = 0
        let getAdder x =
            d <- d + 1
            fun y z -> x + y + z
        let _ = getAdder 4
        let f = getAdder 4
        let adder = { new IAdder with member _.Add = f }
        d |> equal 2
        adder.Add 3 4 |> equal 11

    testCase "Iterating list of functions #2047" <| fun _ ->
        let mutable s = "X"
        for someFun in [fortyTwo] do
            s <- s + someFun "y" "z" + s
        equal "Xy42zX" s

    testCase "Aliasing a function wrapping a multi-arity function in point-free style #2045" <| fun _ ->
        equal "4426" <| doesWork fortyTwo () ()
        equal "4426" <| doesNotWork fortyTwo () ()
        equal "4426" <| Wrapper().doesNotWorki fortyTwo () ()
        equal "4426" <| Wrapper.doesNotWork fortyTwo () ()
        equal 56 <| doesWork fortyTwo2 () () () 4
        equal 56 <| doesNotWork fortyTwo2 () () () 4
        equal 56 <| Wrapper().doesNotWorki2 fortyTwo2 () () () 4
        equal 56 <| Wrapper.doesNotWork2 fortyTwo2 () () () 4
]


module FSharpPlus =
    module Option =
        let apply f (x: option<'T>) : option<'U> =
            match f, x with
            | Some f, Some x -> Some (f x)
            | _              -> None

    module Array =
        let apply f x =
            let lenf, lenx = Array.length f, Array.length x
            Array.init (lenf * lenx) (fun i -> f.[i / lenx] x.[i % lenx])

    type Map =
        static member Map ((x: option<_>, f: 'T->'U), _mthd: Map) = Option.map  f x
        static member Map ((x: _ []     , f: 'T->'U), _mthd: Map) = Array.map   f x

        static member inline Invoke (mapping: 'T->'U) (source: '``Functor<'T>``) : '``Functor<'U>`` =
            let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Map : (_*_)*_ -> _) (source, mapping), mthd)
            call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)

    type Return =
        static member Return (_: option<'a>, _: Return) = fun x -> Some x : option<'a>
        static member Return (_: 'a []     , _: Return) = fun x -> [|x|]  : 'a []

        static member inline Invoke (x: 'T) : '``Applicative<'T>`` =
            let inline call (mthd: ^M, output: ^R) = ((^M or ^R) : (static member Return : _*_ -> _) output, mthd)
            call (Unchecked.defaultof<Return>, Unchecked.defaultof<'``Applicative<'T>``>) x

    type Apply =
        static member ``<*>`` (f: option<_>, x: option<'T>, _output: option<'U>, _mthd: Apply) = Option.apply f x : option<'U>
        static member ``<*>`` (f: _ []     , x: 'T []     , _output: 'U []     , _mthd: Apply) = Array.apply  f x  : 'U []

        static member inline Invoke (f: '``Applicative<'T -> 'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` =
            let inline call (mthd : ^M, input1: ^I1, input2: ^I2, output: ^R) =
                ((^M or ^I1 or ^I2 or ^R) : (static member ``<*>`` : _*_*_*_ -> _) input1, input2, output, mthd)
            call(Unchecked.defaultof<Apply>, f, x, Unchecked.defaultof<'``Applicative<'U>``>)

    let inline forInfiniteSeqs (t: seq<_>, isFailure, conversion) =
            let add x y = y :: x
            let mutable go = true
            let mutable r = Return.Invoke []
            use e = t.GetEnumerator ()
            while go && e.MoveNext () do
                if isFailure e.Current then go <- false
                r <- Apply.Invoke (Map.Invoke add r) e.Current
            Map.Invoke (List.rev >> conversion) r

    let sequence (t: seq<option<'t>>) =
        forInfiniteSeqs (t, Option.isNone, List.toSeq) : option<seq<'t>>

    let tests = [
        testCase "FSharpPlus regression" <| fun () -> // See #2471
            let expected = Some(seq [1; 2])
            sequence (seq [Some 1; Some 2]) |> equal expected
    ]

module Curry =
    let addString (value : string) (f : string -> 'T) =
        f value

    let addBool (value : bool) (f : bool -> 'T) =
        f value

    let tests =
        [
            // Test the smallest arity I managed to create
            testCase "curried function of arity 2 works" <| fun _ ->
                let res =

                    let onSubmit =
                        fun (arg1 : string)
                            (arg2 : bool) ->

                            (arg1, arg2)

                    onSubmit
                        |> addString "name"
                        |> addBool true

                equal ("name", true) res

            // Test old maximum limit for curried functions
            testCase "curried function of arity 8 works" <| fun _ ->
                let actual =
                    let onSubmit =
                        fun (arg1 : string)
                            (arg2 : bool)
                            (arg3 : string)
                            (arg4 : string)
                            (arg5 : string)
                            (arg6 : string)
                            (arg7 : string)
                            (arg8 : string) ->

                            (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)

                    onSubmit
                        |> addString "arg1"
                        |> addBool true
                        |> addString "arg3"
                        |> addString "arg4"
                        |> addString "arg5"
                        |> addString "arg6"
                        |> addString "arg7"
                        |> addString "arg8"

                let expected =
                    ("arg1", true, "arg3", "arg4", "arg5", "arg6", "arg7", "arg8")

                equal expected actual


            // Test an arity which exceeds the old limit of 8
            testCase "curried function of arity 10 works" <| fun _ ->
                let actual =
                    let onSubmit =
                        fun (arg1 : string)
                            (arg2 : bool)
                            (arg3 : string)
                            (arg4 : string)
                            (arg5 : string)
                            (arg6 : string)
                            (arg7 : string)
                            (arg8 : string)
                            (arg9 : string)
                            (arg10 : string) ->

                            (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)

                    onSubmit
                        |> addString "arg1"
                        |> addBool true
                        |> addString "arg3"
                        |> addString "arg4"
                        |> addString "arg5"
                        |> addString "arg6"
                        |> addString "arg7"
                        |> addString "arg8"
                        |> addString "arg9"
                        |> addString "arg10"

                let expected =
                    ("arg1", true, "arg3", "arg4", "arg5", "arg6", "arg7", "arg8", "arg9", "arg10")

                equal expected actual
        ]

module Uncurry =
    // Wrap everything into a compiler directives because we are using JS interop
    // to provoke the behavior we want
    #if FABLE_COMPILER
    open Fable.Core

    let private add2WithLambda (adder: int->int->int) (arg1: int) (arg2: int): int =
        adder arg1 arg2


    let add_2 =
        let add2Arguments (x: int) (y: int): int = JsInterop.importMember "./js/1foo.js"
        add2WithLambda add2Arguments

    let private add10WithLambda
        (adder: int->int->int->int->int->int->int->int->int->int->int)
        (arg1: int)
        (arg2: int)
        (arg3: int)
        (arg4: int)
        (arg5: int)
        (arg6: int)
        (arg7: int)
        (arg8: int)
        (arg9: int)
        (arg10: int)
            : int =

        adder arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10

    let add_10 =
        let add10Arguments (x1: int) (x2: int) (x3: int) (x4: int) (x5: int) (x6: int) (x7: int) (x8: int) (x9: int) (x10: int): int = JsInterop.importMember "./js/1foo.js"
        add10WithLambda add10Arguments

    let tests =
        [
            testCase "uncurry function of arity 2 works" <| fun _ ->
                let actual =
                    add_2 10 5

                equal 15 actual

            testCase "uncurry function of arity 10 works" <| fun _ ->
                let actual =
                    add_10 1 2 3 4 5 6 7 8 9 10

                equal 55 actual
        ]
    #else
    let tests = [ ]
    #endif

module MultipleInlines =
    open Aether
    open Aether.Operators

    //
    // Types with required nesting and optics to reproduce error
    //
    type private Result<'T> =
        | Ok of 'T
        | Error of string

    type private PaymentFrequencyDto =
        | Weekly = 1
        | Fortnightly = 2
        | Monthly = 3
        | Quarterly = 4
        | Annual = 5

    type private InsurancePolicyDetailsDto = {
        PaymentFrequency : PaymentFrequencyDto
        Discount : decimal option
    } with
        static member discount_ : Prism<InsurancePolicyDetailsDto, decimal> =
            (fun p -> p.Discount),
            (fun x p ->
                match p with
                | p when p.Discount.IsSome -> {p with Discount = Some x}
                | p -> p)
        static member paymentFrequency_ : Lens<InsurancePolicyDetailsDto, PaymentFrequencyDto> =
            (fun p -> p.PaymentFrequency), (fun x p -> { p with PaymentFrequency = x })

    type private AccountDetailsDto = {
        Tag : string
        InsurancePolicyData : InsurancePolicyDetailsDto option
    } with
        static member insurancePolicyData_ : Prism<AccountDetailsDto, InsurancePolicyDetailsDto> =
            (fun p -> p.InsurancePolicyData),
            (fun x p ->
                match p with
                | p when p.InsurancePolicyData.IsSome -> {p with InsurancePolicyData = Some x}
                | p -> p)


    type private AccountDto = {
        Details : AccountDetailsDto
    } with
        static member details_ : Lens<AccountDto, AccountDetailsDto> =
            (fun p -> p.Details), (fun x p -> {p with Details = x})

    type Functor = Functor with
      static member inline map (Functor, f: 'a -> 'b, x: array<'a>) = Array.map f x
      static member inline map (Functor, f: 'a -> 'b, x: List<'a>) = List.map f x

    let inline map (f: ^a -> ^b) (x: ^x) =
      ((^x or ^Functor): (static member map: ^Functor * (^a -> ^b) * ^x -> ^r) (Functor, f, x))

    type NonEmptyList<'T> = NonEmptyList of 'T * List<'T> with
      static member inline map (Functor, f: 'a -> 'b, NonEmptyList(h, t): NonEmptyList<'a>) =
        NonEmptyList (f h, map f t)

    let private mapMyList (xs: NonEmptyList<string>) : NonEmptyList<string> =
      map (fun s -> s + "_") xs

    let tests = [
        testCase "Trait call works with multiple inlined functions I" <| fun () -> // See #2809
            let account = Some { Details = { Tag = "Test"; InsurancePolicyData = None }}
            let details_ = Option.value_ >?> AccountDto.details_ >?> AccountDetailsDto.insurancePolicyData_

            let inline getP (optic : Prism<InsurancePolicyDetailsDto,'a>) = Optic.get (details_ >?> optic) account

            getP InsurancePolicyDetailsDto.discount_
            |> equal None

        testCase "Trait call works with multiple inlined functions II" <| fun () -> // See #2809
            let account = Some { Details = { Tag = "Test"; InsurancePolicyData = None }}
            let details_ = Option.value_ >?> AccountDto.details_ >?> AccountDetailsDto.insurancePolicyData_

            let inline getP (optic : Prism<InsurancePolicyDetailsDto,'a>) = account^.(details_ >?> optic)

            getP InsurancePolicyDetailsDto.discount_
            |> equal None

        testCase "Identifiers from witnesses don't get duplicated when resolving inline expressions" <| fun () -> // See #2855
            NonEmptyList("a", ["b"; "c"]) |> mapMyList |> equal (NonEmptyList("a_", ["b_"; "c_"]))
    ]

module AccessorFunctionShorthand =

    type User =
        {
            Name : string
        }

    type Student =
        {
            Name : string
            Age : int
        }

    let inline namePropertyGetter<'a when 'a:(member Name: string)> (x: 'a) = x |> _.Name

    let tests =
        [
            testCase "Accessor function shorthand works for records" <| fun () ->
                let people =
                    [
                        { Name = "John" }
                        { Name = "Jane" }
                    ]

                let names = people |> List.map _.Name
                equal names ["John"; "Jane"]

            testCase "Accessor function shorthand works for anonymous records" <| fun () ->
                let people =
                    [
                        {| Name = "John" |}
                        {| Name = "Jane" |}
                    ]

                let names = people |> List.map _.Name
                equal names ["John"; "Jane"]

            testCase "Accessor function shorthand works with STRP syntax" <| fun () ->
                let user =
                    { Name = "John" }
                let student =
                    { Name = "Jane"; Age = 20 }

                equal (namePropertyGetter user) "John"
                equal (namePropertyGetter student) "Jane"
        ]

let tests =
    testList "Applicative" (
        tests1
        @ tests2
        @ tests3
        @ tests4
        @ tests5
        @ tests6
        @ tests7
        @ CurriedApplicative.tests
        @ Curry.tests
        @ Uncurry.tests
        @ MultipleInlines.tests
        @ AccessorFunctionShorthand.tests
    )
