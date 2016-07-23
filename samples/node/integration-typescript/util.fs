module Util

let reverse (s: string) =
    s
    |> Seq.rev
    |> Seq.map string
    |> String.concat ""

let greet s =
    printfn "Hello %s!" s

let sum (list: int list) =
    List.fold (fun acc i -> acc + i) 0 list



// Alias
type ProductCode = string

// Record
type Complex = {real: float; imaginary: float}

// Discriminated Unions
type Angle = 
    | Deg of float
    | Rad of float

// Enum
type Gender = 
    | Male = 10
    | Female = 11

// Interface
type IPrintable =
   abstract member Print : unit -> unit

// Class
type Product (code:ProductCode, price:float) = 
    let isFree = price = Product.FreePrice 
    new (code) = Product(code, 0.0)
    member this.Code = code 
    member this.IsFree = isFree
    static member FreePrice = 0.0

    interface IPrintable with 
        member this.Print() =
            printfn "%s" this.Code 

type Part (code:ProductCode, price:float, serial:string) =
    inherit Product(code, price)
    member this.Serial = serial 

// constants
let PI = 3.1415
let tuple1 = (1, "one")
let complex1 = {real = 1.0; imaginary = 2.0}
let rightAngle = Deg 90.0 

let addComplex (x: Complex) (y: Complex): Complex =
    {real = x.real + y.real; imaginary = x.imaginary + y.imaginary}

// initialization / static constructor
do printfn "module initialized" 
