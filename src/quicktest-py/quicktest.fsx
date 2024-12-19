#r "nuget:Fable.Python"

open Fable.Core
open Fable.Core.Testing
open Fable.Core.PyInterop
open Fable.Python.Builtins
open System
open System.Globalization

// let equal expected actual =
//     // According the console log arguments are reversed
//     Assert.AreEqual(actual, expected)

type ITypeTestEmpty1 = interface end

// type ITypeTestEmpty2 =
//     interface
//     end

type ClassTypeTestEmpty1() =
    interface ITypeTestEmpty1

// type ClassTypeTestEmpty2() =
//     interface ITypeTestEmpty2

// type ClassTypeTestEmpty1NotCatched() =
//     interface ITypeTestEmpty1

// type ClassTypeTestEmpty2NotCatched() =
//     interface ITypeTestEmpty2

// // let typeMatchSomeBoxedObject (o:obj) =
// //     match o with
// //     | :? int -> 1
// //     | :? IInterface -> 2
// //     | :? INewINterface -> 3
// //     | _ -> 100

// let typeMatchInterface (o:obj) =
//     match o with
//     | :? ClassTypeTestEmpty1 -> 1
//     | :? ClassTypeTestEmpty2 -> 2
//     | :? ITypeTestEmpty1 -> 3
//     | :? ITypeTestEmpty2 -> 4
//     | _ -> 100


// [<EntryPoint>]
// let main argv =
//     let name = Array.tryHead argv |> Option.defaultValue "Guest"
//     printfn $"Hello {name}!"

//     // Open file with builtin `open`
//     // use file = builtins.``open``(StringPath "data.txt")
//     // file.read() |> printfn "File contents: %s"

//     // equal 1 (typeMatchInterface (ClassTypeTestEmpty1()))
//     // equal 2 (typeMatchInterface (ClassTypeTestEmpty2()))
//     // equal 3 (typeMatchInterface (ClassTypeTestEmpty1() :> ITypeTestEmpty1))
//     // equal 4 (typeMatchInterface (ClassTypeTestEmpty2() :> ITypeTestEmpty2))

//     printfn "%A" (typeMatchInterface (ClassTypeTestEmpty1()))
//     printfn "%A" (typeMatchInterface (ClassTypeTestEmpty2()))
//     printfn "%A" (typeMatchInterface (ClassTypeTestEmpty1NotCatched()))
//     printfn "%A" (typeMatchInterface (ClassTypeTestEmpty2NotCatched()))

//     0
