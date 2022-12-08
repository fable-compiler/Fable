module Fable.Tests.TestSystemIO

open System
open Util.Testing

[<Fact>]
let ``test get random filename works`` () =
    let name = System.IO.Path.GetRandomFileName()
    name.Length
    |> equal 12

[<Fact>]
let ``test get temp path works`` () =
    let path = System.IO.Path.GetTempPath()
    path.Length > 0
    |> equal true

[<Fact>]
let ``test get temp file name works`` () =
    let name = System.IO.Path.GetTempFileName()
    name.Length > 0
    |> equal true

[<Fact>]
let ``test get file name works`` () =
    let name = System.IO.Path.GetFileName("temp/test.txt")
    name
    |> equal "test.txt"

[<Fact>]
let ``test get file name without extesion works`` () =
    let name = System.IO.Path.GetFileNameWithoutExtension("temp/test.txt")
    name
    |> equal "test"

[<Fact>]
let ``test get directory name works`` () =
    let name = System.IO.Path.GetDirectoryName("temp/test.txt")
    name
    |> equal "temp"

[<Fact>]
let ``test has extension works`` () =
    let hasExt = System.IO.Path.HasExtension("temp/test.txt")
    hasExt
    |> equal true
    let hasExt = System.IO.Path.HasExtension("temp/test")
    hasExt
    |> equal false

[<Fact>]
let ``test get extension works`` () =
    let ext = System.IO.Path.GetExtension("temp/test.txt")
    ext
    |> equal ".txt"

[<Fact>]
let ``test get full path works`` () =
    let path = System.IO.Path.GetFullPath("temp/test.txt")
    path.Length > 0
    |> equal true

[<Fact>]
let ``test read all text works`` () =
    let file = System.IO.Path.GetTempFileName()
    System.IO.File.WriteAllText(file, "Hello World")
    let text = System.IO.File.ReadAllText(file)
    System.IO.File.Delete(file)

    text.Contains "Hello World"
    |> equal true

[<Fact>]
let ``test read all lines works`` () =
    let file = System.IO.Path.GetTempFileName()
    System.IO.File.WriteAllLines(file, [|"Hello"; "World"|])
    let lines = System.IO.File.ReadAllLines(file)
    System.IO.File.Delete(file)

    lines.Length
    |> equal 2
    lines = [|"Hello"; "World"|]
    |> equal true

[<Fact>]
let ``test file exists works`` () =
    let file = System.IO.Path.GetTempFileName()
    System.IO.File.Exists(file)
    |> equal true
    System.IO.File.Delete(file)
    System.IO.File.Exists(file)
    |> equal false
