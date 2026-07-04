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
let ``test combine works`` () =
    let path = System.IO.Path.Combine("temp", "test.txt")
    path
    |> equal "temp/test.txt"

[<Fact>]
let ``test combine with three parts works`` () =
    let path = System.IO.Path.Combine("temp", "sub", "test.txt")
    path
    |> equal "temp/sub/test.txt"

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

[<Fact>]
let ``test directory exists works`` () =
    System.IO.Directory.Exists(System.IO.Path.GetTempPath())
    |> equal true

[<Fact>]
let ``test directory exists returns false for non-existing directory`` () =
    let dir =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.IO.Path.GetRandomFileName())

    System.IO.Directory.Exists(dir)
    |> equal false

[<Fact>]
let ``test directory exists returns false for a file path`` () =
    let file = System.IO.Path.GetTempFileName()
    System.IO.Directory.Exists(file)
    |> equal false
    System.IO.File.Delete(file)

[<Fact>]
let ``test create directory creates missing parent directories`` () =
    let baseDir =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.IO.Path.GetRandomFileName())

    let nested = System.IO.Path.Combine(baseDir, "a", "b", "c")

    System.IO.Directory.Exists(nested)
    |> equal false

    System.IO.Directory.CreateDirectory(nested)
    |> ignore

    System.IO.Directory.Exists(nested)
    |> equal true
    System.IO.Directory.Exists(baseDir)
    |> equal true

[<Fact>]
let ``test create directory does not throw when directory already exists`` () =
    let dir =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.IO.Path.GetRandomFileName())

    System.IO.Directory.CreateDirectory(dir)
    |> ignore

    System.IO.Directory.CreateDirectory(dir)
    |> ignore

    System.IO.Directory.Exists(dir)
    |> equal true
