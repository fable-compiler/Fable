module Fable.Tests.SystemIO

open System.IO
open Util.Testing

let tests =
    testList "SystemIO" [
        testList "Path" [
            testCase "Path.Combine works with two parts" <| fun () ->
                let combined = Path.Combine("foo", "bar")
                Path.GetFileName(combined) |> equal "bar"
                Path.GetDirectoryName(combined) |> equal "foo"

            testCase "Path.Combine works with three parts" <| fun () ->
                let combined = Path.Combine("foo", "bar", "baz.txt")
                Path.GetFileName(combined) |> equal "baz.txt"
                Path.GetDirectoryName(combined) |> equal (Path.Combine("foo", "bar"))

            testCase "Path.GetDirectoryName works" <| fun () ->
                Path.GetDirectoryName("temp/test.txt")
                |> equal "temp"

            testCase "Path.GetExtension works" <| fun () ->
                Path.GetExtension("temp/test.txt")
                |> equal ".txt"

            testCase "Path.GetFileName works" <| fun () ->
                Path.GetFileName("temp/test.txt")
                |> equal "test.txt"

            testCase "Path.GetFileNameWithoutExtension works" <| fun () ->
                Path.GetFileNameWithoutExtension("temp/test.txt")
                |> equal "test"

            testCase "Path.HasExtension works" <| fun () ->
                Path.HasExtension("temp/test.txt")
                |> equal true

                Path.HasExtension("temp/test")
                |> equal false

            testCase "Path.GetFullPath works" <| fun () ->
                (Path.GetFullPath("temp/test.txt")).Length > 0
                |> equal true

            testCase "Path.GetTempPath works" <| fun () ->
                (Path.GetTempPath()).Length > 0
                |> equal true

            testCase "Path.GetRandomFileName works" <| fun () ->
                (Path.GetRandomFileName()).Length > 0
                |> equal true

            testCase "Path.GetTempFileName works" <| fun () ->
                let file1 = Path.GetTempFileName()
                let file2 = Path.GetTempFileName()

                File.Exists(file1) |> equal true
                File.Exists(file2) |> equal true
                (file1 = file2) |> equal false

                File.Delete(file1)
                File.Delete(file2)
        ]

        testList "File" [
            testCase "File.Exists works" <| fun () ->
                let file = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
                File.Exists(file) |> equal false

                File.WriteAllText(file, "Hello World")
                File.Exists(file) |> equal true

                File.Delete(file)
                File.Exists(file) |> equal false

            testCase "File.WriteAllText and ReadAllText work" <| fun () ->
                let file = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
                File.WriteAllText(file, "Hello World")

                File.ReadAllText(file)
                |> equal "Hello World"

                File.Delete(file)

            testCase "File.WriteAllLines and ReadAllLines work" <| fun () ->
                let file = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
                File.WriteAllLines(file, [| "Hello"; "World" |])

                File.ReadAllLines(file)
                |> equal [| "Hello"; "World" |]

                File.Delete(file)

            testCase "File.ReadAllLines handles CRLF and a trailing newline" <| fun () ->
                let file = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
                File.WriteAllText(file, "Hello\r\nWorld\r\n")

                File.ReadAllLines(file)
                |> equal [| "Hello"; "World" |]

                File.Delete(file)

            testCase "File.WriteAllBytes and ReadAllBytes work" <| fun () ->
                let file = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
                let bytes = [| 1uy; 2uy; 3uy |]
                File.WriteAllBytes(file, bytes)

                File.ReadAllBytes(file)
                |> equal bytes

                File.Delete(file)

            testCase "File.Copy works" <| fun () ->
                let file = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
                let copy = file + ".copy"
                File.WriteAllText(file, "Hello World")
                File.Copy(file, copy)

                File.Exists(copy) |> equal true
                File.ReadAllText(copy) |> equal "Hello World"

                File.Delete(file)
                File.Delete(copy)

            testCase "File.Move works" <| fun () ->
                let file = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
                let moved = file + ".moved"
                File.WriteAllText(file, "Hello World")
                File.Move(file, moved)

                File.Exists(file) |> equal false
                File.Exists(moved) |> equal true

                File.Delete(moved)
        ]
    ]
