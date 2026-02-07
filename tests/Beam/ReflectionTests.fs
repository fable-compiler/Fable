module Fable.Tests.ReflectionTests

open Util.Testing
open Xunit

[<Fact>]
let ``test typeof int FullName`` () =
    typeof<int>.FullName |> equal "System.Int32"

[<Fact>]
let ``test typeof string FullName`` () =
    typeof<string>.FullName |> equal "System.String"

[<Fact>]
let ``test typeof float FullName`` () =
    typeof<float>.FullName |> equal "System.Double"

[<Fact>]
let ``test typeof bool FullName`` () =
    typeof<bool>.FullName |> equal "System.Boolean"

[<Fact>]
let ``test typeof int Namespace`` () =
    typeof<int>.Namespace |> equal "System"

[<Fact>]
let ``test typeof string Namespace`` () =
    typeof<string>.Namespace |> equal "System"

[<Fact>]
let ``test typeof int IsGenericType is false`` () =
    typeof<int>.IsGenericType |> equal false

[<Fact>]
let ``test typeof int list IsGenericType is true`` () =
    typeof<int list>.IsGenericType |> equal true

[<Fact>]
let ``test typeof int list FullName starts with`` () =
    typeof<int list>.FullName.StartsWith("Microsoft.FSharp.Collections.FSharpList`1")
    |> equal true

[<Fact>]
let ``test typeof int IsArray is false`` () =
    typeof<int>.IsArray |> equal false

[<Fact>]
let ``test typeof int array IsArray is true`` () =
    typeof<int array>.IsArray |> equal true
