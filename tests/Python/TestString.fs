module Fable.Tests.String

open Util.Testing

[<Fact>]
let ``test sprintf works`` () =
    // Immediately applied
    sprintf "%.2f %g" 0.5468989 5.
    |> equal "0.55 5"
    // Curried
    let printer = sprintf "Hi %s, good %s!"
    let printer = printer "Alfonso"
    printer "morning" |> equal "Hi Alfonso, good morning!"
    printer "evening" |> equal "Hi Alfonso, good evening!"

[<Fact>]
let ``test sprintf works II`` () =
      let printer2 = sprintf "Hi %s, good %s%s" "Maxime"
      let printer2 = printer2 "afternoon"
      printer2 "?" |> equal "Hi Maxime, good afternoon?"

[<Fact>]
let ``test sprintf with different decimal digits works`` () =
      sprintf "Percent: %.0f%%" 5.0 |> equal "Percent: 5%"
      sprintf "Percent: %.2f%%" 5. |> equal "Percent: 5.00%"
      sprintf "Percent: %.1f%%" 5.24 |> equal "Percent: 5.2%"
      sprintf "Percent: %.2f%%" 5.268 |> equal "Percent: 5.27%"
      sprintf "Percent: %f%%" 5.67 |> equal "Percent: 5.670000%"

[<Fact>]
let ``sprintf displays sign correctly`` () =
      sprintf "%i" 1 |> equal "1"
      sprintf "%d" 1 |> equal "1"
      sprintf "%d" 1L |> equal "1"
      sprintf "%.2f" 1. |> equal "1.00"
      sprintf "%i" -1 |> equal "-1"
      sprintf "%d" -1 |> equal "-1"
      sprintf "%d" -1L |> equal "-1"
      sprintf "%.2f" -1. |> equal "-1.00"

[<Fact>]
let ``test Print.sprintf works`` () =
    let res = Printf.sprintf "%s" "abc"
    equal "res: abc" ("res: " + res)

[<Fact>]
let ``test sprintf without arguments works`` () =
    sprintf "hello" |> equal "hello"

[<Fact>]
let ``test input of print format can be retrieved`` () =
    let pathScan (pf:PrintfFormat<_,_,_,_,'t>) =
        let formatStr = pf.Value
        formatStr

    equal "/hello/%s" (pathScan "/hello/%s")

[<Fact>]
let ``test interpolate works`` () =
    let name = "Phillip"
    let age = 29
    $"Name: {name}, Age: %i{age}"
    |> equal "Name: Phillip, Age: 29"

#if FABLE_COMPILER
[<Fact>]
let ``test string interpolation works with inline expressions`` () =
    $"I think {3.0 + 0.14} is close to %.8f{3.14159265}!"
    |> equal "I think 3.14 is close to 3.14159265!"
#endif

// [<Fact>]
// let ``test string interpolation works with anonymous records`` () =
//     let person =
//         {| Name = "John"
//            Surname = "Doe"
//            Age = 32
//            Country = "The United Kingdom" |}
//     $"Hi! My name is %s{person.Name} %s{person.Surname.ToUpper()}. I'm %i{person.Age} years old and I'm from %s{person.Country}!"
//     |> equal "Hi! My name is John DOE. I'm 32 years old and I'm from The United Kingdom!"
