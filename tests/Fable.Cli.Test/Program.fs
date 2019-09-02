open Expecto

open RegexTest

let allTests = [ linuxTests; windowsTests]

[<EntryPoint>]
let main args =
    allTests
    |> testList "All"
    |> runTestsWithArgs defaultConfig args