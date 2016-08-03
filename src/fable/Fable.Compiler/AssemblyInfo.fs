namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.5.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.5.1"
    let [<Literal>] InformationalVersion = "0.5.1"
