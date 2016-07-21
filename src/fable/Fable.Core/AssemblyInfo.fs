namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.2.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.2.2"
    let [<Literal>] InformationalVersion = "0.2.2"
