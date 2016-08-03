namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.5.0")>]
[<assembly: AssemblyMetadataAttribute("fableCoreVersion","0.5.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.5.0"
    let [<Literal>] InformationalVersion = "0.5.0"
