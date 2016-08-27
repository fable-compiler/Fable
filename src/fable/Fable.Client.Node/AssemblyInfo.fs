namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.5.9")>]
[<assembly: AssemblyMetadataAttribute("fableCoreVersion","0.5.5")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.5.9"
    let [<Literal>] InformationalVersion = "0.5.9"
