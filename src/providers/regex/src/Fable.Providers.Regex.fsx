module Fable.Providers.Regex

#load "../../../../paket-files/fsprojects/FSharp.TypeProviders.StarterPack/src/ProvidedTypes.fs"

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open System.Text.RegularExpressions

module Internal =
    type EmitAttribute(_s: string) =
        inherit System.Attribute()

    type Helper =
        [<Emit("new RegExp($0,'g{{$1?i:}}{{$2?m:}}')")>]
        static member CreateRegex(pattern, ignoreCase, multiline) =
            let flags = if ignoreCase then RegexOptions.IgnoreCase else RegexOptions.None
            let flags = if multiline then flags ||| RegexOptions.Multiline else flags
            Regex(pattern, flags)
            
open Internal

[<TypeProvider>]
type RegexProvider (_config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Fable.Providers.Regex"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let newType = ProvidedTypeDefinition(asm, ns, "SafeRegex", Some typeof<obj>)
        let staticParams = [
            ProvidedStaticParameter("pattern", typeof<string>)
            ProvidedStaticParameter("ignoreCase", typeof<bool>, parameterDefaultValue=false)
            ProvidedStaticParameter("multiline", typeof<bool>, parameterDefaultValue=false)
        ]
        let methWithStaticParams =  
            let m = ProvidedMethod("Create", [], typeof<Regex>, IsStaticMethod = true)
            m.DefineStaticParameters(staticParams, (fun nm args ->
                let pattern = args.[0] :?> string
                try
                    Regex(pattern, RegexOptions.ECMAScript) |> ignore
                with
                | _ -> failwith "Cannot compile regular expression"
                let ignoreCase, multiline = args.[1] :?> bool, args.[2] :?> bool
                let m2 = ProvidedMethod(nm, [], typeof<Regex>, IsStaticMethod = true)
                m2.InvokeCode <- fun _ -> <@@ Helper.CreateRegex(pattern, ignoreCase, multiline) @@>
                newType.AddMember m2
                m2))
            m
        newType.AddMember(methWithStaticParams)
        [newType]

    do this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()
