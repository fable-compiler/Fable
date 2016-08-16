module Fable.Providers.Regex

#r "../../../../build/fable/bin/Fable.AST.dll"
#load "../../../../paket-files/fable-compiler/FSharp.TypeProviders.StarterPack/src/ProvidedTypes.fs"

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open System.Text.RegularExpressions

module Internal =
    open System
    open Fable.AST

    type EmitAttribute private () =
        inherit Attribute()
        new (macro: string) = EmitAttribute()
        new (emitterType: Type, methodName: string) = EmitAttribute()

    type Emitter() =
        member __.CreateRegex(_com: Fable.ICompiler, info: Fable.ApplyInfo) =
            let isTrue = function Fable.Value(Fable.BoolConst true) -> true | _ -> false
            match info.args with
            | Fable.Value(Fable.StringConst pattern)::ignoreCase::[multiline] ->
                let flags = if isTrue ignoreCase then [RegexIgnoreCase] else []
                let flags = if isTrue multiline then RegexMultiline::flags else flags
                Fable.RegexConst(pattern, flags) |> Fable.Value
            | _ -> failwith "Wrong arguments passed to CreateRegex"

    type Helper =
        // [<Emit("new RegExp($0,'g{{$1?i:}}{{$2?m:}}')")>]
        [<Emit(typeof<Emitter>, "CreateRegex")>]
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
