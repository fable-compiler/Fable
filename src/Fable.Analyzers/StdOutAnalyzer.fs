module Fable.Analyzers.StdOutAnalyzer

open System
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK

let namesToAvoid =
    set
        [
            "System.Console.Write"
            "System.Console.WriteLine"
            "System.Console.WriteAsync"
            "System.Console.WriteLineAsync"
            "Microsoft.FSharp.Core.ExtraTopLevelOperators.printf"
            "Microsoft.FSharp.Core.ExtraTopLevelOperators.printfn"
        ]

[<CliAnalyzer "StdOutAnalyzer">]
let stdOutAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            let usages = ResizeArray<range * string>()

            let walker =
                { new TypedTreeCollectorBase() with
                    override _.WalkCall
                        _
                        (m: FSharpMemberOrFunctionOrValue)
                        _
                        _
                        (args: FSharpExpr list)
                        (range: range)
                        =
                        match m.DeclaringEntity with
                        | None -> ()
                        | Some de ->
                            let name = String.Join(".", de.FullName, m.DisplayName)

                            if Set.contains name namesToAvoid then
                                usages.Add(range, m.DisplayName)
                }

            Option.iter (walkTast walker) context.TypedTree

            return
                usages
                |> Seq.map (fun (m, name) ->
                    {
                        Type = "StdOut analyzer"
                        Message =
                            $"Writing to the standard output from this location should absolutely be avoided. Replace `%s{name}` with an ILogger call."
                        Code = "FABLE_001"
                        Severity = Severity.Error
                        Range = m
                        Fixes = []
                    }
                )
                |> Seq.toList
        }
