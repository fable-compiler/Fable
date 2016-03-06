namespace Fable.Plugins

#r "../../build/fable/bin/Fable.exe"

open Fable.AST
open Fable.FSharp2Fable

type RandomPlugin() =
    interface IReplacePlugin with
        member x.TryReplace com (info: Fable.ApplyInfo) =
            match info.ownerFullName with
            | "System.Random" ->
                match info.methodName with
                | ".ctor" ->
                    let o = Fable.ObjExpr ([], [], info.range)
                    Fable.Wrapped (o, info.returnType) |> Some
                | "next" ->
                    let min, max =
                        match info.args with
                        | [] -> Fable.Util.makeConst 0, Fable.Util.makeConst System.Int32.MaxValue
                        | [max] -> Fable.Util.makeConst 0, max
                        | [min; max] -> min, max
                        | _ -> failwith "Unexpected arg count for Random.Next"
                    let emitExpr =
                        Fable.Emit("Math.floor(Math.random() * ($1 - $0)) + $0")
                        |> Fable.Value
                    Fable.Apply(emitExpr, [min; max], Fable.ApplyMeth, info.returnType, info.range)
                    |> Some
                    // Alternative to the previous five lines
                    // "Math.floor(Math.random() * ($1 - $0)) + $0"
                    // |> Fable.Replacements.Util.emit info <| [min; max]
                    // |> Some
                | _ -> None
            | _ -> None
            