namespace Fable.Plugins

#r "../../../build/fable/bin/Fable.Core.dll"

open Fable
open Fable.AST

type RandomPlugin() =
    interface IReplacePlugin with
        member x.TryReplace _com (info: Fable.ApplyInfo) =
            match info.ownerFullName with
            | "System.Random" ->
                match info.methodName with
                | ".ctor" ->
                    let o = Fable.ObjExpr ([], [], None, info.range)
                    Fable.Wrapped (o, info.returnType) |> Some
                | "Next" ->
                    let intConst x =
                        Fable.NumberConst (U2.Case1 x, Int32) |> Fable.Value
                    let min, max =
                        match info.args with
                        | [] -> intConst 0, intConst System.Int32.MaxValue
                        | [max] -> intConst 0, max
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
            