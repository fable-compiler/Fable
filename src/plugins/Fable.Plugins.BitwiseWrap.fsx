namespace Fable.Plugins

#r "../../build/fable/bin/Fable.exe"

open Fable.AST
open Fable.AST.Fable
open Fable.FSharp2Fable

type RandomPlugin() =
    interface IReplacePlugin with
        member x.TryReplace com (info: Fable.ApplyInfo) =

            match info.ownerFullName with
            | "Microsoft.FSharp.Core.ExtraTopLevelOperators"
            | "Microsoft.FSharp.Core.Operators" ->
                let patternFor t = 
                    match t with
                    | PrimitiveType (Number kind) -> 
                        match kind with
                        | Int8 -> Some "($0 + 0x80 & 0xFF) - 0x80"
                        | UInt8 -> Some "$0 & 0xFF"
                        | Int16 -> Some "($0 + 0x8000 & 0xFFFF) - 0x8000"
                        | UInt16 -> Some "$0 & 0xFFFF"
                        | Int32 -> Some "($0 + 0x80000000 >>> 0) - 0x80000000"
                        | UInt32 -> Some "$0 >>> 0"
                        | _ -> None
                    | _ -> None

                let applyMask t args =
                    match patternFor t with
                    | Some pattern ->
                        pattern
                        |> Fable.Replacements.Util.emit info <| args
                        |> Some
                    | _ -> None

                match info.methodName with
                | "sbyte"
                | "byte"
                | "int8"
                | "uint8"
                | "int16"
                | "uint16"
                | "int"
                | "int32"
                | "uint"
                | "uint32"
                | "op_LogicalNot"
                | "op_UnaryNegation" 
                | "op_UnaryPlus" ->
                    if info.args.Length <> 1 then
                        failwithf "Unexpected arg count for '%s'" info.methodName

                    applyMask info.returnType info.args

                | "op_Addition" 
                | "op_Subtraction" 
                | "op_Multiply" 
                | "op_Division" 
                | "op_Modulus"
                | "op_LeftShift" 
                | "op_RightShift" 
                | "op_BitwiseAnd" 
                | "op_BitwiseOr" 
                | "op_ExclusiveOr" ->
                    if info.args.Length <> 2 then
                        failwithf "Unexpected arg count for %s" info.methodName

                    match Fable.Replacements.Util.applyOp com info info.args info.methodName with
                    | Some expr -> applyMask info.returnType [expr]
                    | _ -> None

                | _ -> None

            | _ -> None
            