namespace Fable.Plugins

#if !DOTNETCORE
#r "../../../build/fable/bin/Fable.Core.dll"
#r "../../../build/fable/bin/Fable.Compiler.dll"
#endif

open Fable
open Fable.AST
open Fable.AST.Fable

type BitwiseWrapPlugin() =
    interface IReplacePlugin with
        member x.TryReplace com (info: Fable.ApplyInfo) =

            match info.ownerFullName with
            | "Microsoft.FSharp.Core.ExtraTopLevelOperators"
            | "Microsoft.FSharp.Core.Operators" ->
                let patternFor t = 
                    match t with
                    | Number kind -> 
                        match kind with
                        | Int8 -> Some "($0 + 0x80 & 0xFF) - 0x80"
                        | UInt8 -> Some "$0 & 0xFF"
                        | Int16 -> Some "($0 + 0x8000 & 0xFFFF) - 0x8000"
                        | UInt16 -> Some "$0 & 0xFFFF"
                        | Int32 -> Some "($0 + 0x80000000 >>> 0) - 0x80000000"
                        | UInt32 -> Some "$0 >>> 0"
                        | Int64 -> Some "Math.trunc($0)" // only 53-bit (still better than nothing)
                        | UInt64 -> Some "($0 > 0) ? Math.trunc($0) : ($0 >>> 0)" // 53-bit positive, 32-bit negative
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
                | "ToSByte"
                | "ToByte"
                | "ToInt8"
                | "ToUInt8"
                | "ToInt16"
                | "ToUInt16"
                | "ToInt"
                | "ToUInt"
                | "ToInt32"
                | "ToUInt32"
                | "ToInt64"
                | "ToUInt64"
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
                | "op_LogicalNot"
                | "op_BitwiseAnd" 
                | "op_BitwiseOr" 
                | "op_ExclusiveOr" ->
                    let argCount = if info.methodName = "op_LogicalNot" then 1 else 2
                    if info.args.Length <> argCount then
                        failwithf "Unexpected arg count for %s" info.methodName

                    Fable.Replacements.Util.applyOp com info info.args info.methodName
                    |> List.singleton |> applyMask info.returnType

                | _ -> None

            | _ -> None
            