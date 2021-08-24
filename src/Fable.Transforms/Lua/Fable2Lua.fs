module rec Fable.Transforms.Fable2Lua

//cloned from FableToBabel

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.AST.Lua
open Fable.Naming
open Fable.Core

// type ILuaCompiler =
//     inherit Compiler

// type LuaCompiler(com: Fable.Compiler) =
//     interface ILuaCompiler // with
        //member this.AddType(entref, Type: LuaType) = this.AddType(entref, phpType)
module Transforms =
    module Helpers =
        let transformStatements transformStatements transformReturn exprs = [
                match exprs |> List.rev with
                | h::t ->
                    for x in t |> List.rev do
                        yield transformStatements x
                    yield transformReturn h
                | [] -> ()
            ]
    let transformValueKind = function
        | Fable.NumberConstant(v,_,_) ->
            Const(ConstNumber v)
        | Fable.StringConstant(s) ->
            Const(ConstString s)
        | Fable.BoolConstant(b) ->
            Const(ConstBool b)
        | Fable.UnitConstant ->
            Const(ConstNull)
        | Fable.CharConstant(c) ->
            Const(ConstString (string c))
        // | Fable.EnumConstant(e,ref) ->
        //     convertExpr com e
        | Fable.Null _ ->
            Const(ConstNull)
        | _ -> Const ConstNull
    let transformOp = function
        | Fable.OperationKind.Binary (op, left, right) ->
            let op = match op with
                | BinaryMultiply -> Multiply
                | BinaryDivide -> Divide
                | BinaryEqual -> Equals
                | BinaryPlus -> Plus
                | BinaryMinus -> Minus
                | BinaryEqualStrict -> Equals
                | x -> sprintf "%A" x |> BinaryTodo
            Binary(op, transformExpr left, transformExpr right )
        | Fable.OperationKind.Unary (op, expr) ->
            match op with
            | UnaryNotBitwise -> transformExpr expr //not sure why this is being added
            | _ -> sprintf "%A %A" op expr |> Unknown
        | x -> Unknown(sprintf "%A" x)
    let asSingleExprIife = function
        | [] -> NoOp
        | [h] ->
            h
        | exprs ->
            let statements =
                Helpers.transformStatements
                    (Do)
                    (Return)
                    exprs
            FunctionCall(AnonymousFunc([], statements), [])
    let asSingleExprIifeTr = List.map transformExpr >> asSingleExprIife
    let transformExpr = function
        | Fable.Expr.Value(value, _) -> transformValueKind value
        | Fable.Expr.Call(expr, callInfo, t, r) ->
            //Unknown(sprintf "call %A %A" expr callInfo)
            FunctionCall(transformExpr expr, List.map transformExpr callInfo.Args)
        | Fable.Expr.Import (info, t, r) ->
            let path = info.Path.Replace(".fs", "") //todo - make less brittle
            let rcall = FunctionCall(Ident { Namespace=None; Name= "require" }, [Const (ConstString path)])
            match info.Selector with
            | "" -> rcall
            | s -> Get(rcall, FieldGet s)
        | Fable.Expr.IdentExpr(i) when i.Name <> "" ->
            Ident {Namespace = None; Name = i.Name }
        | Fable.Expr.Operation (kind, _, _) ->
            transformOp kind
        | Fable.Expr.Get(expr, Fable.GetKind.FieldGet(fieldName, isMut), _, _) ->
            Get(transformExpr expr, FieldGet(fieldName))
        | Fable.Expr.Sequential exprs ->
            asSingleExprIifeTr exprs
        | Fable.Expr.Let (ident, value, body) ->
            Let(ident.Name, transformExpr body)
        | Fable.Expr.Emit(m, _, _) ->
            // let argsExprs = m.CallInfo.Args |> List.map transformExpr
            // let macroExpr = Macro(m.Macro, argsExprs)
            // let exprs =
            //     argsExprs
            //     @ [macroExpr]
            // asSingleExprIife exprs
            Macro(m.Macro, m.CallInfo.Args |> List.map transformExpr)
        | Fable.Expr.DecisionTree(expr, lst) ->
            transformExpr expr
        | Fable.Expr.DecisionTreeSuccess(i, exprs, _) ->
            asSingleExprIifeTr exprs
        | Fable.Expr.Lambda(arg, body, name) ->
            Function([arg.Name], [transformExpr body |> Return])
        | Fable.Expr.CurriedApply(applied, args, _, _) ->
            FunctionCall(transformExpr applied, args |> List.map transformExpr)
        | Fable.Expr.IfThenElse (guardExpr, thenExpr, elseExpr, _) ->
            IfThenElse(transformExpr guardExpr, transformExpr thenExpr, transformExpr elseExpr)
        | x -> Unknown (sprintf "%A" x)

    let transformDeclarations = function
        | Fable.ModuleDeclaration m ->
            Assignment("moduleDecTest", Expr.Const (ConstString "moduledectest"))
        | Fable.MemberDeclaration m ->
            if m.Args.Length = 0 then
                Assignment(m.Name, transformExpr m.Body)
            else
                // let body =
                //     match m.Body with
                //     | Fable.Expr.Emit(mChild, _, _) ->
                //         [Macro(mChild.Macro, m.Args |> List.map (fun a -> Ident { Name = a.Name; Namespace = None })) |> Return]
                //     | _ -> [transformExpr m.Body |> Return]
                // FunctionDeclaration(m.Name, m.Args |> List.map(fun a -> a.Name), body, m.Info.IsPublic)
                FunctionDeclaration(m.Name, m.Args |> List.map(fun a -> a.Name), [transformExpr m.Body |> Return], m.Info.IsPublic)
        | x -> sprintf "%A" x |> Unknown |> Do

let transformFile com (file: Fable.File): File =
    //let comp = LuaCompiler(com) :> ILuaCompiler
    {
        Filename = "abc"
        Statements =  file.Declarations |> List.map Transforms.transformDeclarations
        ASTDebug = sprintf "%A" file.Declarations
    }