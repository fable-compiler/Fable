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
                | BinaryEqualStrict -> Equals
                | x -> sprintf "%A" x |> BinaryTodo
            Binary(op, transformExpr left, transformExpr right )
        | x -> Unknown(sprintf "%A" x)
    let asSingleExprIife = function
        | [] -> NoOp
        | [h] ->
            transformExpr h
        | exprs ->
            let statements =
                Helpers.transformStatements
                    (transformExpr >> Do)
                    (transformExpr >> Return)
                    exprs
            FunctionCall(AnonymousFunc([], statements), [])
    let transformExpr = function
        | Fable.Expr.Value(value, _) -> transformValueKind value
        | Fable.Expr.Call(expr, callInfo, t, r) ->
            //Unknown(sprintf "call %A %A" expr callInfo)
            FunctionCall(transformExpr expr, List.map transformExpr callInfo.Args)
        | Fable.Expr.Import (info, t, r) ->
            let path = info.Path.Replace(".fs", ".lua") //todo - make less brittle
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
            asSingleExprIife exprs
        | Fable.Expr.Let (ident, value, body) ->
            Let(ident.Name, transformExpr body)
        | Fable.Expr.Emit(m, _, _) ->
            Macro(m.Macro, m.CallInfo.Args |> List.map transformExpr)
        | Fable.Expr.DecisionTree(expr, lst) ->
            transformExpr expr
        | Fable.Expr.DecisionTreeSuccess(i, exprs, _) ->
            asSingleExprIife exprs
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
                FunctionDeclaration(m.Name, m.Args |> List.map(fun a -> a.Name), [transformExpr m.Body |> Return])
        | x -> sprintf "%A" x |> Unknown |> Do

let transformFile com (file: Fable.File): File =
    //let comp = LuaCompiler(com) :> ILuaCompiler
    {
        Filename = "abc"
        Statements =  file.Declarations |> List.map Transforms.transformDeclarations
        ASTDebug = sprintf "%A" file.Declarations
    }