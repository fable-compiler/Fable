// fsharplint:disable InterfaceNames
module Fable.Transforms.LuaPrinter

open System
open System.IO
open Fable
open Fable.AST
open Fable.AST.Lua


module Output =
    type Writer =
        { Writer: TextWriter
          Indent: int
          Precedence: int
          CurrentNamespace: string option }

    module Helper =
        let separateWithCommas = function
            | [] -> ""
            | [x] -> x
            | lst -> lst |> List.reduce (fun acc item -> acc + " ," + item)

    let indent ctx =
        { ctx with Indent = ctx.Indent + 1}

    module Writer =
        let create w =
            { Writer = w; Indent = 0; Precedence = Int32.MaxValue; CurrentNamespace = None }

    let writeIndent ctx =
        for _ in 1 .. ctx.Indent do
            ctx.Writer.Write("    ")

    let write ctx txt =
        ctx.Writer.Write(txt: string)

    let writei ctx txt =
        writeIndent ctx
        write ctx txt

    let writeln ctx txt =
        ctx.Writer.WriteLine(txt: string)
    let writeCommented ctx help txt =
        writeln ctx "--[["
        write ctx help
        writeln ctx txt
        writeln ctx " --]]"
    let writeOp ctx = function
        | Multiply -> write ctx "*"
        | Equals -> write ctx "=="
        | Divide -> write ctx """/"""
        | Plus -> write ctx "+"
        | Minus -> write ctx "-"
        | BinaryTodo x -> writeCommented ctx "binary todo" x
    let sprintExprSimple = function
        | Ident i -> i.Name
        | _ -> ""
    let rec writeExpr ctx = function
        | Ident i ->
            write ctx i.Name
        | Const c ->
            match c with
            | ConstString s -> s |> sprintf "'%s'" |> write ctx
            | ConstNumber n -> n |> sprintf "%f" |> write ctx
            | ConstBool b -> b |> sprintf "%b" |> write ctx
            | ConstNull -> write ctx "null"
        | FunctionCall(e, args) ->
            writeExpr ctx e
            write ctx "("
            args |> writeExprs ctx
            write ctx ")"
        | AnonymousFunc(args, body) ->
            write ctx "(function "
            write ctx "("
            args |> Helper.separateWithCommas |> write ctx
            write ctx ")"
            writeln ctx ""
            let ctxI = indent ctx
            for b in body do
                writeStatement ctxI b
            writei ctx "end)"
        | Binary (op, left, right) ->
            writeExpr ctx left
            write ctx " "
            writeOp ctx op
            write ctx " "
            writeExpr ctx right
        | Get(expr, FieldGet(fieldName)) ->
            writeExpr ctx expr
            write ctx "."
            write ctx fieldName
        | Let(name, value, expr) ->
            write ctx name
            write ctx " = "
            writeExpr ctx value
            writeExpr ctx expr
        | Ternary(guardExpr, thenExpr, elseExpr) ->
            writeln ctx ""
            let ctxA = indent ctx
            writei ctxA ""
            writeExpr ctxA guardExpr
            writeln ctxA ""
            let ctxI = indent ctxA
            writei ctxI " and "
            writeExpr ctxI thenExpr
            writeln ctxI ""
            writei ctxI " or "
            writeExpr ctxI elseExpr
            write ctx ""
        | Macro (s, args) ->
            let subbedMacro =
                (s, args |> List.mapi(fun i x -> i.ToString(), sprintExprSimple x))
                ||> List.fold (fun acc (i, arg) -> acc.Replace("$"+i, arg) )
            writei ctx subbedMacro
        | Function(args, body) ->
            write ctx "function "
            write ctx "("
            args |> Helper.separateWithCommas |> write ctx
            write ctx ")"
            let ctxI = indent ctx
            writeln ctxI ""
            body |> List.iter (writeStatement ctxI)
            writei ctx "end"
        | NewObj(args) ->
            write ctx "{"
            let ctxI = indent ctx
            writeln ctxI ""
            for idx, (name, expr) in args |> List.mapi (fun i x -> i, x) do
                writei ctxI name
                write ctxI " = "
                writeExpr ctxI expr
                if idx < args.Length - 1 then
                    writeln ctxI ","
            //writeExprs ctxI args
            writeln ctx ""
            writei ctx "}"
        | NoOp -> ()
        | Unknown x ->
            writeCommented ctx "unknown" x
        | x -> sprintf "%A" x |> writeCommented ctx "todo"
    and writeExprs ctx = function
        | [] -> ()
        | h::t ->
            writeExpr ctx h
            for item in t do
                write ctx ", "
                writeExpr ctx item

    and writeStatement ctx = function
        | Assignment(name, expr) ->
            writei ctx name
            write ctx " = "
            writeExpr ctx expr
            writeln ctx ""
        | FunctionDeclaration(name, args, body, exportToMod) ->
            writei ctx "function "
            write ctx name
            write ctx "("
            args |> Helper.separateWithCommas |> write ctx
            write ctx ")"
            let ctxI = indent ctx
            writeln ctxI ""
            body |> List.iter (writeStatement ctxI)
            writeln ctx "end"
            if exportToMod then
                writei ctx "mod."
                write ctx name
                write ctx " = "
                write ctx name
                writeln ctxI ""
        | Return expr ->
            writei ctx "return "
            writeExpr ctx expr
            writeln ctx ""
        | Do expr ->
            writei ctx ""
            writeExpr ctx expr
            writeln ctx ""
        | SNoOp -> ()

    let writeFile ctx (file: File) =
        writeln ctx "mod = {}"
        for s in file.Statements do
            writeStatement ctx s
        write ctx "return mod"
        //debugging
        writeln ctx ""
        // writeln ctx "--[["
        // sprintf "%s" file.ASTDebug |> write ctx
        // sprintf "%A" file.Statements |> write ctx
        // writeln ctx " --]]"