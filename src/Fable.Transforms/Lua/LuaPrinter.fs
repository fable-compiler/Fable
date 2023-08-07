// fsharplint:disable InterfaceNames
module Fable.Transforms.LuaPrinter

open System
open System.IO
open Fable
open Fable.AST
open Fable.AST.Lua

type System.Text.StringBuilder with
    member sb.Write (txt: string) =
        sb.Append(txt) |> ignore
    member sb.WriteLine (txt: string) =
        sb.Append(txt) |> ignore
        sb.AppendLine() |> ignore

module Output =

    type Writer =
        { Writer: System.Text.StringBuilder
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
        | Unequal -> write ctx "~="
        | Less -> write ctx "<"
        | LessOrEqual -> write ctx "<="
        | Greater -> write ctx ">"
        | GreaterOrEqual -> write ctx ">="
        | Divide -> write ctx """/"""
        | Plus -> write ctx "+"
        | Minus -> write ctx "-"
        | And -> write ctx "and"
        | Or -> write ctx "or"
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
            | ConstInteger n -> n |> sprintf "%i" |> write ctx
            | ConstBool b -> b |> sprintf "%b" |> write ctx
            | ConstNull -> write ctx "nil"
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
        | Unary(Not, expr) ->
            write ctx "not "
            writeExpr ctx expr
        | Binary (op, left, right) ->
            writeExpr ctx left
            write ctx " "
            writeOp ctx op
            write ctx " "
            writeExpr ctx right
        | GetField(expr, fieldName) ->
            writeExpr ctx expr
            write ctx "."
            write ctx fieldName
        | GetObjMethod(expr, fieldName) ->
            writeExpr ctx expr
            write ctx ":"
            write ctx fieldName
        | GetAtIndex(expr, idx) ->
            writeExpr ctx expr
            write ctx "["
            //hack alert - lua indexers are 1-based and not 0-based, so we need to "add1". Probably correct soln here is to simplify ast after +1 if possible
            let add1 = Binary(BinaryOp.Plus, Const (ConstNumber 1.0), idx)
            writeExpr ctx add1
            write ctx "]"
        | SetValue(expr, value) ->
            writeExpr ctx expr
            write ctx " = "
            writeExpr ctx value
        | SetExpr(expr, a, value) ->
            writeExpr ctx expr
            write ctx " = "
            // writeExpr ctx a
            // write ctx " "
            writeExpr ctx value
        | Ternary(guardExpr, thenExpr, elseExpr) ->
            //let ctxA = indent ctx
            write ctx "("
            writeExpr ctx guardExpr
            //writeln ctx ""
            let ctxI = indent ctx
            write ctx " and "
            //writei ctx "and "
            writeExpr ctxI thenExpr
            //writeln ctx ""
            write ctx " or "
            //writei ctx "or "
            writeExpr ctxI elseExpr
            write ctx ")"
        | Macro (macro, args) ->

            // let subbedMacro =
            //     (s, args |> List.mapi(fun i x -> i.ToString(), sprintExprSimple x))
            //     ||> List.fold (fun acc (i, arg) -> acc.Replace("$"+i, arg) )
            // writei ctx subbedMacro
            let regex = System.Text.RegularExpressions.Regex("\$(?<n>\d)(?<s>\.\.\.)?")
            let matches = regex.Matches(macro)
            let mutable pos = 0
            for m in matches do
                let n = int m.Groups.["n"].Value
                write ctx (macro.Substring(pos,m.Index-pos))
                if m.Groups.["s"].Success then
                    if n < args.Length then
                        match args.[n] with
                        | NewArr items ->
                           let mutable first = true
                           for value in items do
                               if first then
                                   first <- false
                               else
                                   write ctx ", "
                               writeExpr ctx value
                        | _ ->
                            writeExpr ctx args.[n]

                elif n < args.Length then
                    writeExpr ctx args.[n]

                pos <- m.Index + m.Length
            write ctx (macro.Substring(pos))
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
        | NewArr(args) ->
            write ctx "{"
            let ctxI = indent ctx
            writeln ctxI ""
            for idx, expr in args |> List.mapi (fun i x -> i, x) do
                writei ctxI ""
                writeExpr ctxI expr
                if idx < args.Length - 1 then
                    writeln ctxI ","
            //writeExprs ctxI args
            writeln ctx ""
            writei ctx "}"
        | NoOp -> ()
        | Parentheses expr ->
            write ctx "("
            writeExpr ctx expr
            write ctx ")"
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
        | Assignment(names, expr, isLocal) ->
            let names = names |> Helper.separateWithCommas
            writei ctx ""
            if isLocal then write ctx "local "
            write ctx names
            write ctx " = "
            writeExpr ctx expr
            writeln ctx ""
        | FunctionDeclaration(name, args, body, exportToMod) ->
            writei ctx "function "
            write ctx name
            write ctx "("
            // let args = if exportToMod then "self"::args else args
            args |> Helper.separateWithCommas |> write ctx
            write ctx ")"
            let ctxI = indent ctx
            writeln ctxI ""
            body |> List.iter (writeStatement ctxI)
            writeln ctx "end"
            if exportToMod then
                writei ctx "mod."
                write ctx name
                write ctx " = function(self, ...) "
                write ctx name
                write ctx "(...)"
                write ctx " end"
                writeln ctxI ""
        | Return expr ->
            writei ctx "return "
            writeExpr ctx expr
            writeln ctx ""
        | Do expr ->
            writei ctx ""
            writeExpr ctx expr
            writeln ctx ""
        | ForLoop (name, start, limit, body) ->
            writei ctx "for "
            write ctx name
            write ctx "="
            writeExpr ctx start
            write ctx ", "
            writeExpr ctx limit
            write ctx " do"
            let ctxI = indent ctx
            for statement in body do
                writeln ctxI ""
                writeStatement ctxI statement
            writeln ctx ""
            writei ctx "end"
            writeln ctx ""
        | WhileLoop (guard, body) ->
            writei ctx "while "
            writeExpr ctx guard
            write ctx " do"
            let ctxI = indent ctx
            for statement in body do
                writeln ctxI ""
                writeStatement ctxI statement
            writeln ctx ""
            writei ctx "end"
            writeln ctx ""
        | IfThenElse(guard, thenSt, elseSt) ->
            writei ctx "if "
            writeExpr ctx guard
            write ctx " then"
            let ctxI = indent ctx
            for statement in thenSt do
                writeln ctxI ""
                writeStatement ctxI statement
            writeln ctx ""
            writei ctx "else"
            for statement in elseSt do
                writeln ctxI ""
                writeStatement ctxI statement
            writeln ctx ""
            writei ctx "end"
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
        //sprintf "%A" file.Statements |> write ctx
        //writeln ctx " --]]"

let isEmpty (file: File): bool =
    false //TODO: determine if printer will not print anything

let run (writer: Printer.Writer) (lib: File): Async<unit> =
    async {
        let sb = System.Text.StringBuilder()
        let ctx = Output.Writer.create sb
        Output.writeFile ctx lib
        do! writer.Write(sb.ToString())
    }