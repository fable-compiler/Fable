module Fable.Transforms.CPrinter

open System
open System.IO
open Fable
open Fable.AST
open Fable.AST.C

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
        writeln ctx "/*"
        write ctx help
        writeln ctx txt
        writeln ctx "*/"

    let writeCommentedShort ctx txt =
        write ctx "/*"
        write ctx txt
        write ctx "*/"

    let writeOp ctx = function
        | Multiply -> write ctx "*"
        | Equals -> write ctx "=="
        | Unequal -> write ctx "!="
        | Less -> write ctx "<"
        | LessOrEqual -> write ctx "<="
        | Greater -> write ctx ">"
        | GreaterOrEqual -> write ctx ">="
        | Divide -> write ctx """/"""
        | Plus -> write ctx "+"
        | Minus -> write ctx "-"
        | And -> write ctx "&&"
        | Or -> write ctx "||"
        | BinaryTodo x -> writeCommented ctx "binary todo" x

    let sprintExprSimple = function
        | Ident i -> i.Name
        | _ -> ""

    let rec writeType ctx = function
        | Int ->
            write ctx "int"
        | Char ->
            write ctx "char"
        | Void -> write ctx "void"
        | Pointer t ->
            writeType ctx t
            write ctx "* "
        | Array t ->
            writeType ctx t
            write ctx " "
            write ctx "array[]"
        | CStruct name ->
            write ctx "struct "
            write ctx name
        | Rc _ ->
            write ctx "struct Rc"
            //
        | CTypeDef td -> write ctx td
        | x -> sprintf "%A" x |> write ctx

    let rec writeExpr ctx = function
        | Ident i ->
            write ctx i.Name
        | Const c ->
            match c with
            | ConstString s -> s |> sprintf "\"%s\"" |> write ctx
            | ConstInt16 n -> n |> sprintf "%i" |> write ctx
            | ConstInt32 n -> n |> sprintf "%i" |> write ctx
            | ConstBool b -> b |> sprintf "%b" |> write ctx
            | ConstNull -> write ctx "NULL"

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
        | Unary(op, expr) ->
            let op =
                match op with
                | Not -> "!"
                | RefOf -> "&"
            write ctx op
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
        | GetFieldThroughPointer(expr, fieldName) ->
            writeExpr ctx expr
            write ctx "->"
            write ctx fieldName
        | GetObjMethod(expr, fieldName) ->
            writeExpr ctx expr
            write ctx ":"
            write ctx fieldName
        | GetAtIndex(expr, idx) ->
            writeExpr ctx expr
            write ctx "["
            writeExpr ctx idx
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

        // | Ternary(guardExpr, thenExpr, elseExpr) ->

        //     write ctx "("
        //     writeExpr ctx guardExpr
        //     let ctxI = indent ctx
        //     write ctx " and "
        //     writeExpr ctxI thenExpr
        //     write ctx " or "
        //     writeExpr ctxI elseExpr
        //     write ctx ")"

        | Macro (macro, args) ->
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
        // | Function(args, body) ->
        //     write ctx "function "
        //     write ctx "("
        //     args |> Helper.separateWithCommas |> write ctx
        //     write ctx ")"
        //     let ctxI = indent ctx
        //     writeln ctxI ""
        //     body |> List.iter (writeStatement ctxI)
        //     writei ctx "end"

        // | NewStructInst(args) ->
        //     write ctx "{"
        //     let ctxI = indent ctx
        //     writeln ctxI ""
        //     for idx, (name, expr) in args |> List.mapi (fun i x -> i, x) do
        //         writei ctxI name
        //         write ctxI " = "
        //         writeExpr ctxI expr
        //         if idx < args.Length - 1 then
        //             writeln ctxI ","
        //     writeln ctx ""
        //     writei ctx "}"

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
        | Brackets expr ->
            write ctx "("
            writeExpr ctx expr
            write ctx ")"
        | Cast (t, expr) ->
            write ctx "("
            writeType ctx t
            write ctx ")"
            write ctx "("
            writeExpr ctx expr
            write ctx ")"
        | Unknown x ->
            writeCommented ctx "unknown" x
        | Comment c ->
            writeCommentedShort ctx c

    and writeExprs ctx = function

        | [] -> ()

        | h::t ->

            writeExpr ctx h

            for item in t do

                write ctx ", "

                writeExpr ctx item




    and writeStatement ctx = function
        | DeclareIdent(name, assignType) ->
            writei ctx ""
            writeType ctx assignType
            write ctx " "
            write ctx name
            writeln ctx ";"
        | Assignment(names, expr, assignType) ->
            let names = names |> Helper.separateWithCommas
            writei ctx ""
            writeType ctx assignType
            write ctx " "
            write ctx names
            write ctx " = "
            writeExpr ctx expr
            writeln ctx ";"
        | Return (expr, _) ->
            writei ctx "return "
            writeExpr ctx expr
            writeln ctx ";"

        | Do expr ->
            writei ctx ""
            writeExpr ctx expr
            writeln ctx ";"
        | ForLoop (name, start, limit, body) ->
            writei ctx "for "
            write ctx name
            write ctx "="
            writeExpr ctx start
            write ctx ", "
            writeExpr ctx limit
            write ctx " do"
            let ctxI = indent ctx
            writeln ctxI ""
            for statement in body do
                writeStatement ctxI statement
            writeln ctx ""
            writei ctx "end"
            writeln ctx ";"

        | WhileLoop (guard, body) ->
            writei ctx "while "
            writeExpr ctx guard
            write ctx " do"
            let ctxI = indent ctx
            writeln ctxI ""
            for statement in body do
                writeStatement ctxI statement
            writeln ctx ""
            writei ctx "end"
            writeln ctx ";"

        | IfThenElse(guard, thenSt, elseSt) ->
            writei ctx "if ("
            writeExpr ctx guard
            write ctx ") {"
            let ctxI = indent ctx
            writeln ctxI ""
            for statement in thenSt do
                writeStatement ctxI statement
            writei ctx "}"
            writeln ctx ""
            writei ctx "else {"
            writeln ctxI ""
            for statement in elseSt do
                writeStatement ctxI statement
            writei ctx "}"
            writeln ctx ""

        | SNoOp -> ()

    let rec writeHeaderDeclaration ctx declaration =
        match declaration with
        | FunctionDeclaration(name, args, body, returnType) ->
            writei ctx ""
            writeType ctx returnType
            write ctx " "
            write ctx name
            write ctx "("
            // let args = if exportToMod then "self"::args else args
            let mutable first = true
            for (arg, t) in args do
                if not first then
                    write ctx ", "
                first <- false
                writeType ctx t
                write ctx " "
                write ctx arg
            // args |> Helper.separateWithCommas |> write ctx
            write ctx ");"
            writeln ctx ""
        | StructDeclaration(name, fields) ->
            writei ctx ""
            write ctx "struct "
            write ctx name
            write ctx " {"
            let ctxI = indent ctx
            writeln ctxI ""
            for (name, t) in fields do
                writei ctxI ""
                writeType ctxI t
                write ctxI " "
                write ctxI name
                writeln ctxI ";"
            writeln ctx "};"
        | TypedefFnDeclaration(name, args, returnArg) ->
            write ctx "typedef "
            writeType ctx returnArg
            write ctx " "
            let mutable first = true
            // write ctx ("(*" + name + ")")
            write ctx name
            write ctx " ("
            for (name, t) in args do
                if not first then
                    write ctx ", "
                first <- false
                writeType ctx t
                write ctx " "
                write ctx name
            writeln ctx ");"
        | NothingDeclared _ -> ()

    let rec writeDeclaration ctx declaration =
        match declaration with
        | FunctionDeclaration(name, args, body, returnType) ->
            writei ctx ""
            writeType ctx returnType
            write ctx " "
            write ctx name
            write ctx "("
            // let args = if exportToMod then "self"::args else args
            let mutable first = true
            for (arg, t) in args do
                if not first then
                    write ctx ", "
                first <- false
                writeType ctx t
                write ctx " "
                write ctx arg
            // args |> Helper.separateWithCommas |> write ctx
            write ctx ") {"
            let ctxI = indent ctx
            writeln ctxI ""
            body |> List.iter (writeStatement ctxI)
            writeln ctx "}"
        | StructDeclaration(name, fields) ->
            writei ctx ""
            write ctx "struct "
            write ctx name
            write ctx " {"
            let ctxI = indent ctx
            writeln ctxI ""
            for (name, t) in fields do
                writei ctxI ""
                writeType ctxI t
                write ctxI " "
                write ctxI name
                writeln ctxI ";"
            writeln ctx "};"
        | TypedefFnDeclaration(name, args, returnArg) ->
            write ctx "typedef "
            writeType ctx returnArg
            write ctx " "
            let mutable first = true
            // write ctx ("(*" + name + ")")
            write ctx name
            write ctx " ("
            for (name, t) in args do
                if not first then
                    write ctx ", "
                first <- false
                writeType ctx t
                write ctx " "
                write ctx name
            writeln ctx ");"
        | NothingDeclared _ -> ()

    let writeHeaderFile ctx (file: File) =
        file.Filename.Replace(".","_").Replace("/", "_").Replace(":", "_") |> sprintf "#ifndef %s"  |> writeln ctx
        file.Filename.Replace(".","_").Replace("/", "_").Replace(":", "_") |> sprintf "#define %s"  |> writeln ctx
        for fInclude in file.Includes do
            if fInclude.IsBuiltIn then
                sprintf "#include <%s>" fInclude.Name |> writei ctx
                writeln ctx ""
            else
                sprintf "#include \"%s\"" (fInclude.Name.Replace(".c", ".h")) |> writei ctx
                writeln ctx ""
        for s in file.Declarations do
            writeHeaderDeclaration ctx s
        writeln ctx ""
        writeln ctx "#endif"

    let writeFile ctx (file: File) =
        // writeln ctx "#include <stdio.h>"
        // writeln ctx "#include <assert.h>"
        // writeln ctx "#include \"../../fable-lib/rc.c\"" // todo imports should handle this
        //todo write includes
        file.Filename.Replace(".","_").Replace("/", "_").Replace(":", "_") |> sprintf "#ifndef %s"  |> writeln ctx
        file.Filename.Replace(".","_").Replace("/", "_").Replace(":", "_") |> sprintf "#define %s"  |> writeln ctx

        let useHFiles = false
        for fInclude in file.Includes do
            if fInclude.IsBuiltIn then
                sprintf "#include <%s>" fInclude.Name |> writei ctx
                writeln ctx ""
            else
                if useHFiles then
                    sprintf "#include \"%s\"" (fInclude.Name.Replace(".c", ".h")) |> writei ctx
                else
                    sprintf "#include \"%s\"" fInclude.Name |> writei ctx
                writeln ctx ""
        for s in file.Declarations do
            writeDeclaration ctx s
        writeln ctx ""

        writeln ctx "#endif"

        // writeln ctx "--[["

        // sprintf "%s" file.ASTDebug |> write ctx

        //sprintf "%A" file.Statements |> write ctx

        //writeln ctx " --]]"