module Fable.Transforms.PhpPrinter

open System
open Fable.AST.Php

module Output =
    type Writer =
        {
            Writer: System.Text.StringBuilder
            Indent: int
            Precedence: int
            UsedTypes: PhpType Set
            CurrentNamespace: string option
        }

    let indent ctx = { ctx with Indent = ctx.Indent + 1 }

    module Writer =
        let create w =
            {
                Writer = w
                Indent = 0
                Precedence = Int32.MaxValue
                UsedTypes = Set.empty
                CurrentNamespace = None
            }

    let writeIndent ctx =
        for _ in 1 .. ctx.Indent do
            ctx.Writer.Append("    ") |> ignore

    let write ctx txt =
        ctx.Writer.Append(txt: string) |> ignore

    let writeln ctx txt =
        ctx.Writer.AppendLine(txt: string) |> ignore

    let writei ctx txt =
        writeIndent ctx
        write ctx txt

    let writeiln ctx txt =
        writeIndent ctx
        writeln ctx txt

    let writeVarList ctx vars =
        let mutable first = true

        for var in vars do
            if first then
                first <- false
            else
                write ctx ", "

            write ctx "$"
            write ctx var

    let writeUseList ctx vars =
        let mutable first = true

        for var in vars do
            if first then
                first <- false
            else
                write ctx ", "

            match var with
            | ByValue v ->
                write ctx "$"
                write ctx v
            | ByRef v ->
                write ctx "&$"
                write ctx v

    module Precedence =
        let binary =
            function
            | "*"
            | "/"
            | "%" -> 3
            | "+"
            | "-"
            | "." -> 4
            | "<<"
            | ">>"
            | ">>>" -> 5
            | "<"
            | "<="
            | ">="
            | ">" -> 7
            | "=="
            | "!="
            | "==="
            | "!=="
            | "<>"
            | "<=>" -> 7
            | "&" -> 8
            | "^" -> 9
            | "|" -> 10
            | "&&" -> 11
            | "||" -> 12
            | "??" -> 13
            | op -> failwithf "Unknown binary operator %s" op


        let unary =
            function
            | "!" -> 2
            | "-" -> 4
            | "~~~"
            | "&" -> 8
            | "(void)" -> 10
            | op -> failwithf "Unknown unary operator %s" op

        let _new = 0
        let instanceOf = 1
        let ternary = 14
        let assign = 15


        let clear ctx =
            { ctx with Precedence = Int32.MaxValue }

    let writeIdent ctx (id: PhpIdentity) =
        match id.Namespace with
        | Some ns ->
            write ctx @"\"
            write ctx ns

            if ns <> "" then
                write ctx @"\"
        | None -> ()

        match id.Class with
        | Some cls ->
            write ctx cls
            write ctx "::"
        | None -> ()

        write ctx id.Name

    let withPrecedence ctx prec f =
        let useParens =
            prec > ctx.Precedence || (prec = 14 && ctx.Precedence = 14)

        let subCtx = { ctx with Precedence = prec }

        if useParens then
            write subCtx "("

        f subCtx

        if useParens then
            write subCtx ")"

    let rec writeTypeRef ctx ref =
        match ref with
        | InType t ->
            if not (Set.contains t ctx.UsedTypes) then
                match t.Namespace with
                | None -> write ctx @"\"
                | Some ns ->
                    if t.Namespace <> ctx.CurrentNamespace then
                        write ctx @"\"
                        write ctx ns
                        write ctx @"\"

            write ctx t.Name

        | ExType id -> writeIdent ctx id
        | ArrayRef t ->
            writeTypeRef ctx t
            write ctx "[]"

    let writeStr ctx (str: string) =
        write ctx "'"
        write ctx (str.Replace(@"\", @"\\").Replace("'", @"\'"))
        write ctx "'"


    let rec writeExpr ctx expr =
        match expr with
        | PhpBinaryOp(op, left, right) ->
            withPrecedence
                ctx
                (Precedence.binary op)
                (fun subCtx ->
                    writeExpr subCtx left
                    write subCtx " "
                    write subCtx op
                    write subCtx " "
                    writeExpr subCtx right
                )

        | PhpUnaryOp(op, expr) ->
            withPrecedence
                ctx
                (Precedence.unary op)
                (fun subCtx ->
                    write subCtx op
                    writeExpr subCtx expr
                )
        | PhpConst cst ->
            match cst with
            | PhpConstNumber n -> write ctx (string<float> n)
            | PhpConstString s -> writeStr ctx s
            | PhpConstBool true -> write ctx "true"
            | PhpConstBool false -> write ctx "false"
            | PhpConstNull -> write ctx "NULL"
        | PhpVar(v, _) ->
            write ctx "$"
            write ctx v
        | PhpGlobal v ->
            write ctx "$GLOBALS['"
            //write ctx "$"
            write ctx v
            write ctx "']"
        | PhpField(l, r, _) ->
            writeExpr ctx l
            write ctx "->"

            match r with
            | Field r -> write ctx r.Name
            | StrField r -> write ctx r
        | PhpIdent id -> writeIdent ctx id
        | PhpNew(t, args) ->
            withPrecedence
                ctx
                (Precedence._new)
                (fun subCtx ->
                    write subCtx "new "

                    writeTypeRef subCtx t
                    write subCtx "("
                    writeArgs subCtx args
                    write subCtx ")"
                )
        | PhpNewArray(args) ->
            write ctx "[ "
            let mutable first = true

            for key, value in args do
                if first then
                    first <- false
                else
                    write ctx ", "

                writeArrayIndex ctx key
                writeExpr ctx value

            write ctx " ]"
        | PhpArrayAccess(array, index) ->
            writeExpr ctx array
            write ctx "["
            writeExpr ctx index
            write ctx "]"

        | PhpFunctionCall(f, args) ->
            let anonymous =
                match f with
                | PhpAnonymousFunc _ -> true
                | _ -> false

            if anonymous then
                write ctx "("

            writeExpr ctx f

            if anonymous then
                write ctx ")"

            write ctx "("
            writeArgs ctx args
            write ctx ")"
        | PhpMethodCall(this, f, args) ->
            writeExpr ctx this

            match this with
            | PhpParent -> write ctx "::"
            | _ -> write ctx "->"

            match f with
            | PhpConst(PhpConstString f) -> write ctx f
            | _ -> writeExpr ctx f

            write ctx "("
            writeArgs ctx args
            write ctx ")"
        | PhpTernary(guard, thenExpr, elseExpr) ->
            withPrecedence
                ctx
                (Precedence.ternary)
                (fun ctx ->
                    writeExpr ctx guard
                    write ctx " ? "
                    writeExpr ctx thenExpr
                    write ctx " : "
                    writeExpr ctx elseExpr
                )
        | PhpInstanceOf(expr, t) ->
            withPrecedence
                ctx
                (Precedence.instanceOf)
                (fun ctx ->
                    writeExpr ctx expr
                    write ctx " instanceof "
                    writeTypeRef ctx t
                )
        | PhpAnonymousFunc(args, uses, body) ->
            write ctx "function ("
            writeVarList ctx args
            write ctx ")"

            match uses with
            | [] -> ()
            | _ ->
                write ctx " use ("
                writeUseList ctx uses
                write ctx ")"

            write ctx " { "
            let multiline = body.Length > 1

            let bodyCtx =
                if multiline then
                    writeln ctx ""
                    indent ctx
                else
                    ctx

            for st in body do
                writeStatement bodyCtx st

            if multiline then
                writei ctx "}"
            else
                write ctx " }"
        | PhpMacro(macro, args) ->
            let regex =
                System.Text.RegularExpressions.Regex("\$(?<n>\d)(?<s>\.\.\.)?")

            let matches = regex.Matches(macro)
            let mutable pos = 0

            for m in matches do
                let n = int m.Groups.["n"].Value
                write ctx (macro.Substring(pos, m.Index - pos))

                if m.Groups.["s"].Success then
                    if n < args.Length then
                        match args.[n] with
                        | PhpNewArray items ->
                            let mutable first = true

                            for _, value in items do
                                if first then
                                    first <- false
                                else
                                    write ctx ", "

                                writeExpr ctx value


                        | _ -> writeExpr ctx args.[n]

                elif n < args.Length then
                    writeExpr ctx args.[n]

                pos <- m.Index + m.Length

            write ctx (macro.Substring(pos))
        | PhpParent -> write ctx "parent"


    and writeArgs ctx args =
        let mutable first = true

        for arg in args do
            if first then
                first <- false
            else
                write ctx ", "

            writeExpr ctx arg

    and writeArrayIndex ctx index =
        match index with
        | PhpArrayString s ->
            write ctx "'"
            write ctx s
            write ctx "' => "
        | PhpArrayInt n ->
            write ctx (string<int> n)
            write ctx " => "
        | PhpArrayNoIndex -> ()


    and writeStatement ctx st =
        match st with
        | PhpStatement.PhpReturn expr ->
            writei ctx "return "
            writeExpr (Precedence.clear ctx) expr
            writeln ctx ";"
        | PhpExpr expr ->
            writei ctx ""
            writeExpr (Precedence.clear ctx) expr
            writeln ctx ";"
        | PhpAssign(name, expr) ->
            writei ctx ""
            writeExpr (Precedence.clear ctx) name
            write ctx " = "
            writeExpr (Precedence.clear ctx) expr
            writeln ctx ";"
        | PhpSwitch(expr, cases) ->
            writei ctx "switch ("
            writeExpr (Precedence.clear ctx) expr
            writeln ctx ")"
            writeiln ctx "{"
            let casesCtx = indent ctx
            let caseCtx = indent casesCtx

            for case, sts in cases do
                match case with
                | IntCase i ->
                    writei casesCtx "case "
                    write casesCtx (string<int> i)
                | StringCase s ->
                    writei casesCtx "case '"
                    write casesCtx s
                    write casesCtx "'"
                | DefaultCase -> writei casesCtx "default"

                writeln casesCtx ":"

                for st in sts do
                    writeStatement caseCtx st

            writeiln ctx "}"
        | PhpBreak level ->
            writei ctx "break"

            match level with
            | Some l ->
                write ctx " "
                write ctx (string<int option> level)
            | None -> ()

            writeln ctx ";"

        | PhpIf(guard, thenCase, elseCase) ->
            writei ctx "if ("
            writeExpr (Precedence.clear ctx) guard
            writeln ctx ") {"
            let body = indent ctx

            for st in thenCase do
                writeStatement body st

            writei ctx "}"

            if List.isEmpty elseCase then
                writeiln ctx ""
            else
                writeln ctx " else {"

                for st in elseCase do
                    writeStatement body st

                writeiln ctx "}"
        | PhpThrow(expr) ->
            writei ctx "throw "
            writeExpr ctx expr
            writeln ctx ";"
        | PhpStatement.PhpDo(PhpConst PhpConstNull) -> ()
        | PhpStatement.PhpDo(expr) ->
            writei ctx ""
            writeExpr (Precedence.clear ctx) expr
            writeln ctx ";"
        | PhpStatement.PhpTryCatch(body, catch, finallizer) ->
            writeiln ctx "try {"
            let bodyind = indent ctx

            for st in body do
                writeStatement bodyind st

            writeiln ctx "}"

            match catch with
            | Some(var, sts) ->
                writeiln ctx "catch (exception $"
                write ctx var
                writeln ctx ") {"

                for st in sts do
                    writeStatement bodyind st

                writeiln ctx "}"
            | None -> ()

            match finallizer with
            | [] -> ()
            | _ ->
                writeiln ctx "finally {"

                for st in finallizer do
                    writeStatement bodyind st

                writeiln ctx "}"
        | PhpStatement.PhpWhileLoop(guard, body) ->
            writei ctx "while ("
            writeExpr ctx guard
            writeln ctx ") {"
            let bodyctx = indent ctx

            for st in body do
                writeStatement bodyctx st

            writeiln ctx "}"
        | PhpStatement.PhpFor(ident, start, limit, isUp, body) ->
            writei ctx "for ($"
            write ctx ident
            write ctx " = "
            writeExpr ctx start
            write ctx "; $"
            write ctx ident
            write ctx " <= "
            writeExpr ctx limit
            write ctx "; $"
            write ctx ident

            if isUp then
                write ctx "++"
            else
                write ctx "--"

            writeln ctx ") {"
            let bodyctx = indent ctx

            for st in body do
                writeStatement bodyctx st

            writeiln ctx "}"


    let writeFunc ctx (f: PhpFun) =
        writei ctx ""

        if f.Static then
            write ctx "static "

        write ctx "function "
        write ctx f.Name
        write ctx "("
        let mutable first = true

        for arg in f.Args do
            if first then
                first <- false
            else
                write ctx ", "

            write ctx "$"
            write ctx arg

        writeln ctx ") {"
        let bodyCtx = indent ctx

        for s in f.Matchings do
            writeStatement bodyCtx s

        for s in f.Body do
            writeStatement bodyCtx s

        writeiln ctx "}"

    let writeField ctx (m: PhpField) =
        writei ctx "public $"
        write ctx m.Name
        writeln ctx ";"

    let writeCtor ctx (ctor: PhpConstructor) =

        writei ctx "function __construct("
        let mutable first = true

        for a in ctor.Args do
            if first then
                first <- false
            else
                write ctx ", "

            write ctx "$"
            write ctx a

        writeln ctx ") {"
        let bodyctx = indent ctx

        for s in ctor.Body do
            writeStatement bodyctx s

        writeiln ctx "}"

    let writeType ctx (t: PhpType) =
        writei ctx ""

        if t.Abstract then
            write ctx "abstract "

        write ctx "class "
        write ctx t.Name

        match t.BaseType with
        | Some t ->
            write ctx " extends "
            write ctx t.Name
        | None -> ()

        if t.Interfaces <> [] then
            write ctx " implements "
            let mutable first = true

            for itf in t.Interfaces do
                if first then
                    first <- false
                else
                    write ctx ", "

                write ctx itf.Name

        writeln ctx " {"
        let mbctx = indent ctx

        for m in t.Fields do
            writeField mbctx m

        t.Constructor |> Option.iter (writeCtor mbctx)

        for m in t.Methods do
            writeFunc mbctx m

        writeiln ctx "}"


    let writeAssign ctx n expr =
        writei ctx "$GLOBALS['"
        write ctx n
        write ctx "'] = "
        writeExpr ctx expr
        writeln ctx ";"


    let writeDecl ctx d =
        match d with
        | PhpType t -> writeType ctx t
        | PhpFun t -> writeFunc ctx t
        | PhpDeclValue(n, expr) -> writeAssign ctx n expr
        | PhpAction statements ->
            for s in statements do
                writeStatement ctx s

    let writeFile ctx (file: PhpFile) =
        writeln ctx "<?php"

        file.Namespace
        |> Option.iter (fun ns ->
            write ctx "namespace "
            write ctx ns
            writeln ctx ";"
            writeln ctx ""
        )

        if not (List.isEmpty file.Require) then
            //writeln ctx "define('__ROOT__',dirname(__FILE__));"
            for v, r in file.Require do
                write ctx "require_once("

                match v with
                | Some var ->
                    write ctx var
                    write ctx "."
                | None -> ()

                writeStr ctx r
                writeln ctx ");"

            writeln ctx ""

        if not (List.isEmpty file.Uses) then
            for u in file.Uses do
                write ctx "use "

                match u.Namespace with
                | Some ns ->
                    write ctx @"\"
                    write ctx ns
                | None -> ()

                write ctx @"\"
                write ctx u.Name
                writeln ctx ";"

            writeln ctx ""

        let ctx =
            { ctx with
                UsedTypes = set file.Uses
                CurrentNamespace = file.Namespace
            }

        for i, d in file.Decls do
            writeln ctx ("#" + string<int> i)
            writeDecl ctx d
            writeln ctx ""


let isEmpty (file: PhpFile) : bool = false //TODO: determine if printer will not print anything

let run (writer: Printer.Writer) (file: PhpFile) : Async<unit> =
    async {
        let sb = System.Text.StringBuilder()
        let ctx = Output.Writer.create sb
        Output.writeFile ctx file
        do! writer.Write(sb.ToString())
    }
