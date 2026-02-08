module Fable.Transforms.ErlangPrinter

open Fable.AST.Beam
open Fable.Transforms

module Output =
    let escapeErlangString (s: string) =
        s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")

    let rec printLiteral (sb: System.Text.StringBuilder) (lit: ErlLiteral) =
        match lit with
        | Integer i -> sb.Append(string i) |> ignore
        | Float f ->
            // Use full double precision (17 significant digits) to avoid lossy rounding
            let s = sprintf "%.17g" f
            // Ensure the float literal always has a decimal point for Erlang
            if s.Contains(".") || s.Contains("e") || s.Contains("E") then
                sb.Append(s) |> ignore
            else
                sb.Append(s + ".0") |> ignore
        | StringLit s -> sb.Append($"<<\"{escapeErlangString s}\">>") |> ignore
        | AtomLit(Atom a) -> sb.Append(a) |> ignore
        | BoolLit true -> sb.Append("true") |> ignore
        | BoolLit false -> sb.Append("false") |> ignore
        | NilLit -> sb.Append("[]") |> ignore

    let rec printPattern (sb: System.Text.StringBuilder) (pat: ErlPattern) =
        match pat with
        | PVar name -> sb.Append(name) |> ignore
        | PLiteral lit -> printLiteral sb lit
        | PTuple pats ->
            sb.Append("{") |> ignore

            pats
            |> List.iteri (fun i p ->
                if i > 0 then
                    sb.Append(", ") |> ignore

                printPattern sb p
            )

            sb.Append("}") |> ignore
        | PList(head, tail) ->
            sb.Append("[") |> ignore
            printPattern sb head
            sb.Append(" | ") |> ignore
            printPattern sb tail
            sb.Append("]") |> ignore
        | PWildcard -> sb.Append("_") |> ignore

    let rec printExpr (sb: System.Text.StringBuilder) (indent: int) (expr: ErlExpr) =
        let writeIndent () =
            for _ in 1..indent do
                sb.Append("    ") |> ignore

        match expr with
        | Literal lit -> printLiteral sb lit

        | Variable name -> sb.Append(name) |> ignore

        | Tuple exprs ->
            sb.Append("{") |> ignore

            exprs
            |> List.iteri (fun i e ->
                if i > 0 then
                    sb.Append(", ") |> ignore

                printExpr sb indent e
            )

            sb.Append("}") |> ignore

        | List exprs ->
            sb.Append("[") |> ignore

            exprs
            |> List.iteri (fun i e ->
                if i > 0 then
                    sb.Append(", ") |> ignore

                printExpr sb indent e
            )

            sb.Append("]") |> ignore

        | ListCons(head, tail) ->
            sb.Append("[") |> ignore
            printExpr sb indent head
            sb.Append(" | ") |> ignore
            printExpr sb indent tail
            sb.Append("]") |> ignore

        | Map entries ->
            sb.Append("#{") |> ignore

            entries
            |> List.iteri (fun i (k, v) ->
                if i > 0 then
                    sb.Append(", ") |> ignore

                printExpr sb indent k
                sb.Append(" => ") |> ignore
                printExpr sb indent v
            )

            sb.Append("}") |> ignore

        | Call(module_, func, args) ->
            match module_ with
            | Some m -> sb.Append($"{m}:{func}(") |> ignore
            | None -> sb.Append($"{func}(") |> ignore

            args
            |> List.iteri (fun i a ->
                if i > 0 then
                    sb.Append(", ") |> ignore

                printExpr sb indent a
            )

            sb.Append(")") |> ignore

        | Apply(func, args) ->
            match func with
            | Variable _ -> printExpr sb indent func
            | _ ->
                sb.Append("(") |> ignore
                printExpr sb indent func
                sb.Append(")") |> ignore

            sb.Append("(") |> ignore

            args
            |> List.iteri (fun i a ->
                if i > 0 then
                    sb.Append(", ") |> ignore

                printExpr sb indent a
            )

            sb.Append(")") |> ignore

        | Fun clauses ->
            sb.Append("fun") |> ignore

            clauses
            |> List.iteri (fun i clause ->
                if i > 0 then
                    sb.Append(";") |> ignore

                sb.Append("(") |> ignore

                clause.Patterns
                |> List.iteri (fun j p ->
                    if j > 0 then
                        sb.Append(", ") |> ignore

                    printPattern sb p
                )

                sb.Append(") ->") |> ignore
                sb.AppendLine() |> ignore

                clause.Body
                |> List.iteri (fun j bodyExpr ->
                    writeIndent ()
                    sb.Append("    ") |> ignore
                    printExpr sb (indent + 1) bodyExpr

                    if j < clause.Body.Length - 1 then
                        sb.Append(",") |> ignore

                    sb.AppendLine() |> ignore
                )
            )

            sb.Append("end") |> ignore

        | NamedFun(name, clauses) ->
            sb.Append("fun ") |> ignore

            clauses
            |> List.iteri (fun i clause ->
                if i > 0 then
                    sb.Append(";") |> ignore

                sb.Append($"{name}(") |> ignore

                clause.Patterns
                |> List.iteri (fun j p ->
                    if j > 0 then
                        sb.Append(", ") |> ignore

                    printPattern sb p
                )

                sb.Append(") ->") |> ignore
                sb.AppendLine() |> ignore

                clause.Body
                |> List.iteri (fun j bodyExpr ->
                    writeIndent ()
                    sb.Append("    ") |> ignore
                    printExpr sb (indent + 1) bodyExpr

                    if j < clause.Body.Length - 1 then
                        sb.Append(",") |> ignore

                    sb.AppendLine() |> ignore
                )
            )

            sb.Append("end") |> ignore

        | Case(expr, clauses) ->
            sb.Append("case ") |> ignore
            printExpr sb indent expr
            sb.AppendLine(" of") |> ignore

            clauses
            |> List.iteri (fun i clause ->
                writeIndent ()
                sb.Append("    ") |> ignore
                printPattern sb clause.Pattern

                match clause.Guard with
                | [] -> ()
                | guards ->
                    sb.Append(" when ") |> ignore

                    guards
                    |> List.iteri (fun gi g ->
                        if gi > 0 then
                            sb.Append(", ") |> ignore

                        printExpr sb indent g
                    )

                sb.Append(" ->") |> ignore
                sb.AppendLine() |> ignore

                clause.Body
                |> List.iteri (fun j bodyExpr ->
                    writeIndent ()
                    sb.Append("        ") |> ignore
                    printExpr sb (indent + 2) bodyExpr

                    if j < clause.Body.Length - 1 then
                        sb.Append(",") |> ignore

                    sb.AppendLine() |> ignore
                )

                if i < clauses.Length - 1 then
                    writeIndent ()
                    sb.AppendLine("    ;") |> ignore
            )

            writeIndent ()
            sb.Append("end") |> ignore

        | Match(pattern, expr) ->
            printPattern sb pattern
            sb.Append(" = ") |> ignore
            printExpr sb indent expr

        | Block exprs ->
            exprs
            |> List.iteri (fun i e ->
                printExpr sb indent e

                if i < exprs.Length - 1 then
                    sb.Append(",") |> ignore
                    sb.AppendLine() |> ignore
                    writeIndent ()
            )

        | BinOp(op, left, right) ->
            let needsParens =
                function
                | BinOp _ -> true
                | _ -> false

            if needsParens left then
                sb.Append("(") |> ignore

            printExpr sb indent left

            if needsParens left then
                sb.Append(")") |> ignore

            sb.Append($" {op} ") |> ignore

            if needsParens right then
                sb.Append("(") |> ignore

            printExpr sb indent right

            if needsParens right then
                sb.Append(")") |> ignore

        | UnaryOp(op, expr) ->
            sb.Append(op) |> ignore

            if System.Char.IsLetter(op.[op.Length - 1]) then
                sb.Append(" ") |> ignore

            printExpr sb indent expr

        | TryCatch(body, catchVar, catchBody) ->
            sb.AppendLine("try") |> ignore

            body
            |> List.iteri (fun i bodyExpr ->
                writeIndent ()
                sb.Append("    ") |> ignore
                printExpr sb (indent + 1) bodyExpr

                if i < body.Length - 1 then
                    sb.Append(",") |> ignore

                sb.AppendLine() |> ignore
            )

            writeIndent ()
            sb.AppendLine($"catch") |> ignore
            writeIndent ()
            sb.AppendLine($"    _:{catchVar} ->") |> ignore

            catchBody
            |> List.iteri (fun i bodyExpr ->
                writeIndent ()
                sb.Append("        ") |> ignore
                printExpr sb (indent + 2) bodyExpr

                if i < catchBody.Length - 1 then
                    sb.Append(",") |> ignore

                sb.AppendLine() |> ignore
            )

            writeIndent ()
            sb.Append("end") |> ignore

        | Emit(template, args) ->
            // Substitute $0, $1, etc. with printed argument expressions
            let mutable result = template

            args
            |> List.iteri (fun i arg ->
                let argSb = System.Text.StringBuilder()
                printExpr argSb indent arg
                result <- result.Replace($"${i}", argSb.ToString())
            )

            sb.Append(result) |> ignore

    let printFunClause (sb: System.Text.StringBuilder) (name: Atom) (clause: ErlFunClause) =
        let (Atom atomName) = name
        sb.Append($"{atomName}(") |> ignore

        clause.Patterns
        |> List.iteri (fun i p ->
            if i > 0 then
                sb.Append(", ") |> ignore

            printPattern sb p
        )

        sb.Append(") ->") |> ignore
        sb.AppendLine() |> ignore

        clause.Body
        |> List.iteri (fun i bodyExpr ->
            sb.Append("    ") |> ignore
            printExpr sb 1 bodyExpr

            if i < clause.Body.Length - 1 then
                sb.Append(",") |> ignore

            sb.AppendLine() |> ignore
        )

    let printAttribute (sb: System.Text.StringBuilder) (attr: ErlAttribute) =
        match attr with
        | ModuleAttr(Atom name) -> sb.AppendLine($"-module({name}).") |> ignore

        | ExportAttr exports ->
            let exportStrs =
                exports
                |> List.map (fun (Atom name, arity) -> $"{name}/{arity}")
                |> String.concat ", "

            sb.AppendLine($"-export([{exportStrs}]).") |> ignore

        | CustomAttr(Atom name, value) -> sb.AppendLine($"-{name}({value}).") |> ignore

    let printForm (sb: System.Text.StringBuilder) (form: ErlForm) =
        match form with
        | Attribute attr -> printAttribute sb attr

        | Function def ->
            sb.AppendLine() |> ignore

            def.Clauses
            |> List.iteri (fun i clause ->
                if i > 0 then
                    sb.AppendLine(";") |> ignore

                printFunClause sb def.Name clause
            )

            sb.AppendLine(".") |> ignore

        | Comment text -> sb.AppendLine($"%% {text}") |> ignore

    let printModule (sb: System.Text.StringBuilder) (erlModule: ErlModule) =
        erlModule.Forms |> List.iter (printForm sb)

let isEmpty (erlModule: ErlModule) : bool =
    erlModule.Forms
    |> List.forall (fun form ->
        match form with
        | Attribute _ -> true
        | Comment _ -> true
        | Function _ -> false
    )

let run (writer: Printer.Writer) (erlModule: ErlModule) : Async<unit> =
    async {
        let sb = System.Text.StringBuilder()
        Output.printModule sb erlModule
        do! writer.Write(sb.ToString())
    }
