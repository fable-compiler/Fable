module Fable.Transforms.ErlangPrinter

open Fable.AST.Beam
open Fable.Transforms

module Output =
    let escapeErlangString (s: string) =
        s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")

    let rec printLiteral (sb: System.Text.StringBuilder) (lit: ErlLiteral) =
        match lit with
        | Integer i -> sb.Append(i.ToString()) |> ignore
        | Float f ->
            // Use full double precision (17 significant digits) to avoid lossy rounding
            let s = sprintf "%.17g" f
            // Ensure the float literal always has a decimal point for Erlang
            if s.Contains(".") || s.Contains("e") || s.Contains("E") then
                sb.Append(s) |> ignore
            else
                sb.Append(s + ".0") |> ignore
        | StringLit s -> sb.Append($"<<\"%s{escapeErlangString s}\">>") |> ignore
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

    /// Surround with parens anything that can potentially conflict with operator precedence.
    /// Negative literals need parens too to avoid Erlang's '--' list subtraction operator.
    let rec complexExprWithParens (sb: System.Text.StringBuilder) (indent: int) (expr: ErlExpr) =
        match expr with
        | Literal(Integer n) when n < 0L ->
            sb.Append("(") |> ignore
            printExpr sb indent expr
            sb.Append(")") |> ignore
        | Literal(Float f) when f < 0.0 ->
            sb.Append("(") |> ignore
            printExpr sb indent expr
            sb.Append(")") |> ignore
        | Literal _
        | Variable _
        | Call _
        | List _
        | Tuple _
        | Map _ -> printExpr sb indent expr
        | _ ->
            sb.Append("(") |> ignore
            printExpr sb indent expr
            sb.Append(")") |> ignore

    and printExpr (sb: System.Text.StringBuilder) (indent: int) (expr: ErlExpr) =
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
            | Some m -> sb.Append($"%s{m}:%s{func}(") |> ignore
            | None -> sb.Append($"%s{func}(") |> ignore

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

                sb.Append($"%s{name}(") |> ignore

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
            // Wrap multi-expression blocks in begin...end to avoid
            // comma-separated expressions being misinterpreted as
            // separate function call arguments
            let needsBeginEnd = exprs.Length > 1

            if needsBeginEnd then
                sb.Append("begin ") |> ignore

            exprs
            |> List.iteri (fun i e ->
                printExpr sb indent e

                if i < exprs.Length - 1 then
                    sb.Append(",") |> ignore
                    sb.AppendLine() |> ignore
                    writeIndent ()
            )

            if needsBeginEnd then
                sb.Append(" end") |> ignore

        | BinOp(op, left, right) ->
            complexExprWithParens sb indent left
            sb.Append($" %s{op} ") |> ignore
            complexExprWithParens sb indent right

        | UnaryOp(op, expr) ->
            sb.Append(op) |> ignore

            if System.Char.IsLetter(op.[op.Length - 1]) then
                sb.Append(" ") |> ignore

            complexExprWithParens sb indent expr

        | TryCatch(body, catchVar, catchBody, after) ->
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
            sb.AppendLine($"    _:%s{catchVar} ->") |> ignore

            catchBody
            |> List.iteri (fun i bodyExpr ->
                writeIndent ()
                sb.Append("        ") |> ignore
                printExpr sb (indent + 2) bodyExpr

                if i < catchBody.Length - 1 then
                    sb.Append(",") |> ignore

                sb.AppendLine() |> ignore
            )

            match after with
            | [] -> ()
            | afterExprs ->
                writeIndent ()
                sb.AppendLine("after") |> ignore

                afterExprs
                |> List.iteri (fun i afterExpr ->
                    writeIndent ()
                    sb.Append("    ") |> ignore
                    printExpr sb (indent + 1) afterExpr

                    if i < afterExprs.Length - 1 then
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
                result <- result.Replace($"$%d{i}", argSb.ToString())
            )

            sb.Append(result) |> ignore

    let printFunClause (sb: System.Text.StringBuilder) (name: Atom) (clause: ErlFunClause) =
        let (Atom atomName) = name
        sb.Append($"%s{atomName}(") |> ignore

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
        | ModuleAttr(Atom name) -> sb.AppendLine($"-module(%s{name}).") |> ignore

        | ExportAttr exports ->
            let exportStrs =
                exports
                |> List.map (fun (Atom name, arity) -> $"%s{name}/%d{arity}")
                |> String.concat ", "

            sb.AppendLine($"-export([%s{exportStrs}]).") |> ignore

        | CustomAttr(Atom name, value) -> sb.AppendLine($"-%s{name}(%s{value}).") |> ignore

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

        | Comment text -> sb.AppendLine($"%%%% %s{text}") |> ignore

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
