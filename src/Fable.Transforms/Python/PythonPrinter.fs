// fsharplint:disable InterfaceNames
module Fable.Transforms.PythonPrinter

open System

open Fable
open Fable.AST
open Fable.AST.Python
open Fable.Transforms.Printer

module PrinterExtensions =
    type Printer with

        member printer.Print(stmt: Statement) =
            match stmt with
            | AsyncFunctionDef def -> printer.Print(def)
            | FunctionDef def -> printer.Print(def)
            | ImportFrom im -> printer.Print(im)
            | NonLocal st -> printer.Print(st)
            | ClassDef st -> printer.Print(st)
            | AsyncFor st -> printer.Print(st)
            | Return rtn -> printer.Print(rtn)
            | Global st -> printer.Print(st)
            | Import im -> printer.Print(im)
            | Assign st -> printer.Print(st)
            | AnnAssign st -> printer.Print(st)
            | While wh -> printer.Print(wh)
            | Raise st -> printer.Print(st)
            | Expr st -> printer.Print(st)
            | With wi -> printer.Print(wi)
            | For st -> printer.Print(st)
            | Try st -> printer.Print(st)
            | If st -> printer.Print(st)
            | Pass -> printer.Print("pass")
            | Break -> printer.Print("break")
            | Continue -> printer.Print("continue")

        member printer.Print(node: Try) =
            printer.Print("try: ", ?loc = node.Loc)
            printer.PrintBlock(node.Body)

            for handler in node.Handlers do
                printer.Print(handler)

            if node.OrElse.Length > 0 then
                printer.Print("else: ")
                printer.PrintBlock(node.OrElse)

            if node.FinalBody.Length > 0 then
                printer.Print("finally: ")
                printer.PrintBlock(node.FinalBody)

        member printer.Print(arg: Arg) =
            let (Identifier name) = arg.Arg
            printer.Print(name)

            match arg.Annotation with
            | Some ann ->
                printer.Print(": ")
                printer.Print(ann)
            | _ -> ()

        member printer.Print(kw: Keyword) =
            let (Identifier name) = kw.Arg
            printer.Print(name)
            printer.Print(" = ")
            printer.Print(kw.Value)

        member printer.Print(arguments: Arguments) =
            if not arguments.PosOnlyArgs.IsEmpty then
                printer.PrintCommaSeparatedList(arguments.PosOnlyArgs)
                printer.Print(", /")

            let args = arguments.Args |> List.map AST.Arg
            let defaults = arguments.Defaults

            for i = 0 to args.Length - 1 do
                printer.Print(args.[i])

                if i >= args.Length - defaults.Length then
                    printer.Print("=")
                    printer.Print(defaults[i - (args.Length - defaults.Length)])

                if i < args.Length - 1 then
                    printer.Print(", ")

            match arguments.Args, arguments.VarArg with
            | [], Some vararg ->
                printer.Print("*")
                printer.Print(vararg)
            | _, Some vararg ->
                printer.Print(", *")
                printer.Print(vararg)
            | _ -> ()

        member printer.Print(wi: With) =
            printer.Print("with ")
            printer.PrintCommaSeparatedList(wi.Items)
            printer.Print(":")
            printer.PrintNewLine()
            printer.PushIndentation()
            printer.PrintStatements(wi.Body)
            printer.PopIndentation()

        member printer.Print(wi: WithItem) =
            printer.Print(wi.ContextExpr)

            match wi.OptionalVars with
            | Some vars ->
                printer.Print(" as ")
                printer.Print(vars)
            | None -> ()

        member printer.Print(assign: Assign) =
            for target in assign.Targets do
                printer.Print(target)
                printer.Print(" = ")

            printer.Print(assign.Value)

        member printer.Print(assign: AnnAssign) =
            printer.Print(assign.Target)
            printer.Print(": ")
            printer.Print(assign.Annotation)

            match assign.Value with
            | Some value ->
                printer.Print(" = ")
                printer.Print(value)
            | _ -> ()

        member printer.Print(expr: Expr) = printer.Print(expr.Value)

        member printer.Print(forIn: For) =
            printer.Print("for ")
            printer.Print(forIn.Target)
            printer.Print(" in ")
            printer.Print(forIn.Iterator)
            printer.Print(":")
            printer.PrintNewLine()
            printer.PushIndentation()
            printer.PrintStatements(forIn.Body)
            printer.PopIndentation()

        member printer.Print(_asyncFor: AsyncFor) = printer.Print("(AsyncFor)")

        member printer.Print(wh: While) =
            printer.Print("while ")
            printer.Print(wh.Test)
            printer.Print(":")
            printer.PrintNewLine()
            printer.PushIndentation()
            printer.PrintStatements(wh.Body)
            printer.PopIndentation()

        member printer.Print(cd: ClassDef) =
            for deco in cd.DecoratorList do
                printer.Print("@")
                printer.Print(deco)
                printer.PrintNewLine()

            let (Identifier name) = cd.Name
            printer.Print("class ", ?loc = cd.Loc)
            printer.Print(name)

            match cd.Bases with
            | [] -> ()
            | xs ->
                printer.Print("(")
                printer.PrintCommaSeparatedList(xs)
                printer.Print(")")

            printer.Print(":")
            printer.PrintNewLine()
            printer.PushIndentation()

            match cd.Body with
            | [] -> printer.PrintStatements([ Statement.ellipsis ])
            | body -> printer.PrintStatements(body)

            printer.PopIndentation()

        member printer.Print(ifElse: If) =
            let rec printElse stmts =
                match stmts with
                | []
                | [ Pass ] -> ()
                | [ If {
                           Test = test
                           Body = body
                           Else = els
                       } ] ->
                    printer.Print("elif ")
                    printer.Print(test)
                    printer.Print(":")
                    printer.PrintBlock(body)
                    printElse els
                | xs ->
                    printer.Print("else: ")
                    printer.PrintBlock(xs)


            printer.Print("if ")
            printer.Print(ifElse.Test)
            printer.Print(":")
            printer.PrintBlock(ifElse.Body)
            printElse ifElse.Else

        member printer.Print(ri: Raise) =
            printer.Print("raise ")
            printer.Print(ri.Exception)

        member printer.Print(func: FunctionDef) =
            printer.PrintFunction(
                Some func.Name,
                func.Args,
                func.Body,
                func.Returns,
                func.DecoratorList,
                isDeclaration = true
            )

            printer.PrintNewLine()

        member printer.Print(func: AsyncFunctionDef) =
            printer.PrintFunction(
                Some func.Name,
                func.Args,
                func.Body,
                func.Returns,
                func.DecoratorList,
                isDeclaration = true,
                isAsync = true
            )

            printer.PrintNewLine()

        member printer.Print(gl: Global) =
            if not (List.isEmpty gl.Names) then
                printer.Print("global ")
                printer.PrintCommaSeparatedList(gl.Names)

        member printer.Print(nl: NonLocal) =
            if not (List.isEmpty nl.Names) then
                printer.Print("nonlocal ")
                printer.PrintCommaSeparatedList nl.Names

        member printer.Print(im: Import) =
            if not (List.isEmpty im.Names) then
                printer.Print("import ")

                if List.length im.Names > 1 then
                    printer.Print("(")

                printer.PrintCommaSeparatedList(im.Names)

                if List.length im.Names > 1 then
                    printer.Print(")")

        member printer.Print(im: ImportFrom) =
            let (Identifier path) =
                im.Module |> Option.defaultValue (Identifier ".")

            printer.Print("from ")
            printer.Print(path)
            printer.Print(" import ")

            if not (List.isEmpty im.Names) then
                if List.length im.Names > 1 then
                    printer.Print("(")

                printer.PrintCommaSeparatedList(im.Names)

                if List.length im.Names > 1 then
                    printer.Print(")")

        member printer.Print(node: Return) =
            printer.Print("return ")
            printer.PrintOptional(node.Value)

        member printer.Print(node: Attribute) =
            printer.Print(node.Value)
            printer.Print(".")
            printer.Print(node.Attr)

        member printer.Print(ne: NamedExpr) =
            printer.Print(ne.Target)
            printer.Print(" :=")
            printer.Print(ne.Value)

        member printer.Print(node: Subscript) =
            printer.Print(node.Value)
            printer.Print("[")

            match node.Slice with
            | Tuple { Elements = [] } -> printer.Print("()")
            | Tuple { Elements = elems } ->
                printer.PrintCommaSeparatedList(elems)
            | _ -> printer.Print(node.Slice)

            printer.Print("]")

        member printer.Print(node: BinOp) =
            printer.PrintOperation(node.Left, node.Operator, node.Right)

        member printer.Print(node: BoolOp) =
            for i, value in node.Values |> List.indexed do
                printer.ComplexExpressionWithParens(value)

                if i < node.Values.Length - 1 then
                    printer.Print(node.Operator)

        member printer.Print(node: Compare) =
            //printer.AddLocation(loc)
            printer.ComplexExpressionWithParens(node.Left)

            for op, comparator in List.zip node.Ops node.Comparators do
                printer.Print(op)
                printer.ComplexExpressionWithParens(comparator)

        member printer.Print(node: UnaryOp) =
            printer.AddLocation(node.Loc)
            printer.Print(node.Op)
            printer.ComplexExpressionWithParens(node.Operand)

        member printer.Print(_node: FormattedValue) =
            printer.Print("(FormattedValue)")

        member printer.Print(node: Call) =
            printer.ComplexExpressionWithParens(node.Func)
            printer.Print("(")
            printer.PrintCommaSeparatedList(node.Args)

            if not node.Keywords.IsEmpty then
                if not node.Args.IsEmpty then
                    printer.Print(", ")

                printer.PrintCommaSeparatedList(node.Keywords)

            printer.Print(")")

        member printer.Print(node: Emit) =
            let inline replace
                pattern
                (f: System.Text.RegularExpressions.Match -> string)
                input
                =
                System.Text.RegularExpressions.Regex.Replace(input, pattern, f)

            let printSegment
                (printer: Printer)
                (value: string)
                segmentStart
                segmentEnd
                =
                let segmentLength = segmentEnd - segmentStart

                if segmentLength > 0 then
                    let segment = value.Substring(segmentStart, segmentLength)
                    printer.Print(segment)

            // Macro transformations
            // https://fable.io/docs/communicate/js-from-fable.html#Emit-when-F-is-not-enough
            let value =
                node.Value
                |> replace
                    @"\$(\d+)\.\.\."
                    (fun m ->
                        let rep = ResizeArray()
                        let i = int m.Groups[1].Value

                        for j = i to node.Args.Length - 1 do
                            rep.Add("$" + string<int> j)

                        String.concat ", " rep
                    )

                |> replace
                    @"\{\{\s*\$(\d+)\s*\?(.*?):(.*?)\}\}"
                    (fun m ->
                        let i = int m.Groups[1].Value

                        match node.Args[i] with
                        | Constant(value = BoolLiteral value) when value ->
                            m.Groups[2].Value
                        | _ -> m.Groups[3].Value
                    )

                |> replace
                    @"\{\{([^\}]*\$(\d+).*?)\}\}"
                    (fun m ->
                        let i = int m.Groups[2].Value

                        match List.tryItem i node.Args with
                        | Some _ -> m.Groups[1].Value
                        | None -> ""
                    )

                // If placeholder is followed by !, emit string literals as JS: "let $0! = $1"
                |> replace
                    @"\$(\d+)!"
                    (fun m ->
                        let i = int m.Groups[1].Value

                        match List.tryItem i node.Args with
                        | Some(Constant(StringLiteral value, _)) -> value
                        | _ -> ""
                    )

            let matches =
                System.Text.RegularExpressions.Regex.Matches(value, @"\$\d+")

            if matches.Count > 0 then
                for i = 0 to matches.Count - 1 do
                    let m = matches[i]

                    let isSurroundedWithParens =
                        m.Index > 0
                        && m.Index + m.Length < value.Length
                        && value[m.Index - 1] = '('
                        && value[m.Index + m.Length] = ')'

                    let segmentStart =
                        if i > 0 then
                            matches[i - 1].Index + matches[i - 1].Length
                        else
                            0

                    printSegment printer value segmentStart m.Index

                    let argIndex = int m.Value[1..]

                    match List.tryItem argIndex node.Args with
                    | Some e when isSurroundedWithParens -> printer.Print(e)
                    | Some e -> printer.ComplexExpressionWithParens(e)
                    | None -> printer.Print("None")

                let lastMatch = matches[matches.Count - 1]

                printSegment
                    printer
                    value
                    (lastMatch.Index + lastMatch.Length)
                    value.Length
            else
                printSegment printer value 0 value.Length

        member printer.Print(node: IfExp) =
            printer.ComplexExpressionWithParens(node.Body)
            printer.Print(" if ")
            printer.ComplexExpressionWithParens(node.Test)
            printer.Print(" else ")
            printer.ComplexExpressionWithParens(node.OrElse)

        member printer.Print(node: Lambda) =
            printer.Print("lambda")

            if (List.isEmpty >> not) node.Args.Args then
                printer.Print(" ")

            printer.Print(node.Args)
            printer.Print(": ")

            printer.Print(node.Body)


        member printer.Print(node: Tuple) =
            printer.Print("(", ?loc = node.Loc)
            printer.PrintCommaSeparatedList(node.Elements)

            if node.Elements.Length = 1 then
                printer.Print(",")

            printer.Print(")")

        member printer.Print(_node: List) = printer.Print("(List)")

        member printer.Print(_node: Set) = printer.Print("(Set)")

        member printer.Print(node: Dict) =
            printer.Print("{")

            if not node.Keys.IsEmpty then
                printer.PrintNewLine()
                printer.PushIndentation()

                let nodes =
                    List.zip node.Keys node.Values
                    |> List.mapi (fun i n -> (i, n))

                for i, (key, value) in nodes do
                    printer.Print(key)
                    printer.Print(": ")
                    printer.Print(value)

                    if i < nodes.Length - 1 then
                        printer.Print(",")
                        printer.PrintNewLine()

                printer.PrintNewLine()
                printer.PopIndentation()

            printer.Print("}")

        member printer.Print(node: Name) =
            let (Identifier name) = node.Id
            printer.Print(name)

        member printer.Print(node: ExceptHandler) =
            printer.Print("except ", ?loc = node.Loc)
            printer.PrintOptional(node.Type)
            printer.PrintOptional(" as ", node.Name)
            printer.Print(":")

            match node.Body with
            | [] -> printer.PrintBlock([ Pass ])
            | _ -> printer.PrintBlock(node.Body)

        member printer.Print(node: Alias) =
            printer.Print(node.Name)

            match node.AsName with
            | Some(Identifier alias) when Identifier alias <> node.Name ->
                printer.Print(" as ")
                printer.Print(alias)
            | _ -> ()

        member printer.Print(node: Module) = printer.PrintStatements(node.Body)

        member printer.Print(node: Identifier) =
            let (Identifier id) = node
            printer.Print(id)

        member printer.Print(node: UnaryOperator) =
            let op =
                match node with
                | Invert -> "~"
                | Not -> "not "
                | UAdd -> "+"
                | USub -> "-"

            printer.Print(op)

        member printer.Print(node: ComparisonOperator) =
            let op =
                match node with
                | Eq -> " == "
                | NotEq -> " != "
                | Lt -> " < "
                | LtE -> " <= "
                | Gt -> " > "
                | GtE -> " >= "
                | Is -> " is "
                | IsNot -> " is not "
                | In -> " in "
                | NotIn -> " not in "

            printer.Print(op)

        member printer.Print(node: BoolOperator) =
            let op =
                match node with
                | And -> " and "
                | Or -> " or "

            printer.Print(op)

        member printer.Print(node: Operator) =
            let op =
                match node with
                | Add -> " + "
                | Sub -> " - "
                | Mult -> " * "
                | Div -> " / "
                | FloorDiv -> " // "
                | Mod -> " % "
                | Pow -> " ** "
                | LShift -> " << "
                | RShift -> " >> "
                | BitOr -> " | "
                | BitXor -> " ^ "
                | BitAnd -> $" & "
                | MatMult -> $" @ "

            printer.Print(op)

        member printer.Print(node: Expression) =
            match node with
            | Attribute ex -> printer.Print(ex)
            | Subscript ex -> printer.Print(ex)
            | BoolOp ex -> printer.Print(ex)
            | BinOp ex -> printer.Print(ex)
            | Emit ex -> printer.Print(ex)
            | UnaryOp ex -> printer.Print(ex)
            | FormattedValue ex -> printer.Print(ex)
            | Constant(value = StringLiteral value) ->
                printer.Print("\"")
                printer.Print(Naming.escapeString (fun _ -> false) value)
                printer.Print("\"")
            | Constant(value = FloatLiteral value) ->
                let value = string<float> value
                printer.Print(value)

                // Make sure it's a valid Python float (not int)
                if
                    String.forall
                        (fun char -> char = '-' || Char.IsDigit char)
                        value
                then
                    printer.Print(".0")
            | Constant(value = BoolLiteral value) ->
                printer.Print(
                    if value then
                        "True"
                    else
                        "False"
                )
            | Constant(value = IntLiteral value) ->
                printer.Print(string<obj> value)
            | Constant(value = value) -> printer.Print(string<obj> value)

            | IfExp ex -> printer.Print(ex)
            | Call ex -> printer.Print(ex)
            | Lambda ex -> printer.Print(ex)
            | NamedExpr ex -> printer.Print(ex)
            | Name ex -> printer.Print(ex)
            | Await ex ->
                printer.Print("await ")
                printer.Print(ex)
            | Yield expr -> printer.Print("(Yield)")
            | YieldFrom expr -> printer.Print("(Yield)")
            | Compare cp -> printer.Print(cp)
            | Dict di -> printer.Print(di)
            | Tuple tu -> printer.Print(tu)
            | Slice(lower, upper, _step) ->
                if lower.IsSome then
                    printer.Print(lower.Value)

                printer.Print(":")

                if upper.IsSome then
                    printer.Print(upper.Value)
            | Starred(ex, _ctx) ->
                printer.Print("*")
                printer.Print(ex)
            | List(elts, _ctx) ->
                printer.Print("[")
                printer.PrintCommaSeparatedList(elts)
                printer.Print("]")

        member printer.Print(node: AST) =
            match node with
            | AST.Expression ex -> printer.Print(ex)
            | AST.Operator op -> printer.Print(op)
            | AST.BoolOperator op -> printer.Print(op)
            | AST.ComparisonOperator op -> printer.Print(op)
            | AST.UnaryOperator op -> printer.Print(op)
            | AST.ExpressionContext _ -> ()
            | AST.Alias al -> printer.Print(al)
            | AST.Module mo -> printer.Print(mo)
            | AST.Arguments arg -> printer.Print(arg)
            | AST.Keyword kw -> printer.Print(kw)
            | AST.Arg arg -> printer.Print(arg)
            | AST.Statement st -> printer.Print(st)
            | AST.Identifier id -> printer.Print(id)
            | AST.WithItem wi -> printer.Print(wi)

        member printer.PrintBlock
            (
                nodes: 'a list,
                printNode: Printer -> 'a -> unit,
                printSeparator: Printer -> unit,
                ?skipNewLineAtEnd
            )
            =
            let skipNewLineAtEnd = defaultArg skipNewLineAtEnd false
            printer.Print("")
            printer.PrintNewLine()
            printer.PushIndentation()

            for node in nodes do
                printNode printer node
                printSeparator printer

            printer.PopIndentation()
            printer.Print("")

            if not skipNewLineAtEnd then
                printer.PrintNewLine()

        member printer.PrintStatementSeparator() =
            if printer.Column > 0 then
                printer.Print("")
                printer.PrintNewLine()

        member printer.PrintStatement(stmt: Statement, ?printSeparator) =
            printer.Print(stmt)

            printSeparator |> Option.iter (fun fn -> fn printer)

        member printer.PrintStatements(statements: Statement list) =

            for stmt in statements do
                printer.PrintStatement(
                    stmt,
                    (fun p -> p.PrintStatementSeparator())
                )

        member printer.PrintBlock(nodes: Statement list, ?skipNewLineAtEnd) =
            printer.PrintBlock(
                nodes,
                (fun p s -> p.PrintStatement(s)),
                (fun p -> p.PrintStatementSeparator()),
                ?skipNewLineAtEnd = skipNewLineAtEnd
            )

        member printer.PrintOptional(before: string, node: Identifier option) =
            match node with
            | None -> ()
            | Some node ->
                printer.Print(before)
                printer.Print(node)

        member printer.PrintOptional
            (
                before: string,
                node: AST option,
                after: string
            )
            =
            match node with
            | None -> ()
            | Some node ->
                printer.Print(before)
                printer.Print(node)
                printer.Print(after)

        member printer.PrintOptional(node: AST option) =
            match node with
            | None -> ()
            | Some node -> printer.Print(node)

        member printer.PrintOptional(node: Expression option) =
            printer.PrintOptional(node |> Option.map AST.Expression)

        member printer.PrintOptional(node: Identifier option) =
            match node with
            | None -> ()
            | Some node -> printer.Print(node)

        member printer.PrintList
            (
                nodes: 'a list,
                printNode: Printer -> 'a -> unit,
                printSeparator: Printer -> unit
            )
            =
            for i = 0 to nodes.Length - 1 do
                printNode printer nodes[i]

                if i < nodes.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedList(nodes: AST list) =
            printer.PrintList(
                nodes,
                (fun p x -> p.Print(x)),
                (fun p -> p.Print(", "))
            )

        member printer.PrintCommaSeparatedList(nodes: Expression list) =
            printer.PrintList(
                nodes,
                (fun _ -> printer.Print),
                (fun p -> p.Print(", "))
            )

        member printer.PrintCommaSeparatedList(nodes: Arg list) =
            printer.PrintCommaSeparatedList(nodes |> List.map AST.Arg)

        member printer.PrintCommaSeparatedList(nodes: Keyword list) =
            printer.PrintCommaSeparatedList(nodes |> List.map AST.Keyword)

        member printer.PrintCommaSeparatedList(nodes: Alias list) =
            printer.PrintCommaSeparatedList(nodes |> List.map AST.Alias)

        member printer.PrintCommaSeparatedList(nodes: Identifier list) =
            printer.PrintCommaSeparatedList(nodes |> List.map AST.Identifier)

        member printer.PrintCommaSeparatedList(nodes: WithItem list) =
            printer.PrintCommaSeparatedList(nodes |> List.map AST.WithItem)

        member printer.PrintFunction
            (
                id: Identifier option,
                args: Arguments,
                body: Statement list,
                returnType: Expression option,
                decoratorList: Expression list,
                ?isDeclaration,
                ?isAsync
            )
            =
            for deco in decoratorList do
                printer.Print("@")
                printer.Print(deco)
                printer.PrintNewLine()

            match isAsync with
            | Some true -> printer.Print("async ")
            | _ -> ()

            printer.Print("def ")
            printer.PrintOptional(id)
            printer.Print("(")
            printer.Print(args)
            printer.Print(")")

            if returnType.IsSome then
                printer.Print(" -> ")
                printer.PrintOptional(returnType)

            printer.Print(":")
            printer.PrintBlock(body, skipNewLineAtEnd = true)

        member printer.WithParens(expr: Expression) =
            printer.Print("(")
            printer.Print(expr)
            printer.Print(")")

        /// Surround with parens anything that can potentially conflict with operator precedence
        member printer.ComplexExpressionWithParens(expr: Expression) =
            match expr with
            | Constant _
            | Name _
            | Call _
            | List _
            | Subscript _
            | Attribute _ -> printer.Print(expr)
            | _ -> printer.WithParens(expr)

        member printer.PrintOperation(left, operator, right, ?loc) =
            printer.AddLocation(loc)
            printer.ComplexExpressionWithParens(left)
            printer.Print(operator)
            printer.ComplexExpressionWithParens(right)

open PrinterExtensions

let printDeclWithExtraLine extraLine (printer: Printer) (decl: Statement) =
    printer.Print(decl)

    if printer.Column > 0 then
        printer.PrintNewLine()

    if extraLine then
        printer.PrintNewLine()

let printLine (printer: Printer) (line: string) =
    printer.Print(line)
    printer.PrintNewLine()

let isEmpty (_program: Module) : bool = false // TODO: determine if printer will not print anything

let run writer (program: Module) : Async<unit> =
    async {
        use printerImpl = new PrinterImpl(writer)
        let printer = printerImpl :> Printer

        let imports, restDecls =
            program.Body
            |> List.splitWhile (
                function
                | Import _
                | ImportFrom _ -> true
                | Expr { Value = Expression.Emit _ } -> true
                | _ -> false
            )

        for decl in imports do
            match decl with
            | ImportFrom({ Module = Some(Identifier path) } as info) ->
                let path = printer.MakeImportPath(path)
                ImportFrom { info with Module = Some(Identifier path) }
            | decl -> decl
            |> printDeclWithExtraLine false printer

        printer.PrintNewLine()
        do! printerImpl.Flush()

        for decl in restDecls do
            printDeclWithExtraLine true printer decl
            // TODO: Only flush every XXX lines?
            do! printerImpl.Flush()
    }
