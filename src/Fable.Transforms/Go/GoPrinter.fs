// fsharplint:disable InterfaceNames
module Fable.Transforms.Go.GoPrinter

open System

open Fable
open Fable.AST
open Fable.AST.Go
open Fable.Transforms.Printer

module PrinterExtensions =
    type Printer with

        member printer.Print(stmt: Stmt) =
            match stmt with
            | ExprStmt expr -> printer.Print(expr)
            | SendStmt send -> printer.Print(send)
            | AssignStmt assign -> printer.Print(assign)
            | ForStmt forIn -> printer.Print(forIn)
            | IfStmt ifElse -> printer.Print(ifElse)
            | IncDecStmt incDec -> printer.Print(incDec)
            | GoStmt go -> printer.Print(go)
            | DefStmt def -> printer.Print(def)
            | ReturnStmt ret -> printer.Print(ret)
            | BranchStmt branch -> printer.Print(branch)
            | BlockStmt block -> printer.Print(block)
            | CaseClause case -> printer.Print(case)
            | SwitchStmt switch -> printer.Print(switch)
            | TypeSwitchStmt typeSwitch -> printer.Print(typeSwitch)
            | CommClause comm -> printer.Print(comm)
            | SelectStmt select -> printer.Print(select)
            | RangeStmt range -> printer.Print(range)


        member printer.Print(stmt: ExprStmt) =
            printer.Print("ExprStmt")


        member printer.Print(stmt: SendStmt) =
            printer.Print("SendStmt")

        member printer.Print(kw: AssignStmt) =
            printer.PrintCommaSeparatedList(kw.Lhs)
            printer.Print(" = ")
            printer.PrintCommaSeparatedList(kw.Rhs)

        member printer.Print(stmt: ForStmt) =
            printer.Print("ForStmt")

        member printer.Print(stmt: IfStmt) =
            printer.Print("IfStmt")

        member printer.Print(stmt: IncDecStmt) =
            printer.Print(stmt.X)
            printer.Print(stmt.Tok)

        member printer.Print(stmt: GoStmt) =
            printer.Print("GoStmt")

        member printer.Print(stmt: DefStmt) =
            printer.Print("DefStmt")

        member printer.Print(stmt: ReturnStmt) =
            printer.Print("return ")
            printer.PrintCommaSeparatedList(stmt.Results)

        member printer.Print(stmt: BranchStmt) =
            printer.Print("BranchStmt")

        member printer.Print(stmt: BlockStmt) =
            printer.Print("(BlockStmt")

        member printer.Print(stmt: CaseClause) =
            printer.Print("CaseClause")

        member printer.Print(stmt: SwitchStmt) =
            printer.Print("SwitchStmt")

        member printer.Print(stmt: TypeSwitchStmt) =
            printer.Print("TypeSwitchStmt")

        member printer.Print(stmt: CommClause) =
            printer.Print("CommClause")

        member printer.Print(stmt: SelectStmt) =
            printer.Print("SelectStmt")

        member printer.Print(func: RangeStmt) =
            printer.Print("RangeStmt")

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
            let (Identifier path) = im.Module |> Option.defaultValue (Identifier ".")

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
            | Tuple { Elements = elems } -> printer.PrintCommaSeparatedList(elems)
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

            match node.Op with
            | USub
            | UAdd
            | Not
            | Invert -> printer.Print(node.Op)

            printer.ComplexExpressionWithParens(node.Operand)

        member printer.Print(node: FormattedValue) = printer.Print("(FormattedValue)")

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
            let inline replace pattern (f: System.Text.RegularExpressions.Match -> string) input =
                System.Text.RegularExpressions.Regex.Replace(input, pattern, f)

            let printSegment (printer: Printer) (value: string) segmentStart segmentEnd =
                let segmentLength = segmentEnd - segmentStart

                if segmentLength > 0 then
                    let segment = value.Substring(segmentStart, segmentLength)
                    printer.Print(segment)

            // Macro transformations
            // https://fable.io/docs/communicate/js-from-fable.html#Emit-when-F-is-not-enough
            let value =
                node.Value
                |> replace @"\$(\d+)\.\.\." (fun m ->
                    let rep = ResizeArray()
                    let i = int m.Groups.[1].Value

                    for j = i to node.Args.Length - 1 do
                        rep.Add("$" + string j)

                    String.concat ", " rep)

                |> replace @"\{\{\s*\$(\d+)\s*\?(.*?):(.*?)\}\}" (fun m ->
                    let i = int m.Groups.[1].Value

                    match node.Args.[i] with
                    | Constant(value = :? bool as value) when value -> m.Groups[2].Value
                    | _ -> m.Groups.[3].Value)

                |> replace @"\{\{([^\}]*\$(\d+).*?)\}\}" (fun m ->
                    let i = int m.Groups[2].Value

                    match List.tryItem i node.Args with
                    | Some _ -> m.Groups[1].Value
                    | None -> "")

                // If placeholder is followed by !, emit string literals as JS: "let $0! = $1"
                |> replace @"\$(\d+)!" (fun m ->
                    let i = int m.Groups.[1].Value

                    match List.tryItem i node.Args with
                    | Some (Constant (:? string as value, _)) -> value
                    | _ -> "")

            let matches = System.Text.RegularExpressions.Regex.Matches(value, @"\$\d+")

            if matches.Count > 0 then
                for i = 0 to matches.Count - 1 do
                    let m = matches[i]

                    let isSurroundedWithParens =
                        m.Index > 0
                        && m.Index + m.Length < value.Length
                        && value.[m.Index - 1] = '('
                        && value.[m.Index + m.Length] = ')'

                    let segmentStart =
                        if i > 0 then
                            matches[i - 1].Index + matches.[i - 1].Length
                        else
                            0

                    printSegment printer value segmentStart m.Index

                    let argIndex = int m.Value[1..]

                    match List.tryItem argIndex node.Args with
                    | Some e when isSurroundedWithParens -> printer.Print(e)
                    | Some e -> printer.ComplexExpressionWithParens(e)
                    | None -> printer.Print("None")

                let lastMatch = matches.[matches.Count - 1]
                printSegment printer value (lastMatch.Index + lastMatch.Length) value.Length
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

        member printer.Print(node: List) = printer.Print("(List)")

        member printer.Print(node: Set) = printer.Print("(Set)")

        member printer.Print(node: Dict) =
            printer.Print("{")
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


        member printer.Print(node: Module) = printer.PrintStatements(node.Body)

        member printer.Print(node: Ident) =
            printer.Print(node.Name)

        member printer.Print(node: Token) =
            let op =
                match node with
                | Token.Add -> "+"
                | Token.Sub -> "-"
                | Token.Mul -> "*"
                | Token.Quo -> "/"
                | Token.Rem -> "//"
                | _ -> failwith $"Token {node} not implemented"

            printer.Print(op)

        member printer.Print(expr: Expr) =
            match expr with
            | Emit expr -> printer.Print(expr)
            | BadExpr expr -> printer.Print(expr)
            | Ident expr -> printer.Print(expr)
            | BasicLit expr -> printer.Print(expr)
            | FuncLit expr -> printer.Print(expr)
            | CompositeLit expr -> printer.Print(expr)
            | ParenExpr expr -> printer.Print(expr)
            | SelectorExpr expr -> printer.Print(expr)
            | IndexExpr expr -> printer.Print(expr)
            | IndexListExpr expr -> printer.Print(expr)
            | SliceExpr expr -> printer.Print(expr)
            | TypeAssertExpr expr -> printer.Print(expr)
            | CallExpr expr -> printer.Print(expr)
            | StarExpr expr -> printer.Print(expr)
            | UnaryExpr expr -> printer.Print(expr)
            | BinaryExpr expr -> printer.Print(expr)
            | KeyValueExpr expr -> printer.Print(expr)

        member printer.Print(node: File) =
            printer.Print("package ")
            printer.Print(node.Name)
            printer.PrintNewLine()
            for import in node.Imports do
                printer.Print(import)
                printer.PrintNewLine()

            for decl in node.Decls do
                printer.Print(decl)
                printer.PrintNewLine()

        member printer.PrintBlock(nodes: 'a list, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit, ?skipNewLineAtEnd) =
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

        member printer.PrintStatement(stmt: Stmt, ?printSeparator) =
            printer.Print(stmt)

            printSeparator
            |> Option.iter (fun fn -> fn printer)

        member printer.PrintStatements(statements: Stmt list) =

            for stmt in statements do
                printer.PrintStatement(stmt, (fun p -> p.PrintStatementSeparator()))

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

        member printer.PrintOptional(before: string, node: AST option, after: string) =
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

        member printer.PrintList(nodes: 'a list, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
            for i = 0 to nodes.Length - 1 do
                printNode printer nodes[i]

                if i < nodes.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedList(nodes: AST list) =
            printer.PrintList(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

        member printer.PrintCommaSeparatedList(nodes: Expression list) =
            printer.PrintList(nodes, (fun p x -> printer.Print(x)), (fun p -> p.Print(", ")))

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

        member printer.PrintFunc
            (
                id: Identifier option,
                args: Arguments,
                body: Statement list,
                returnType: Expression option,
                decoratorList: Expression list,
                ?isDeclaration,
                ?isAsync
            ) =
            for deco in decoratorList do
                printer.Print("@")
                printer.Print(deco)
                printer.PrintNewLine()

            match isAsync with
            | Some true ->
                printer.Print("async ")
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

        member printer.WithParens(expr: Expr) =
            printer.Print("(")
            printer.Print(expr)
            printer.Print(")")

        /// Surround with parens anything that can potentially conflict with operator precedence
        member printer.ComplexExpressionWithParens(expr: Expr) =
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

let printDeclWithExtraLine extraLine (printer: Printer) (decl: Stmt) =
    printer.Print(decl)

    if printer.Column > 0 then
        printer.PrintNewLine()

    if extraLine then printer.PrintNewLine()

let printLine (printer: Printer) (line: string) =
    printer.Print(line)
    printer.PrintNewLine()

let isEmpty (program: Module) : bool = false //TODO: determine if printer will not print anything

let run writer (program: Module) : Async<unit> =
    async {
        use printerImpl = new PrinterImpl(writer)
        let printer = printerImpl :> Printer

        let imports, restDecls =
            program.Body
            |> List.splitWhile (function
                | Import _
                | ImportFrom _ -> true
                | Expr { Value = Expression.Emit _ } -> true
                | _ -> false)

        for decl in imports do
            match decl with
            | ImportFrom ({ Module = Some (Identifier path) } as info) ->
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
