// fsharplint:disable InterfaceNames
module Fable.Transforms.Go.GoPrinter

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
            | EmptyStmt empty -> printer.Print(empty)
            | DeclStmt decl -> printer.Print(decl)
            | LabeledStmt label -> printer.Print(label)

        member printer.Print(stmt: LabeledStmt) =
            printer.Print("LabeledStmt")

        member printer.Print(stmt: EmptyStmt) =
            printer.Print("EmptyStmt")

        member printer.Print(stmt: DeclStmt) =
            printer.Print("DeclStmt")

        member printer.Print(stmt: ExprStmt) =
            printer.Print(stmt.X)

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

        member printer.Print(expr: Expr) =
            match expr with
            | EmitExpr expr -> printer.Print(expr)
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
            | ArrayType expr -> printer.Print(expr)
            | StructType expr -> printer.Print(expr)
            | FuncType expr -> printer.Print(expr)
            | InterfaceType expr -> printer.Print(expr)
            | MapType expr -> printer.Print(expr)
            | ChanType expr -> printer.Print(expr)

        member printer.Print(expr: BadExpr) =
            printer.Print("BadExpr")

        member printer.Print(expr: BasicLit) =
            match expr.Kind with
            | Token.String ->
                printer.Print("\"" + expr.Value + "\"")
            | _ -> printer.Print(expr.Value)

        member printer.Print(expr: FuncLit) =
            printer.Print("FuncLit")

        member printer.Print(expr: CompositeLit) =
            printer.Print("CompositeLit")

        member printer.Print(expr: ParenExpr) =
            printer.WithParens(expr.X)

        member printer.Print(expr: SelectorExpr) =
            printer.Print(expr.X)
            printer.Print(".")
            printer.Print(expr.Sel)

        member printer.Print(expr: IndexExpr) =
            printer.Print(expr.X)
            printer.Print("[")
            printer.Print(expr.Index)
            printer.Print("]")

        member printer.Print(expr: IndexListExpr) =
            printer.Print(expr.X)
            printer.Print("[")
            printer.PrintCommaSeparatedList(expr.Indices)
            printer.Print("]")

        member printer.Print(expr: SliceExpr) =
            printer.Print(expr.X)
            printer.Print("[")
            printer.PrintOptional(expr.Low)
            printer.Print(":")
            printer.PrintOptional(expr.High)
            printer.Print("]")

        member printer.Print(expr: TypeAssertExpr) =
            printer.Print("TypeAssertExpr")

        member printer.Print(expr: CallExpr) =
            printer.Print(expr.Fun)
            printer.Print("(")
            printer.PrintCommaSeparatedList(expr.Args)
            printer.Print(")")

        member printer.Print(expr: StarExpr) =
            printer.Print("*")
            printer.Print(expr.X)

        member printer.Print(expr: UnaryExpr) =
            printer.Print(expr.Op)
            printer.Print(expr.X)

        member printer.Print(expr: BinaryExpr) =
            printer.Print(expr.X)
            printer.Print(expr.Op)
            printer.Print(expr.Y)

        member printer.Print(fields: FieldList ) =
            printer.PrintCommaSeparatedList(fields.List)

        member printer.Print(field: Field) =
            printer.PrintCommaSeparatedList(field.Names)
            printer.Print(" ")
            printer.PrintOptional(field.Type)
            // printer.PrintOptional(field.Tag)
            // printer.Print(field.Comment)

        member printer.Print(spec: Spec) =
            match spec with
            | ValueSpec spec -> printer.Print(spec)
            | TypeSpec spec -> printer.Print(spec)
            | ImportSpec spec -> printer.Print(spec)

        member printer.Print(node: EmitExpr) =
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
                    let i = int m.Groups[1].Value

                    for j = i to node.Args.Length - 1 do
                        rep.Add("$" + string j)

                    String.concat ", " rep)

                |> replace @"\{\{\s*\$(\d+)\s*\?(.*?):(.*?)\}\}" (fun m ->
                    let i = int m.Groups[1].Value

                    match node.Args[i] with
                    | Ident { Name = value } when value = "false" || value = "true" -> m.Groups[2].Value
                    | _ -> m.Groups[3].Value)

                |> replace @"\{\{([^\}]*\$(\d+).*?)\}\}" (fun m ->
                    let i = int m.Groups[2].Value

                    match List.tryItem i node.Args with
                    | Some _ -> m.Groups[1].Value
                    | None -> "")

                // If placeholder is followed by !, emit string literals as JS: "let $0! = $1"
                |> replace @"\$(\d+)!" (fun m ->
                    let i = int m.Groups[1].Value

                    match List.tryItem i node.Args with
                    | Some (BasicLit { Kind = Token.String; Value = value }) -> value
                    | _ -> "")

            let matches = System.Text.RegularExpressions.Regex.Matches(value, @"\$\d+")

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
                printSegment printer value (lastMatch.Index + lastMatch.Length) value.Length
            else
                printSegment printer value 0 value.Length

        member printer.Print(expr: KeyValueExpr) =
            printer.Print(expr.Key)
            printer.Print(": ")
            printer.Print(expr.Value)

        member printer.Print(ident: Ident) =
            match ident.ImportModule with
            | None -> ()
            | Some p -> printer.Print(p + ".")
            printer.Print(ident.Name)

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

        member printer.Print(spec: ImportSpec) =
            printer.Print("import ")
            match spec.Name with
            | Some { Name = name } when name <> spec.Path.Value ->
                printer.Print(name)
                printer.Print(" ")
            | _ -> ()

            printer.Print(spec.Path)
            printer.PrintNewLine()

        member printer.Print(spec: TypeSpec) =
            if spec.Name.IsSome then
                printer.Print(spec.Name.Value)
                printer.Print(" ")
            printer.Print(spec.Type)

        member printer.Print(spec: ValueSpec) =
            printer.PrintCommaSeparatedList(spec.Names)
            if spec.Type.IsSome then
                printer.Print(" ")
                printer.Print(spec.Type.Value)
            printer.Print(" = ")
            printer.PrintCommaSeparatedList(spec.Values)

        member printer.Print(decl: Decl) =
            match decl with
            | BadDecl decl -> printer.Print(decl)
            | FuncDecl decl -> printer.Print(decl)
            | GenDecl decl -> printer.Print(decl)

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

        member printer.PrintBlock(nodes: BlockStmt, ?skipNewLineAtEnd) =
            let nodes = nodes.List

            printer.PrintBlock(
                nodes,
                (fun p s -> p.PrintStatement(s)),
                (fun p -> p.PrintStatementSeparator()),
                ?skipNewLineAtEnd = skipNewLineAtEnd
            )

        member printer.PrintOptional(before: string, node: Ident option) =
            match node with
            | None -> ()
            | Some node ->
                printer.Print(before)
                printer.Print(node)

        member printer.PrintOptional(node: Expr option) =
             printer.PrintOptional(node |> Option.map Expr)

        member printer.PrintOptional(node: Node option) =
            match node with
            | Some(Expr(node)) -> printer.Print(node)
            | Some(Stmt(node)) -> printer.Print(node)
            | Some(Decl(node)) -> printer.Print(node)
            | None -> ()

        member printer.PrintList(nodes: 'a list, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
            for i = 0 to nodes.Length - 1 do
                printNode printer nodes[i]

                if i < nodes.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedList(nodes: Expr list) =
            printer.PrintList(nodes, (fun p x -> printer.Print(x)), (fun p -> p.Print(", ")))

        member printer.PrintCommaSeparatedList(nodes: Field list) =
            printer.PrintList(nodes, (fun p x -> printer.Print(x)), (fun p -> p.Print(", ")))

        member printer.PrintCommaSeparatedList(nodes: Ident list) =
            printer.PrintList(nodes, (fun p x -> printer.Print(x)), (fun p -> p.Print(", ")))

        member printer.PrintCommaSeparatedList(nodes: Spec list) =
            printer.PrintList(nodes, (fun p x -> printer.Print(x)), (fun p -> p.Print(", ")))

        member printer.WithParens(expr: Expr) =
            printer.Print("(")
            printer.Print(expr)
            printer.Print(")")

        /// Surround with parens anything that can potentially conflict with operator precedence
        member printer.ComplexExpressionWithParens(expr: Expr) =
            match expr with
            // | Constant _
            // | Name _
            // | Call _
            // | List _
            // | Subscript _
            // | Attribute _ -> printer.Print(expr)
            | _ -> printer.WithParens(expr)

        member printer.Print(arr: ArrayType) =
            ()

        member printer.Print(arr: StructType) =
            ()

        member printer.Print(arr: FuncType) =
            ()
        member printer.Print(arr: InterfaceType) =
            ()
        member printer.Print(arr: MapType) =
            ()
        member printer.Print(arr: ChanType) =
            ()

        // member printer.PrintOperation(left, operator, right, ?loc) =
        //     printer.AddLocation(loc)
        //     printer.ComplexExpressionWithParens(left)
        //     printer.Print(operator)
        //     printer.ComplexExpressionWithParens(right)

        member printer.Print(decl: FuncDecl) =
            printer.Print("func ")
            printer.Print(decl.Name)
            printer.Print("(")
            if decl.Recv.IsSome then
                printer.Print(decl.Recv.Value)
            printer.Print(") {")
            printer.PushIndentation()

            if decl.Body.IsSome then
                printer.PrintBlock(decl.Body.Value)
            printer.PopIndentation()
            printer.Print("}")

        member printer.Print(decl: GenDecl) =
            match decl.Tok with
            | Token.Const -> printer.Print("const")
            | Token.Var -> printer.Print("var")
            | Token.Type -> printer.Print("type")
            | _ -> printer.Print($"GenDecl: unknown token: {decl.Tok}")

            printer.Print(" ")
            printer.PrintCommaSeparatedList(decl.Specs)

        member printer.Print(decl: BadDecl) =
            printer.Print("BadDecl")

open PrinterExtensions

let printDeclWithExtraLine extraLine (printer: Printer) (decl: Decl) =
    printer.Print(decl)

    if printer.Column > 0 then
        printer.PrintNewLine()

    if extraLine then printer.PrintNewLine()

let printLine (printer: Printer) (line: string) =
    printer.Print(line)
    printer.PrintNewLine()

let isEmpty (program: File) : bool = false //TODO: determine if printer will not print anything


let run writer (program: File) : Async<unit> =
    async {
        use printerImpl = new PrinterImpl(writer, indent="\t")
        let printer = printerImpl :> Printer

        printer.Print("package ")
        printer.Print(program.Name)
        printer.PrintNewLine()
        printer.PrintNewLine()

        let imports = program.Imports
        for spec in imports do
            match spec with
            | { Name=name; Path={Value=path}} as info ->
                let path = printer.MakeImportPath(path)
                {spec with Path= BasicLit.basicLit(kind=Token.String, value=path) }
            |> printer.Print

        printer.PrintNewLine()
        do! printerImpl.Flush()

        for decl in program.Decls do
            printDeclWithExtraLine true printer decl
            // TODO: Only flush every XXX lines?
            do! printerImpl.Flush()
    }
