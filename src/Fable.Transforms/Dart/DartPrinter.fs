module Fable.Transforms.Dart.DartPrinter

open System
open Fable.AST
open Fable.AST.Fable

type Writer =
    inherit IDisposable
    abstract Write: string -> Async<unit>
    abstract EscapeStringLiteral: string -> string
    abstract MakeImportPath: string -> string
    abstract AddLog: msg:string * severity: Fable.Severity * ?range: SourceLocation -> unit

type Printer =
    abstract Line: int
    abstract Column: int
    abstract PushIndentation: unit -> unit
    abstract PopIndentation: unit -> unit
    abstract Print: string -> unit
    abstract PrintNewLine: unit -> unit
    abstract EscapeStringLiteral: string -> string
    abstract MakeImportPath: string -> string
    abstract AddLog: msg:string * severity: Fable.Severity * ?range: SourceLocation -> unit

type PrinterImpl(writer: Writer) =
    // TODO: We can make this configurable later
    let indentSpaces = "    "
    let builder = Text.StringBuilder()
    let mutable indent = 0
    let mutable line = 1
    let mutable column = 0

    member _.Flush(): Async<unit> =
        async {
            do! writer.Write(builder.ToString())
            builder.Clear() |> ignore
        }

    interface IDisposable with
        member _.Dispose() = writer.Dispose()

    interface Printer with
        member _.Line = line
        member _.Column = column

        member _.PrintNewLine() =
            builder.AppendLine() |> ignore
            line <- line + 1
            column <- 0

        member _.PushIndentation() =
            indent <- indent + 1

        member _.PopIndentation() =
            if indent > 0 then indent <- indent - 1

        member _.Print(str: string) =
            if column = 0 then
                let indent = String.replicate indent indentSpaces
                builder.Append(indent) |> ignore
                column <- indent.Length

            builder.Append(str) |> ignore
            column <- column + str.Length

        member this.EscapeStringLiteral(str) =
            writer.EscapeStringLiteral(str)

        member this.MakeImportPath(path) =
            writer.MakeImportPath(path)

        member this.AddLog(msg, severity, ?range) =
            writer.AddLog(msg, severity, ?range=range)

module PrinterExtensions =
    type Printer with
        member this.AddError(msg, ?range) =
            this.AddLog(msg, Fable.Severity.Error, ?range=range)

        member this.AddWarning(msg, ?range) =
            this.AddLog(msg, Fable.Severity.Warning , ?range=range)

        member printer.PrintBlock(nodes: 'a array, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit, ?skipNewLineAtEnd) =
            let skipNewLineAtEnd = defaultArg skipNewLineAtEnd false
            printer.Print("{")
            printer.PrintNewLine()
            printer.PushIndentation()
            for node in nodes do
                printer.Print("return ") // TODO HACK
                printNode printer node
                printSeparator printer
            printer.PopIndentation()
            printer.Print("}")
            if not skipNewLineAtEnd then
                printer.PrintNewLine()

        member printer.PrintBlock(nodes: Expr list, ?skipNewLineAtEnd) =
            printer.PrintBlock(List.toArray nodes,
                               (fun p s -> p.Print(s)), // TODO: p.PrintProductiveStatement(s)),
                               (fun p -> p.PrintStatementSeparator()),
                               ?skipNewLineAtEnd=skipNewLineAtEnd)

        member printer.PrintStatementSeparator() =
            if printer.Column > 0 then
                printer.Print(";")
                printer.PrintNewLine()

        // TODO: Use Transforms.AST.canHaveSideEffects?
        member this.HasSideEffects(e: Expr) =
            match e with
            | _ -> true

        member this.IsProductiveStatement(s: Expr) =
            this.HasSideEffects(s)

        member printer.PrintProductiveStatement(s: Expr, ?printSeparator) =
            if printer.IsProductiveStatement(s) then
                printer.Print(s)
                printSeparator |> Option.iter (fun f -> f printer)

        member printer.Print(t: Type) =
            match t with
            | Any -> printer.Print("Object")
            | Unit -> printer.Print("null")
            | Boolean -> printer.Print("bool")
            | Char -> printer.Print("null")
            | String -> printer.Print("String")
            | Number(kind,_) ->
                match kind with
                | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 -> printer.Print("int")
                | Float32 | Float64 -> printer.Print("double")
            | _ -> printer.AddError("TODO: Print type")

        // TODO
        member printer.ComplexExpressionWithParens(expr: Expr) =
            printer.Print(expr)

        member printer.PrintOperation(kind) =
            match kind with
            | Binary(operator, left, right) ->
                printer.ComplexExpressionWithParens(left)
                // TODO: review
                match operator with
                | BinaryEqual | BinaryEqualStrict -> printer.Print(" == ")
                | BinaryUnequal | BinaryUnequalStrict -> printer.Print(" != ")
                | BinaryLess -> printer.Print(" < ")
                | BinaryLessOrEqual -> printer.Print(" <= ")
                | BinaryGreater -> printer.Print(" > ")
                | BinaryGreaterOrEqual -> printer.Print(" >= ")
                | BinaryShiftLeft -> printer.Print(" << ")
                | BinaryShiftRightSignPropagating -> printer.Print(" >> ")
                | BinaryShiftRightZeroFill -> printer.Print(" >>> ")
                | BinaryMinus -> printer.Print(" - ")
                | BinaryPlus -> printer.Print(" + ")
                | BinaryMultiply -> printer.Print(" * ")
                | BinaryDivide -> printer.Print(" / ")
                | BinaryModulus -> printer.Print(" % ")
                | BinaryExponent -> printer.Print(" ** ")
                | BinaryOrBitwise -> printer.Print(" | ")
                | BinaryXorBitwise -> printer.Print(" ^ ")
                | BinaryAndBitwise -> printer.Print(" & ")
                | BinaryIn | BinaryInstanceOf -> printer.AddError($"Operator not supported {operator}")
                printer.ComplexExpressionWithParens(right)

            | Logical(operator, left, right) ->
                printer.ComplexExpressionWithParens(left)
                match operator with
                | LogicalOr -> printer.Print(" || ")
                | LogicalAnd -> printer.Print(" && ")
                printer.ComplexExpressionWithParens(right)

            | Unary(operator, operand) ->
                match operator with
                | UnaryMinus -> printer.Print("-")
                | UnaryPlus -> printer.Print("+")
                | UnaryNot -> printer.Print("!")
                | UnaryNotBitwise -> printer.Print("~")
                | UnaryTypeof | UnaryVoid | UnaryDelete -> printer.AddError($"Operator not supported {operator}")
                printer.ComplexExpressionWithParens(operand)

        member printer.PrintValue(kind: ValueKind) =
            match kind with
            | BoolConstant v -> printer.Print((if v then "true" else "false"))
            | StringConstant value ->
                printer.Print("\"")
                printer.Print(printer.EscapeStringLiteral(value))
                printer.Print("\"")
//            | CharConstant of value: char
            | NumberConstant(value,_,_) ->
                let value =
                    match value.ToString(System.Globalization.CultureInfo.InvariantCulture) with
                    | "∞" -> "double.infinity"
                    | "-∞" -> "-double.infinity"
                    | value -> value
                printer.Print(value)
            | ThisValue _ -> printer.Print("this")
            | Null _ | UnitConstant -> printer.Print("null")
//            | NewArray of values: Expr list * typ: Type
//            | NewArrayFrom of value: Expr * typ: Type
//            | BaseValue of boundIdent: Ident option * typ: Type
//            | TypeInfo of typ: Type
//            | RegexConstant of source: string * flags: RegexFlag list
//            | EnumConstant of value: Expr * ref: EntityRef
//            | NewOption of value: Expr option * typ: Type * isStruct: bool
//            | NewList of headAndTail: (Expr * Expr) option * typ: Type
//            | NewTuple of values: Expr list * isStruct: bool
//            | NewRecord of values: Expr list * ref: EntityRef * genArgs: Type list
//            | NewAnonymousRecord of values: Expr list * fieldNames: string [] * genArgs: Type list
//            | NewUnion of values: Expr list * tag: int * ref: EntityRef * genArgs: Type list
            | _ -> printer.AddError("TODO: Print value")

        member printer.Print(expr: Expr) =
            match expr with
            | IdentExpr i -> printer.Print(i.Name)
            | Value(kind,_) -> printer.PrintValue(kind)
            | Operation(kind,_,_) -> printer.PrintOperation(kind)
            | _ -> printer.AddError("TODO: Print expression")

        member printer.PrintArray(items: 'a array, printItem: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
            for i = 0 to items.Length - 1 do
                printItem printer items.[i]
                if i < items.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedArray(nodes: Ident array) =
            printer.PrintArray(nodes, (fun p x -> p.Print(x.Name)), (fun p -> p.Print(", ")))

        member printer.PrintFunctionDeclaration(name: string, args: Ident list, body: Expr) =
            printer.Print(body.Type)
            printer.Print(" ")
            printer.Print(name)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(List.toArray args)
            printer.Print(") ")

            printer.PrintBlock([body], skipNewLineAtEnd=true)

        member printer.Print(md: Declaration) =
            match md with
            | ModuleDeclaration _ -> printer.AddError("Nested modules are not supported")
            | ActionDeclaration _ -> printer.AddError("TODO: Action declaration")
            | ClassDeclaration _ -> printer.AddError("TODO: Class declaration")
            | MemberDeclaration m -> printer.PrintFunctionDeclaration(m.Name, m.Args, m.Body)

open PrinterExtensions

let run (writer: Writer) (file: File): Async<unit> =
    let printDeclWithExtraLine extraLine (printer: Printer) (decl: Declaration) =
        printer.Print(decl)

//        if printer.Column > 0 then
//            printer.Print(";")
//            printer.PrintNewLine()
        if extraLine then
            printer.PrintNewLine()

    async {
        use printer = new PrinterImpl(writer)

        // TODO: Imports
        for decl in file.Declarations do
            printDeclWithExtraLine true printer decl
            do! printer.Flush()
    }
