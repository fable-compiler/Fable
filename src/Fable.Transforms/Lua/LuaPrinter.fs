module Fable.Transforms.Lua.LuaPrinter

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
            printer.PrintNewLine()
            printer.PushIndentation()
            // for node in nodes do
            //     printNode printer node
            //     printSeparator printer
            // printer.PopIndentation()
            // printer.Print("end")
            match nodes |> Array.toList |> List.rev with
            | h::t ->
                for node in t |> List.rev do
                    printNode printer node
                    printSeparator printer
                printSeparator printer
                printer.Print("return ")
                printNode printer h
            | _ -> ()
            if not skipNewLineAtEnd then
                printer.PrintNewLine()
            printer.PopIndentation()

        member printer.PrintBlock(nodes: Expr list, ?skipNewLineAtEnd) =
            printer.PrintBlock(List.toArray nodes,
                               (fun p s -> p.Print(s)), // TODO: p.PrintProductiveStatement(s)),
                               (fun p -> p.PrintStatementSeparator()),
                               ?skipNewLineAtEnd=skipNewLineAtEnd)

        member printer.PrintStatementSeparator() =
            if printer.Column > 0 then
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
            // match t with
            // | Any -> printer.Print("Object")
            // | Unit -> printer.Print("null")
            // | Boolean -> printer.Print("bool")
            // | Char -> printer.Print("null")
            // | String -> printer.Print("String")
            // | Number(kind,_) ->
            //     match kind with
            //     | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 -> printer.Print("int")
            //     | Float32 | Float64 -> printer.Print("double")
            // | _ -> printer.AddError("TODO: Print type")
            ()

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
            | Extended(kind,_) ->
                match kind with
                //| Return (Value (UnitConstant, _)) -> ()
                | Return e ->
                    printer.Print("return ")
                    //printer.PrintSelfExecutingFnOpen()
                    printer.Print(e)
                    //printer.PrintSelfExecutingFnClose()
                | _ -> printer.AddError("TODO: Print extended set")
            | Import ({ Selector = "toConsole" }, b, c) ->
                printer.Print("print")
            | Import ({ Selector = "printfn" }, b, c) ->
                printer.Print("")
            | Import ({ Selector = "printf" }, b, c) ->
                printer.Print("")
            | Import (importInfo, b, c) ->
                printer.Print("require(\"")
                printer.Print(importInfo.Path)
                printer.Print("""")""")
                printer.Print(".")
                printer.Print(importInfo.Selector)
                //printer.PrintNewLine()
                //printer.Print(sprintf "require(%A)" a)
            | Call (expr, callInfo, t, d) ->
                //printer.Print(sprintf "%A %A" expr callInfo)
                match callInfo.CallMemberInfo with
                | Some m ->
                    printer.Print(m.CompiledName)
                    printer.Print(".")
                    printer.Print(expr)

                | None ->
                    printer.Print(expr)
                    //printer.Print(sprintf "(%A)" expr)
                printer.Print("(")
                callInfo.Args |> Seq.toArray |> printer.PrintCommaSeparatedArgsArray
                printer.Print(")")
            | Lambda(arg, body, name) ->
                printer.Print("function(")
                printer.Print(arg.Name)
                printer.Print(")")
                printer.PushIndentation()
                printer.PrintNewLine()
                printer.Print(body)
                printer.PopIndentation()
                printer.PrintNewLine()
                printer.Print("end")
            | Delegate(args, body, name) -> printer.Print("todo delegate")
            | ObjectExpr(members, typ, baseCall) -> printer.Print("todo object")
            | TypeCast(expr, _) ->
                //printer.Print(sprintf "cast %A" expr)
                printer.Print(expr)
                ()
            | Test(expr, kind, range) -> printer.Print("todo test")
            | CurriedApply(applied, args, _, _) ->
                printer.Print(applied)
                printer.Print("(")
                args |> Seq.toArray |> printer.PrintCommaSeparatedArgsArray
                printer.Print(")")
                //printer.Print("todoApply")
            // | CurriedApply(applied, args, typ, range) ->
            //     printer.Print(applied)
            //     args |> Seq.toArray |> printer.PrintCommaSeparatedArgsArray
            //     printer.Print(sprintf "_curriedApply(%A)" typ)
            | Emit(info, typ, range) ->
                //printer.Print(info.Macro)
                printer.AddError("Emit macros not supported")
            | DecisionTree(expr, targets) -> printer.Print("todo decision tree")
            | DecisionTreeSuccess(targetIndex, boundValues, typ) -> printer.Print("todo decision tree success")
            | Let(ident, value, body) ->
                printer.Print(ident.Name)
                // printer.Print(value)
                printer.Print(" = ")
                printer.Print(value)
                printer.PrintNewLine()
                //printer.Print("(")
                printer.Print(body)
                //printer.Print(")")
                //printer.Print(sprintf "let %A %A %A" ident value body)
            | LetRec(bindings, body) -> printer.Print("todo let rec")
            | Get(expr, kind, typ, range) ->
                printer.Print(expr)
                //printer.Print("todo get")
            | Set(expr, kind, typ, value, range) ->
                printer.Print(expr)
                printer.Print(" = ")
                printer.Print(value)
            | Sequential(exprs) ->
                match exprs |> List.rev with
                | h::t ->
                    for e in t |> List.rev do
                        printer.PrintNewLine()
                        printer.Print(e)
                    printer.PrintNewLine()
                    printer.Print("return ")
                    printer.Print(h)
                    printer.PrintNewLine()
                | _ -> ()
            | WhileLoop(guard, body, label, range) -> printer.Print("todo while")
            | ForLoop(ident, start, limit, body, isUp, range) -> printer.Print("todo for")
            | TryCatch(body, catch, finalizer, range) ->
                printer.PrintSelfExecutingFnOpen()
                printer.Print("local status, retval = pcall(function()")
                printer.PushIndentation()
                printer.PrintNewLine()
                printer.Print body
                printer.PopIndentation()
                //if !status then catch else return retval
                printer.Print("return retval")
                printer.PrintNewLine()
                printer.Print("end)")
                printer.PrintSelfExecutingFnClose()
            | IfThenElse(guardExpr, thenExpr, elseExpr, range) ->
                printer.PrintSelfExecutingFnOpen()
                printer.Print "if "
                printer.Print guardExpr
                printer.Print " then"
                printer.PushIndentation()
                printer.PrintNewLine()
                printer.Print("return ")
                printer.Print thenExpr
                printer.PopIndentation()
                printer.PrintNewLine()
                printer.Print "else "
                printer.PushIndentation()
                printer.PrintNewLine()
                printer.Print("return ")
                printer.Print elseExpr
                printer.PopIndentation()
                printer.PrintSelfExecutingFnClose()
            // | x ->
            //     printer.Print("todo")
                //printer.Print($"TODO: Print expression {x}")
                //printer.AddError("TODO: Print expression")

        member printer.PrintArray(items: 'a array, printItem: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
            for i = 0 to items.Length - 1 do
                printItem printer items.[i]
                if i < items.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedArray(nodes: Ident array) =
            printer.PrintArray(nodes, (fun p x ->
                // p.Print(x.Type)
                // p.Print(" ")
                p.Print(x.Name)
            ), (fun p -> p.Print(", ")))
        member printer.PrintCommaSeparatedArgsArray(nodes: Expr array) =
            printer.PrintArray(nodes, fun p x ->
                //p.Print(sprintf "%A" x)
                p.Print(x)
            , fun p -> p.Print(", "))

        member printer.PrintFunctionDeclaration(name: string, args: Ident list, body: Expr) =
            printer.Print("function ")
            printer.Print("mod.")   //function should be always declared in the local module scope, so they can be imported correctly
            printer.Print(body.Type)
            printer.Print("")
            printer.Print(name)
            printer.Print(" (")
            printer.PrintCommaSeparatedArray(List.toArray args)
            printer.Print(") ")

            printer.PrintBlock([body], skipNewLineAtEnd=false)
            printer.Print("end")
        member printer.PrintSelfExecutingFnOpen() =
            printer.Print("(function ()")
            printer.PushIndentation()
            printer.PrintNewLine()
        member printer.PrintSelfExecutingFnClose() =
            printer.PopIndentation()
            printer.PrintNewLine()
            printer.Print("end)()")

        member printer.Print(md: Declaration) =
            match md with
            | ModuleDeclaration _ -> printer.AddError("Nested modules are not supported")
            | ActionDeclaration _ -> printer.AddError("TODO: Action declaration")
            | ClassDeclaration _ -> printer.AddError("TODO: Class declaration")
            | MemberDeclaration m ->
                printer.PrintFunctionDeclaration(m.Name, m.Args, m.Body)

open PrinterExtensions

let run (writer: Writer) (file: File): Async<unit> =
    let printDeclWithExtraLine extraLine (printer: Printer) (decl: Declaration) =
        printer.Print(decl)

        if extraLine then
            printer.PrintNewLine()

    async {
        use printer = new PrinterImpl(writer)

        // TODO: Imports
        (printer :> Printer).Print("local mod = {}")
        (printer :> Printer).PrintNewLine()
        do! printer.Flush()

        for decl in file.Declarations do
            printDeclWithExtraLine true printer decl
            do! printer.Flush()

        (printer :> Printer).Print("return mod")
        (printer :> Printer).PrintNewLine()
        do! printer.Flush()
    }
