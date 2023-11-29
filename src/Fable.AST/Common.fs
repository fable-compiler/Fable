namespace Fable.AST

open System

/// Each Position object consists of a line number (1-indexed) and a column number (0-indexed):
type Position =
    {
        line: int
        column: int
    }

    static member Empty =
        {
            line = 1
            column = 0
        }

type SourceLocation =
    {
        start: Position
        ``end``: Position
        /// DO NOT USE, use DisplayName instead and Create for instantiation
        identifierName: string option
    }

    member this.DisplayName =
        this.identifierName
        |> Option.bind (fun name ->
            match name.IndexOf(";file:", StringComparison.Ordinal) with
            | -1 -> Some name
            | 0 -> None
            | i -> name.Substring(0, i) |> Some
        )

    member this.File =
        this.identifierName
        |> Option.bind (fun name ->
            match name.IndexOf(";file:", StringComparison.Ordinal) with
            | -1 -> None
            | i -> name.Substring(i + ";file:".Length) |> Some
        )

    static member Create
        (
            start: Position,
            ``end``: Position,
            ?file: string,
            ?displayName: string
        )
        =
        let identifierName =
            match displayName, file with
            | None, None -> None
            | displayName, None -> displayName
            | displayName, Some file ->
                (defaultArg displayName "") + ";file:" + file |> Some

        {
            start = start
            ``end`` = ``end``
            identifierName = identifierName
        }

    static member (+)(r1, r2) =
        SourceLocation.Create(
            start = r1.start,
            ``end`` = r2.``end``,
            ?file = r1.File
        )

    static member Empty =
        SourceLocation.Create(start = Position.Empty, ``end`` = Position.Empty)

    override x.ToString() =
        sprintf
            $"(L%i{x.start.line},%i{x.start.column}-L%i{x.``end``.line},%i{x.``end``.column})"

type NumberKind =
    | Int8
    | UInt8
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    | Int128
    | UInt128
    | BigInt
    | NativeInt
    | UNativeInt
    | Float16
    | Float32
    | Float64
    | Decimal

// TODO: Add missing flags https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#advanced_searching_with_flags
type RegexFlag =
    | RegexGlobal
    | RegexIgnoreCase
    | RegexMultiline
    | RegexSticky
    | RegexUnicode
    | RegexSingleline

// Operators
type UnaryOperator =
    | UnaryMinus
    | UnaryPlus
    | UnaryNot
    | UnaryNotBitwise
    | UnaryAddressOf

type BinaryOperator =
    | BinaryEqual
    | BinaryUnequal
    | BinaryLess
    | BinaryLessOrEqual
    | BinaryGreater
    | BinaryGreaterOrEqual
    | BinaryShiftLeft
    | BinaryShiftRightSignPropagating
    | BinaryShiftRightZeroFill
    | BinaryMinus
    | BinaryPlus
    | BinaryMultiply
    | BinaryDivide
    | BinaryModulus
    | BinaryExponent
    | BinaryOrBitwise
    | BinaryXorBitwise
    | BinaryAndBitwise

type LogicalOperator =
    | LogicalOr
    | LogicalAnd
