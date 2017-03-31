namespace Fable.AST

/// Each Position object consists of a line number (1-indexed) and a column number (0-indexed):
type Position =
    { line: int; column: int; }
    static member Empty = { line = 1; column = 0 }

type SourceLocation =
    { (*source: string option;*) start: Position; ``end``: Position; }
    member x.Collapse() =
        { start = x.start; ``end`` = x.start }
    static member (+) (r1: SourceLocation, r2: SourceLocation) =
        { start = r1.start; ``end`` = r2.``end`` }
    static member Empty =
        { start = Position.Empty; ``end`` = Position.Empty }
    override x.ToString() =
        sprintf "(L%i,%i-L%i,%i)"
            x.start.line x.start.column
            x.``end``.line x.``end``.column

type FableError(msg: string, ?range: SourceLocation, ?file: string) =
    inherit System.Exception(msg)
    member __.Range = range
    member __.File = file
    member __.FormattedMessage =
        match file with
        | Some file ->
            let range =
                match range with
                | Some r -> sprintf "%i, %i" r.start.line r.start.column
                | None -> "1"
            sprintf "%s(%s) : error FABLE: %s" file range msg
        | None -> msg

type NumberKind =
    | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Float32 | Float64

/// Numbers that are not represented with JS native number type
type ExtendedNumberKind =
    | Int64 | UInt64 | BigInt

type RegexFlag =
    | RegexGlobal | RegexIgnoreCase | RegexMultiline | RegexSticky

(** ##Operators *)
type UnaryOperator =
    | UnaryMinus
    | UnaryPlus
    | UnaryNot
    | UnaryNotBitwise
    | UnaryTypeof
    | UnaryVoid
    | UnaryDelete

type UpdateOperator =
    | UpdateMinus
    | UpdatePlus

type BinaryOperator =
    | BinaryEqual
    | BinaryUnequal
    | BinaryEqualStrict
    | BinaryUnequalStrict
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
    | BinaryIn
    | BinaryInstanceOf

type LogicalOperator =
    | LogicalOr
    | LogicalAnd

type AssignmentOperator =
    | AssignEqual
    | AssignMinus
    | AssignPlus
    | AssignMultiply
    | AssignDivide
    | AssignModulus
    | AssignShiftLeft
    | AssignShiftRightSignPropagating
    | AssignShiftRightZeroFill
    | AssignOrBitwise
    | AssignXorBitwise
    | AssignAndBitwise
