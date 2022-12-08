namespace Fable.AST

/// Each Position object consists of a line number (1-indexed) and a column number (0-indexed):
type Position =
    { line: int; column: int; }
    static member Empty = { line = 1; column = 0 }

type SourceLocation =
    { start: Position
      ``end``: Position
      /// We added the display name here because it seemed to be used by Babel source map generation
      identifierName: string option }
    static member (+)(r1, r2) =
        { start = r1.start
          ``end`` = r2.start
          identifierName = None }
    static member Empty =
        { start = Position.Empty
          ``end`` = Position.Empty
          identifierName = None }
    override x.ToString() =
        sprintf $"(L%i{x.start.line},%i{x.start.column}-L%i{x.``end``.line},%i{x.``end``.column})"

type NumberKind =
    | Int8
    | UInt8
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    | BigInt
    | NativeInt
    | UNativeInt
    | Float32
    | Float64
    | Decimal

// TODO: Add missing flags https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#advanced_searching_with_flags
type RegexFlag =
    | RegexGlobal | RegexIgnoreCase | RegexMultiline | RegexSticky | RegexUnicode | RegexSingleline

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
