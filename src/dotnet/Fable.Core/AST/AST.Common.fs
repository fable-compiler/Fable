namespace Fable.AST

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
