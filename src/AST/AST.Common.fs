namespace Fabel.AST

// Utility
type EraseAttribute() = inherit System.Attribute()
[<Erase>] type U2<'a, 'b> = Case1 of 'a | Case2 of 'b
[<Erase>] type U3<'a, 'b, 'c> = Case1 of 'a | Case2 of 'b | Case3 of 'c

type IdentForbiddenChars =
    static member Regex = System.Text.RegularExpressions.Regex "^[^a-zA-Z_]|[^0-9a-zA-Z_]"

/// Each Position object consists of a line number (1-indexed) and a column number (0-indexed):
type Position =
    { line: int; column: int; }

type SourceLocation =
    { source: string option; start: Position; ``end``: Position; }

type NumberKind =
    | Int8 | UInt8 | UInt8Clamped | Int16 | UInt16 | Int32 | UInt32 | Float32 | Float64

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
    | BinaryMore
    | BinaryMoreOrEqual
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
