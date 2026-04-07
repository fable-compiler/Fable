namespace Fable.Core.Quotations

/// Serializable variable in a quotation expression.
/// Matches the JSON shape produced by Fable's quotation runtime.
type QuotVar =
    {
        Name: string
        Type: string
        IsMutable: bool
    }

/// Serializable quotation expression tree.
/// Designed for cross-platform JSON deserialization (e.g. via Thoth.Json)
/// of quotations constructed on Fable JS/TS clients.
///
/// JSON format: ["Tag", ...fields] arrays, e.g. ["Value", 42, "int32"]
[<RequireQualifiedAccess>]
type QuotExpr =
    | Value of value: obj * typeName: string
    | Var of var: QuotVar
    | Lambda of var: QuotVar * body: QuotExpr
    | Application of func: QuotExpr * arg: QuotExpr
    | Let of var: QuotVar * value: QuotExpr * body: QuotExpr
    | IfThenElse of guard: QuotExpr * thenExpr: QuotExpr * elseExpr: QuotExpr
    | Call of instance: QuotExpr option * methodName: string * args: QuotExpr array
    | Sequential of first: QuotExpr * second: QuotExpr
    | NewTuple of elements: QuotExpr array
    | TupleGet of tuple: QuotExpr * index: int
    | NewUnion of typeName: string * tag: int * fields: QuotExpr array
    | UnionTag of expr: QuotExpr
    | UnionField of expr: QuotExpr * index: int
    | NewRecord of fieldNames: string array * values: QuotExpr array
    | FieldGet of expr: QuotExpr * fieldName: string
    | FieldSet of expr: QuotExpr * fieldName: string * value: QuotExpr
    | VarSet of target: QuotExpr * value: QuotExpr
    | NewList of head: QuotExpr * tail: QuotExpr
