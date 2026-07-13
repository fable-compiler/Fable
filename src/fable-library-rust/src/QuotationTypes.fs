// F# quotation AST types for the Fable Rust target.
//
// Declared in the Microsoft.FSharp.Quotations namespace with the *compiled* names
// (FSharpExpr / FSharpVar) that Fable maps the real FSharp.Core Quotations.Expr / .Var
// to on the Rust target. Using the compiled names (rather than Expr/Var) means no clash
// with FSharp.Core during F# type-checking, while still resolving to the same Rust path
// (Microsoft::FSharp::Quotations::FSharpExpr) that generated user code references.
namespace Microsoft.FSharp.Quotations

type FSharpVar =
    {
        Name: string
        Type: string
        IsMutable: bool
    }

type FSharpExpr =
    | ExprValue of value: obj * typ: string
    | ExprVarExpr of var: FSharpVar
    | ExprLambda of var: FSharpVar * body: FSharpExpr
    | ExprApplication of func: FSharpExpr * arg: FSharpExpr
    | ExprLet of var: FSharpVar * value: FSharpExpr * body: FSharpExpr
    | ExprIfThenElse of guard: FSharpExpr * thenExpr: FSharpExpr * elseExpr: FSharpExpr
    | ExprCall of instance: FSharpExpr * method: string * args: FSharpExpr[] * declaringType: string
    | ExprSequential of first: FSharpExpr * second: FSharpExpr
    | ExprNewTuple of elements: FSharpExpr[]
    | ExprNewUnion of typeName: string * tag: int * fields: FSharpExpr[]
    | ExprNewRecord of fieldNames: string[] * values: FSharpExpr[]
    | ExprNewList of head: FSharpExpr * tail: FSharpExpr
    | ExprTupleGet of expr: FSharpExpr * index: int
    | ExprUnionTag of expr: FSharpExpr
    | ExprUnionField of expr: FSharpExpr * fieldIndex: int
    | ExprFieldGet of expr: FSharpExpr * fieldName: string
    | ExprFieldSet of expr: FSharpExpr * fieldName: string * value: FSharpExpr
    | ExprVarSet of target: FSharpExpr * value: FSharpExpr
