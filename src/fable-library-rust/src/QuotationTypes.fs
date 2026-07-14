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

// Rust-native carrier for Microsoft.FSharp.Reflection.UnionCaseInfo. Fable-Rust
// erases the F# UnionCaseInfo type to this struct (see Fable2Rust.transformType),
// so `uci.Name`/`uci.Tag` on a NewUnionCase binding route to the accessors below.
type FSharpUnionCaseInfo =
    {
        Name: string
        Tag: int
        DeclaringType: string
    }

// Rust-native carrier for System.Reflection.MethodInfo. Fable-Rust erases the F#
// MethodInfo type to this struct (see Fable2Rust.transformType), so `mi.Name` and
// `mi.DeclaringType` on a Call quotation deconstruction route to the accessors in
// Quotation.fs. DeclaringType is stored as the declaring-type fullname string (the
// emitter fills it from declaringEntity.FullName, e.g. "...Collections.ListModule").
type FSharpMethodInfo =
    {
        Name: string
        DeclaringType: string
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
    | ExprNewUnion of typeName: string * tag: int * caseName: string * fields: FSharpExpr[]
    | ExprNewRecord of typeName: string * fieldNames: string[] * values: FSharpExpr[]
    | ExprNewList of head: FSharpExpr * tail: FSharpExpr
    | ExprTupleGet of expr: FSharpExpr * index: int
    | ExprUnionTag of expr: FSharpExpr
    | ExprUnionField of expr: FSharpExpr * fieldIndex: int
    | ExprFieldGet of expr: FSharpExpr * fieldName: string
    | ExprFieldSet of expr: FSharpExpr * fieldName: string * value: FSharpExpr
    | ExprVarSet of target: FSharpExpr * value: FSharpExpr
