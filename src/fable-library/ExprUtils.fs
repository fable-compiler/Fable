module ExprUtils 

open Microsoft.FSharp.Quotations

let deserialize (str : string) =
    let v = Var("a", typeof<int>)
    Expr.Let(v, Expr.Value(1, typeof<int>), Expr.Var v)

let isExpr (o : obj) =
    match o with
    | :? Expr -> true
    | _ -> false