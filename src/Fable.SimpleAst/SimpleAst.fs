namespace Fable.SimpleAst

type Ident =
    abstract Name: string
    abstract DisplayName: string

type Expr =
    interface end

type SimpleExpr =
    | IdentExpr of Ident
    | Import of selector: string * path: string
    | Function of args: Ident list * body: Expr
    | ObjectExpr of (string * Expr) list
    | Apply of Expr * args: Expr list
    | Let of var: Ident * value: Expr * body: Expr
    | Get of Expr * key: string
    | Set of Expr * value: Expr
    interface Expr

type Declaration =
    | ValueDeclaration of Expr
    | FunctionDeclaration of args: Ident list * body: Expr

type TransformDeclaration =
    abstract TransformDeclaration: Declaration -> Declaration
