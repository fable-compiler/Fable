namespace Fable.SimpleAst

type Ident =
    /// Compiled name of identifier, guaranteed to be unique within the file when created by Fable
    abstract CompiledName: string
    /// Original name of identifier as it appears in source code
    abstract DisplayName: string
    abstract IsFunction: bool

type Expr =
    interface end

type SimpleConstant =
    | NullConst
    | UndefinedConst
    | BooleanConst of bool
    | StringConst of string
    | NumberConst of float
    | ArrayConst of Expr list
    | RegexConst of source: string * flags: char list

type SimpleExpr =
    | Constant of SimpleConstant
    | IdentExpr of Ident
    | Import of selector: string * path: string
    | Function of args: Ident list * body: Expr
    | ObjectExpr of (string * Expr) list
    | Apply of Expr * args: Expr list
    | ApplyNew of Expr * args: Expr list
    | Let of var: Ident * value: Expr * body: Expr
    | GetField of Expr * key: string
    | GetIndex of Expr * indext: int
    | Set of Expr * value: Expr
    | Sequential of Expr list
    | EmitJs of macro: string * args: Expr list
//    | WhileLoop of guard: Expr * body: Expr
//    | ForOfLoop of ident: Ident * iterable: Expr * body: Expr
//    | TryCatch of body: Expr * catch: (Ident * Expr) option * finalizer: Expr option
//    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr
    interface Expr

type Declaration =
    abstract CompiledName: string
    abstract DisplayName: string
    /// Includes the full name of enclosing namespace/module
    /// in a format compatible with JS variable declaration
    abstract FullDisplayName: string
    abstract Args: (Ident list) option
    abstract Body: Expr

type Helper =
    abstract LogWarning: string -> unit
    abstract LogError: string -> unit
    abstract GetUniqueVar: name: string -> string

type TransformDeclaration =
    abstract TransformDeclaration: logger: Helper * Declaration -> Declaration
