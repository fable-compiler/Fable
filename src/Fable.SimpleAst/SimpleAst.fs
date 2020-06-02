namespace Fable.SimpleAst

type Ident =
    /// Compiled name of identifier, guaranteed to be unique within the file when created by Fable
    abstract CompiledName: string
    /// Original name of identifier as it appears in source code
    abstract DisplayName: string

type Expr =
    interface end

type ConstantKind =
    | NullConst
    | UndefinedConst
    | BooleanConst of bool
    | StringConst of string
    | NumberConst of float

type SimpleExpr =
    | IdentExpr of Ident
    | Import of selector: string * path: string
    | Function of args: Ident list * body: Expr
    | ObjectExpr of (string * Expr) list
    | Apply of Expr * args: Expr list
    | ApplyNew of Expr * args: Expr list
    | Assign of var: Ident * value: Expr * body: Expr
    | GetField of Expr * key: string
    | GetIndex of Expr * indext: int
    | Set of Expr * value: Expr
    | Constant of ConstantKind
    interface Expr

type Declaration =
    | ValueDeclaration of Expr
    | FunctionDeclaration of args: Ident list * body: Expr

type Logger =
    abstract LogWarning: string -> unit
    abstract LogError: string -> unit

type TransformDeclaration =
    abstract TransformDeclaration: logger: Logger * fullDisplayName: string * Declaration -> Declaration
