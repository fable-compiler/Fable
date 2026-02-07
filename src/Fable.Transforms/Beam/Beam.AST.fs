namespace rec Fable.AST.Beam

type Atom = | Atom of string

type ErlLiteral =
    | Integer of int64
    | Float of float
    | StringLit of string
    | AtomLit of Atom
    | BoolLit of bool
    | NilLit

type ErlPattern =
    | PVar of string
    | PLiteral of ErlLiteral
    | PTuple of ErlPattern list
    | PList of head: ErlPattern * tail: ErlPattern
    | PWildcard

type ErlExpr =
    | Literal of ErlLiteral
    | Variable of string
    | Tuple of ErlExpr list
    | List of ErlExpr list
    | ListCons of head: ErlExpr * tail: ErlExpr
    | Map of entries: (ErlExpr * ErlExpr) list
    | Call of module_: string option * func: string * args: ErlExpr list
    | Apply of func: ErlExpr * args: ErlExpr list
    | Fun of clauses: ErlFunClause list
    | NamedFun of name: string * clauses: ErlFunClause list
    | Case of ErlExpr * ErlCaseClause list
    | Match of ErlPattern * ErlExpr
    | Block of ErlExpr list
    | BinOp of op: string * ErlExpr * ErlExpr
    | UnaryOp of op: string * ErlExpr
    | TryCatch of body: ErlExpr list * catchVar: string * catchBody: ErlExpr list

and ErlCaseClause =
    {
        Pattern: ErlPattern
        Guard: ErlExpr list
        Body: ErlExpr list
    }

and ErlFunClause =
    {
        Patterns: ErlPattern list
        Guard: ErlExpr list
        Body: ErlExpr list
    }

type ErlAttribute =
    | ModuleAttr of Atom
    | ExportAttr of (Atom * int) list
    | CustomAttr of Atom * string

type ErlFunctionDef =
    {
        Name: Atom
        Arity: int
        Clauses: ErlFunClause list
    }

type ErlForm =
    | Attribute of ErlAttribute
    | Function of ErlFunctionDef
    | Comment of string

type ErlModule =
    {
        Name: Atom
        Forms: ErlForm list
    }
