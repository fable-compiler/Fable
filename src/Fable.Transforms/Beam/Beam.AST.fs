namespace rec Fable.AST.Beam

type Atom = | Atom of name: string

type ErlLiteral =
    | Integer of value: int64
    | Float of value: float
    | StringLit of value: string
    | AtomLit of atom: Atom
    | BoolLit of value: bool
    | NilLit

type ErlPattern =
    | PVar of name: string
    | PLiteral of literal: ErlLiteral
    | PTuple of elements: ErlPattern list
    | PList of head: ErlPattern * tail: ErlPattern
    | PWildcard

type ErlExpr =
    | Literal of literal: ErlLiteral
    | Variable of name: string
    | Tuple of elements: ErlExpr list
    | List of elements: ErlExpr list
    | ListCons of head: ErlExpr * tail: ErlExpr
    | Map of entries: (ErlExpr * ErlExpr) list
    | Call of module_: string option * func: string * args: ErlExpr list
    | Apply of func: ErlExpr * args: ErlExpr list
    | Fun of clauses: ErlFunClause list
    | NamedFun of name: string * clauses: ErlFunClause list
    | Case of expr: ErlExpr * clauses: ErlCaseClause list
    | Match of pattern: ErlPattern * expr: ErlExpr
    | Block of exprs: ErlExpr list
    | BinOp of op: string * left: ErlExpr * right: ErlExpr
    | UnaryOp of op: string * operand: ErlExpr
    | TryCatch of body: ErlExpr list * catchVar: string * catchBody: ErlExpr list
    | Emit of template: string * args: ErlExpr list

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
    | ModuleAttr of name: Atom
    | ExportAttr of exports: (Atom * int) list
    | CustomAttr of name: Atom * value: string

type ErlFunctionDef =
    {
        Name: Atom
        Arity: int
        Clauses: ErlFunClause list
    }

type ErlForm =
    | Attribute of attr: ErlAttribute
    | Function of def: ErlFunctionDef
    | Comment of text: string

type ErlModule =
    {
        Name: Atom
        Forms: ErlForm list
    }
