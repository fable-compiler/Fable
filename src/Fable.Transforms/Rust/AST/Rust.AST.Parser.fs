// Source: https://github.com/rust-lang/rust/blob/master/compiler/rustc_ast/src/util/parser.rs

module rec Fable.Transforms.Rust.AST.Parser

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Symbols
open Fable.Transforms.Rust.AST.Types.token

module ast = Fable.Transforms.Rust.AST.Types

let i8 = int8

/// Associative operator with precedence.
///
/// This is the enum which specifies operator precedence and fixity to the parser.
[<RequireQualifiedAccess>]
type AssocOp =
    /// `+`
    | Add
    /// `-`
    | Subtract
    /// `*`
    | Multiply
    /// `/`
    | Divide
    /// `%`
    | Modulus
    /// `&&`
    | LAnd
    /// `||`
    | LOr
    /// `^`
    | BitXor
    /// ``
    | BitAnd
    /// `|`
    | BitOr
    /// `<<`
    | ShiftLeft
    /// `>>`
    | ShiftRight
    /// `==`
    | Equal
    /// `<`
    | Less
    /// `<=`
    | LessEqual
    /// `!=`
    | NotEqual
    /// `>`
    | Greater
    /// `>=`
    | GreaterEqual
    /// `=`
    | Assign
    /// `?=` where ? is one of the BinOpToken
    | AssignOp of BinOpToken
    /// `as`
    | As
    /// `..` range
    | DotDot
    /// `..=` range
    | DotDotEq
    /// `:`
    | Colon

//#[derive(PartialEq, Debug)]
[<RequireQualifiedAccess>]
type Fixity =
    /// The operator is left-associative
    | Left
    /// The operator is right-associative
    | Right
    /// The operator is not associative
    | None

type AssocOp with
    // /// Creates a new AssocOP from a token
    // static member from_token(t: Token): Option<AssocOp> =
    //     match t.kind with
    //     | TokenKind.BinOpEq(k) -> Some(AssocOp.AssignOp(k))
    //     | TokenKind.Eq -> Some(AssocOp.Assign)
    //     | TokenKind.BinOp(BinOpToken.Star) -> Some(AssocOp.Multiply)
    //     | TokenKind.BinOp(BinOpToken.Slash) -> Some(AssocOp.Divide)
    //     | TokenKind.BinOp(BinOpToken.Percent) -> Some(AssocOp.Modulus)
    //     | TokenKind.BinOp(BinOpToken.Plus) -> Some(AssocOp.Add)
    //     | TokenKind.BinOp(BinOpToken.Minus) -> Some(AssocOp.Subtract)
    //     | TokenKind.BinOp(BinOpToken.Shl) -> Some(AssocOp.ShiftLeft)
    //     | TokenKind.BinOp(BinOpToken.Shr) -> Some(AssocOp.ShiftRight)
    //     | TokenKind.BinOp(BinOpToken.And) -> Some(AssocOp.BitAnd)
    //     | TokenKind.BinOp(BinOpToken.Caret) -> Some(AssocOp.BitXor)
    //     | TokenKind.BinOp(BinOpToken.Or) -> Some(AssocOp.BitOr)
    //     | TokenKind.Lt -> Some(AssocOp.Less)
    //     | TokenKind.Le -> Some(AssocOp.LessEqual)
    //     | TokenKind.Ge -> Some(AssocOp.GreaterEqual)
    //     | TokenKind.Gt -> Some(AssocOp.Greater)
    //     | TokenKind.EqEq -> Some(AssocOp.Equal)
    //     | TokenKind.Ne -> Some(AssocOp.NotEqual)
    //     | TokenKind.AndAnd -> Some(AssocOp.LAnd)
    //     | TokenKind.OrOr -> Some(AssocOp.LOr)
    //     | TokenKind.DotDot -> Some(AssocOp.DotDot)
    //     | TokenKind.DotDotEq -> Some(AssocOp.DotDotEq)
    //     // DotDotDot is no longer supported, but we need some way to display the error
    //     | TokenKind.DotDotDot -> Some(AssocOp.DotDotEq)
    //     | TokenKind.Colon -> Some(AssocOp.Colon)
    //     // `<-` should probably be `< -`
    //     | TokenKind.LArrow -> Some(AssocOp.Less)
    //     | _ when t.is_keyword(kw.As) -> Some(AssocOp.As)
    //     | _ -> None

    /// Creates a new AssocOp from ast.BinOpKind.
    static member from_ast_binop(op: ast.BinOpKind) =
        match op with
        | ast.BinOpKind.Lt -> AssocOp.Less
        | ast.BinOpKind.Gt -> AssocOp.Greater
        | ast.BinOpKind.Le -> AssocOp.LessEqual
        | ast.BinOpKind.Ge -> AssocOp.GreaterEqual
        | ast.BinOpKind.Eq -> AssocOp.Equal
        | ast.BinOpKind.Ne -> AssocOp.NotEqual
        | ast.BinOpKind.Mul -> AssocOp.Multiply
        | ast.BinOpKind.Div -> AssocOp.Divide
        | ast.BinOpKind.Rem -> AssocOp.Modulus
        | ast.BinOpKind.Add -> AssocOp.Add
        | ast.BinOpKind.Sub -> AssocOp.Subtract
        | ast.BinOpKind.Shl -> AssocOp.ShiftLeft
        | ast.BinOpKind.Shr -> AssocOp.ShiftRight
        | ast.BinOpKind.BitAnd -> AssocOp.BitAnd
        | ast.BinOpKind.BitXor -> AssocOp.BitXor
        | ast.BinOpKind.BitOr -> AssocOp.BitOr
        | ast.BinOpKind.And -> AssocOp.LAnd
        | ast.BinOpKind.Or -> AssocOp.LOr

    /// Gets the precedence of this operator
    member self.precedence() : i8 =
        match self with
        | AssocOp.As
        | AssocOp.Colon -> 14
        | AssocOp.Multiply
        | AssocOp.Divide
        | AssocOp.Modulus -> 13
        | AssocOp.Add
        | AssocOp.Subtract -> 12
        | AssocOp.ShiftLeft
        | AssocOp.ShiftRight -> 11
        | AssocOp.BitAnd -> 10
        | AssocOp.BitXor -> 9
        | AssocOp.BitOr -> 8
        | AssocOp.Less
        | AssocOp.Greater
        | AssocOp.LessEqual
        | AssocOp.GreaterEqual
        | AssocOp.Equal
        | AssocOp.NotEqual -> 7
        | AssocOp.LAnd -> 6
        | AssocOp.LOr -> 5
        | AssocOp.DotDot
        | AssocOp.DotDotEq -> 4
        | AssocOp.Assign
        | AssocOp.AssignOp(_) -> 2
        |> i8

    /// Gets the fixity of this operator
    member self.fixity() : Fixity =
        // NOTE: it is a bug to have an operators that has same precedence but different fixities!
        match self with
        | AssocOp.Assign
        | AssocOp.AssignOp(_) -> Fixity.Right
        | AssocOp.As
        | AssocOp.Multiply
        | AssocOp.Divide
        | AssocOp.Modulus
        | AssocOp.Add
        | AssocOp.Subtract
        | AssocOp.ShiftLeft
        | AssocOp.ShiftRight
        | AssocOp.BitAnd
        | AssocOp.BitXor
        | AssocOp.BitOr
        | AssocOp.Less
        | AssocOp.Greater
        | AssocOp.LessEqual
        | AssocOp.GreaterEqual
        | AssocOp.Equal
        | AssocOp.NotEqual
        | AssocOp.LAnd
        | AssocOp.LOr
        | AssocOp.Colon -> Fixity.Left
        | AssocOp.DotDot
        | AssocOp.DotDotEq -> Fixity.None

    member self.is_comparison() : bool =
        match self with
        | AssocOp.Less
        | AssocOp.Greater
        | AssocOp.LessEqual
        | AssocOp.GreaterEqual
        | AssocOp.Equal
        | AssocOp.NotEqual -> true
        | AssocOp.Assign
        | AssocOp.AssignOp(_)
        | AssocOp.As
        | AssocOp.Multiply
        | AssocOp.Divide
        | AssocOp.Modulus
        | AssocOp.Add
        | AssocOp.Subtract
        | AssocOp.ShiftLeft
        | AssocOp.ShiftRight
        | AssocOp.BitAnd
        | AssocOp.BitXor
        | AssocOp.BitOr
        | AssocOp.LAnd
        | AssocOp.LOr
        | AssocOp.DotDot
        | AssocOp.DotDotEq
        | AssocOp.Colon -> false

    member self.is_assign_like() : bool =
        match self with
        | AssocOp.Assign
        | AssocOp.AssignOp(_) -> true
        | AssocOp.Less
        | AssocOp.Greater
        | AssocOp.LessEqual
        | AssocOp.GreaterEqual
        | AssocOp.Equal
        | AssocOp.NotEqual
        | AssocOp.As
        | AssocOp.Multiply
        | AssocOp.Divide
        | AssocOp.Modulus
        | AssocOp.Add
        | AssocOp.Subtract
        | AssocOp.ShiftLeft
        | AssocOp.ShiftRight
        | AssocOp.BitAnd
        | AssocOp.BitXor
        | AssocOp.BitOr
        | AssocOp.LAnd
        | AssocOp.LOr
        | AssocOp.DotDot
        | AssocOp.DotDotEq
        | AssocOp.Colon -> false

    member self.to_ast_binop() : Option<ast.BinOpKind> =
        match self with
        | AssocOp.Less -> Some(ast.BinOpKind.Lt)
        | AssocOp.Greater -> Some(ast.BinOpKind.Gt)
        | AssocOp.LessEqual -> Some(ast.BinOpKind.Le)
        | AssocOp.GreaterEqual -> Some(ast.BinOpKind.Ge)
        | AssocOp.Equal -> Some(ast.BinOpKind.Eq)
        | AssocOp.NotEqual -> Some(ast.BinOpKind.Ne)
        | AssocOp.Multiply -> Some(ast.BinOpKind.Mul)
        | AssocOp.Divide -> Some(ast.BinOpKind.Div)
        | AssocOp.Modulus -> Some(ast.BinOpKind.Rem)
        | AssocOp.Add -> Some(ast.BinOpKind.Add)
        | AssocOp.Subtract -> Some(ast.BinOpKind.Sub)
        | AssocOp.ShiftLeft -> Some(ast.BinOpKind.Shl)
        | AssocOp.ShiftRight -> Some(ast.BinOpKind.Shr)
        | AssocOp.BitAnd -> Some(ast.BinOpKind.BitAnd)
        | AssocOp.BitXor -> Some(ast.BinOpKind.BitXor)
        | AssocOp.BitOr -> Some(ast.BinOpKind.BitOr)
        | AssocOp.LAnd -> Some(ast.BinOpKind.And)
        | AssocOp.LOr -> Some(ast.BinOpKind.Or)
        | AssocOp.Assign
        | AssocOp.AssignOp(_)
        | AssocOp.As
        | AssocOp.DotDot
        | AssocOp.DotDotEq
        | AssocOp.Colon -> None

    /// This operator could be used to follow a block unambiguously.
    ///
    /// This is used for error recovery at the moment, providing a suggestion to wrap blocks with
    /// parentheses while having a high degree of confidence on the correctness of the suggestion.
    member self.can_continue_expr_unambiguously() : bool =
        match self with
        | AssocOp.BitXor // `{ 42 } ^ 3`
        | AssocOp.Assign // `{ 42 } = { 42 }`
        | AssocOp.Divide // `{ 42 } / 42`
        | AssocOp.Modulus // `{ 42 } % 2`
        | AssocOp.ShiftRight // `{ 42 } >> 2`
        | AssocOp.LessEqual // `{ 42 } <= 3`
        | AssocOp.Greater // `{ 42 } > 3`
        | AssocOp.GreaterEqual // `{ 42 } >= 3`
        | AssocOp.AssignOp(_) // `{ 42 } +=`
        | AssocOp.As // `{ 42 } as usize`
        // AssocOp.Equal        // `{ 42 } = { 42 }`   Accepting these here would regress incorrect
        // AssocOp.NotEqual     // `{ 42 } <> { 42 }`  struct literals parser recovery.
        | AssocOp.Colon -> true // `{ 42 }: usize`
        | _ -> false

let PREC_CLOSURE: i8 = -40y
let PREC_JUMP: i8 = -30y
let PREC_RANGE: i8 = -10y
// The range 2..=14 is reserved for AssocOp binary operator precedences.
let PREC_PREFIX: i8 = 50y
let PREC_POSTFIX: i8 = 60y
let PREC_PAREN: i8 = 99y
let PREC_FORCE_PAREN: i8 = 100y

[<RequireQualifiedAccess>]
type ExprPrecedence =
    | Closure
    | Break
    | Continue
    | Ret
    | Yield

    | Range

    | Binary of ast.BinOpKind

    | Cast
    | Type

    | Assign
    | AssignOp

    | Box
    | AddrOf
    | Let
    | Unary

    | Call
    | MethodCall
    | Field
    | Index
    | Try
    | InlineAsm
    | Mac

    | Array
    | Repeat
    | Tup
    | Lit
    | Path
    | Paren
    | If
    | While
    | ForLoop
    | Loop
    | Match
    | ConstBlock
    | Block
    | TryBlock
    | Struct
    | Async
    | Await
    | Err

type ExprPrecedence with

    member self.order() : i8 =
        match self with
        | ExprPrecedence.Closure -> PREC_CLOSURE

        | ExprPrecedence.Break
        | ExprPrecedence.Continue
        | ExprPrecedence.Ret
        | ExprPrecedence.Yield -> PREC_JUMP

        // `Range` claims to have higher precedence than `Assign`, but `x .. x = x` fails to
        // parse, instead of parsing as `(x .. x) = x`.  Giving `Range` a lower precedence
        // ensures that `pprust` will add parentheses in the right places to get the desired
        // parse.
        | ExprPrecedence.Range -> PREC_RANGE

        // Binop-like expr kinds, handled by `AssocOp`.
        | ExprPrecedence.Binary(op) -> AssocOp.from_ast_binop(op).precedence ()
        | ExprPrecedence.Cast -> AssocOp.As.precedence ()
        | ExprPrecedence.Type -> AssocOp.Colon.precedence ()

        | ExprPrecedence.Assign
        | ExprPrecedence.AssignOp -> AssocOp.Assign.precedence ()

        // Unary, prefix
        | ExprPrecedence.Box
        | ExprPrecedence.AddrOf
        // Here `let pats = expr` has `let pats =` as a "unary" prefix of `expr`.
        // However, this is not exactly right. When `let _ = a` is the LHS of a binop we
        // need parens sometimes. E.g. we can print `(let _ = a) && b` as `let _ = a && b`
        // but we need to print `(let _ = a) < b` as-is with parens.
        | ExprPrecedence.Let
        | ExprPrecedence.Unary -> PREC_PREFIX

        // Unary, postfix
        | ExprPrecedence.Await
        | ExprPrecedence.Call
        | ExprPrecedence.MethodCall
        | ExprPrecedence.Field
        | ExprPrecedence.Index
        | ExprPrecedence.Try
        | ExprPrecedence.InlineAsm
        | ExprPrecedence.Mac -> PREC_POSTFIX

        // Never need parens
        | ExprPrecedence.Array
        | ExprPrecedence.Repeat
        | ExprPrecedence.Tup
        | ExprPrecedence.Lit
        | ExprPrecedence.Path
        | ExprPrecedence.Paren
        | ExprPrecedence.If
        | ExprPrecedence.While
        | ExprPrecedence.ForLoop
        | ExprPrecedence.Loop
        | ExprPrecedence.Match
        | ExprPrecedence.ConstBlock
        | ExprPrecedence.Block
        | ExprPrecedence.TryBlock
        | ExprPrecedence.Async
        | ExprPrecedence.Struct
        | ExprPrecedence.Err -> PREC_PAREN

/// In `let p = e`, operators with precedence `<=` this one requires parenthesis in `e`.
let prec_let_scrutinee_needs_par () : i8 = AssocOp.LAnd.precedence ()

/// Suppose we have `let _ = e` and the `order` of `e`.
/// Is the `order` such that `e` in `let _ = e` needs parenthesis when it is on the RHS?
///
/// Conversely, suppose that we have `(let _ = a) OP b` and `order` is that of `OP`.
/// Can we print this as `let _ = a OP b`?
let needs_par_as_let_scrutinee (order: i8) : bool =
    order <= prec_let_scrutinee_needs_par ()

/// Expressions that syntactically contain an "exterior" struct literal i.e., not surrounded by any
/// parens or other delimiters, e.g., `X { y: 1 }`, `X { y: 1 }.method()`, `foo = X { y: 1 }` and
/// `X { y: 1 } = foo` all do, but `(X { y: 1 }) = foo` does not.
let contains_exterior_struct_lit (value: ast.Expr) : bool =
    match value.kind with
    | ast.ExprKind.Struct (..) -> true

    | ast.ExprKind.Assign(lhs, rhs, _)
    | ast.ExprKind.AssignOp(_, lhs, rhs)
    | ast.ExprKind.Binary(_, lhs, rhs) ->
        // X { y: 1 } + X { y: 2 }
        contains_exterior_struct_lit (lhs) || contains_exterior_struct_lit (rhs)
    | ast.ExprKind.Await(x)
    | ast.ExprKind.Unary(_, x)
    | ast.ExprKind.Cast(x, _)
    | ast.ExprKind.Type(x, _)
    | ast.ExprKind.Field(x, _)
    | ast.ExprKind.Index(x, _) ->
        // X { y: 1 }, X { y: 1 }.y
        contains_exterior_struct_lit (x)

    | ast.ExprKind.MethodCall(_, exprs, _) ->
        // X { y: 1 }.bar(...)
        contains_exterior_struct_lit (exprs[0])

    | _ -> false
