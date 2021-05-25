module Fable.Transforms.Rust.AST.Helpers

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Types

[<AutoOpen>]
module TokenLiterals =

  let mkTokenLit kind symbol: token.Lit =
    { kind = kind
      symbol = symbol
      suffix = None }

  let mkBoolTokenLit symbol: token.Lit =
    mkTokenLit token.LitKind.Bool symbol

  let mkCharTokenLit symbol: token.Lit =
    mkTokenLit token.LitKind.Char symbol

  let mkIntTokenLit symbol: token.Lit =
    mkTokenLit token.LitKind.Integer symbol

  let mkFloatTokenLit symbol: token.Lit =
    mkTokenLit token.LitKind.Float symbol

  let mkStrTokenLit symbol: token.Lit =
    mkTokenLit token.LitKind.Str symbol

  let mkRawStrTokenLit raw symbol: token.Lit =
    mkTokenLit (token.LitKind.StrRaw raw) symbol

[<AutoOpen>]
module Tokens =

  let dummyDelimSpan: token.DelimSpan =
    { open_ = DUMMY_SP
      close = DUMMY_SP }

  let mkToken kind: token.Token =
    { kind = kind
      span = DUMMY_SP }

  let mkLiteralToken kind: token.Token =
    kind
    |> token.TokenKind.Literal
    |> mkToken

  let mkIdentToken symbol: token.Token =
    token.TokenKind.Ident(symbol, false)
    |> mkToken

  let mkRawIdentToken symbol: token.Token =
    token.TokenKind.Ident(symbol, true)
    |> mkToken

  let mkBoolToken symbol: token.Token =
    symbol
    |> mkBoolTokenLit
    |> mkLiteralToken

  let mkCharToken symbol: token.Token =
    symbol
    |> mkCharTokenLit
    |> mkLiteralToken

  let mkIntToken symbol: token.Token =
    symbol
    |> mkIntTokenLit
    |> mkLiteralToken

  let mkFloatToken symbol: token.Token =
    symbol
    |> mkFloatTokenLit
    |> mkLiteralToken

  let mkStrToken symbol: token.Token =
    symbol
    |> mkStrTokenLit
    |> mkLiteralToken

  let mkRawStrToken raw symbol: token.Token =
    symbol
    |> mkRawStrTokenLit raw
    |> mkLiteralToken

[<AutoOpen>]
module TokenTrees =

  let mkIdentTokenTreeToken symbol: token.TokenTree =
    symbol
    |> mkIdentToken
    |> token.TokenTree.Token

  let mkRawIdentTokenTreeToken symbol: token.TokenTree =
    symbol
    |> mkRawIdentToken
    |> token.TokenTree.Token

  let mkBoolTokenTreeToken symbol: token.TokenTree =
    symbol
    |> mkBoolToken
    |> token.TokenTree.Token

  let mkCharTokenTreeToken symbol: token.TokenTree =
    symbol
    |> mkCharToken
    |> token.TokenTree.Token

  let mkIntTokenTreeToken symbol: token.TokenTree =
    symbol
    |> mkIntToken
    |> token.TokenTree.Token

  let mkFloatTokenTreeToken symbol: token.TokenTree =
    symbol
    |> mkFloatToken
    |> token.TokenTree.Token

  let mkStrTokenTreeToken symbol: token.TokenTree =
    symbol
    |> mkStrToken
    |> token.TokenTree.Token

  let mkRawStrTokenTreeToken raw symbol: token.TokenTree =
    symbol
    |> mkRawStrToken raw
    |> token.TokenTree.Token

  let commaTokenTreeToken: token.TokenTree =
    token.TokenKind.Comma
    |> mkToken
    |> token.TokenTree.Token

[<AutoOpen>]
module Literals =

  let mkBoolLit (value: bool): Lit =
    { token = mkBoolTokenLit (string value)
      kind = LitKind.Bool(value)
      span = DUMMY_SP }

  let mkCharLit (value: char): Lit =
    { token = mkCharTokenLit (string value)
      kind = LitKind.Char(value)
      span = DUMMY_SP }

  let mkIntLit (value: u128): Lit =
    { token = mkIntTokenLit (string value)
      kind = LitKind.Int(value, LitIntType.Unsuffixed)
      span = DUMMY_SP }

  let mkFloatLit (value: Symbol): Lit =
    { token = mkFloatTokenLit value
      kind = LitKind.Float(value, LitFloatType.Unsuffixed)
      span = DUMMY_SP }

  let mkStrLit (value: Symbol): Lit =
    { token = mkStrTokenLit value
      kind = LitKind.Str(value, StrStyle.Cooked)
      span = DUMMY_SP }

  let mkRawStrLit raw (value: Symbol): Lit =
    { token = mkRawStrTokenLit raw value
      kind = LitKind.Str(value, StrStyle.Raw raw)
      span = DUMMY_SP }

[<AutoOpen>]
module Paths =

  let mkGenericPathSegment ident args: PathSegment =
    { ident = ident
      id = DUMMY_NODE_ID
      args = args }

  let mkPathSegment ident: PathSegment =
    mkGenericPathSegment ident None

  let mkPathFromIdents (idents: Vec<Ident>): Path =
    { span = DUMMY_SP
      segments = idents.map(mkPathSegment)
      tokens = None }

  let mkPathFromSymbols (symbols: Vec<Symbol>) =
    symbols.map(Ident.from_str)
    |> mkPathFromIdents

[<AutoOpen>]
module Patterns =

  let mkPat str mut: Pat =
    { id = DUMMY_NODE_ID
      kind =
        PatKind.Ident(
          BindingMode.ByValue(mut),
          Ident.from_str(str),
          None
        )
      span = DUMMY_SP
      tokens = None }

  let mkPatMut str = mkPat str Mutability.Mut
  let mkPatNot str = mkPat str Mutability.Not

[<AutoOpen>]
module Visibilities =

  let mkVisibility kind: Visibility =
    { kind = kind
      span = DUMMY_SP
      tokens = None }

  let PUBLIC_VIS =
    VisibilityKind.Public |> mkVisibility

  let INHERITED_VIS =
    VisibilityKind.Inherited |> mkVisibility

[<AutoOpen>]
module Exprs =

  let mkExpr kind: Expr =
    { id = DUMMY_NODE_ID
      kind = kind
      span = DUMMY_SP
      attrs = Vec []
      tokens = None }

  let mkBoolLitExpr value: Expr =
    value
    |> mkBoolLit
    |> ExprKind.Lit
    |> mkExpr

  let mkCharLitExpr value: Expr =
    value
    |> mkCharLit
    |> ExprKind.Lit
    |> mkExpr

  let mkIntLitExpr value: Expr =
    value
    |> mkIntLit
    |> ExprKind.Lit
    |> mkExpr

  let mkFloatLitExpr value: Expr =
    value
    |> mkFloatLit
    |> ExprKind.Lit
    |> mkExpr

  let mkStrLitExpr value: Expr =
    value
    |> mkStrLit
    |> ExprKind.Lit
    |> mkExpr

  let mkRawStrLitExpr raw value: Expr =
    value
    |> mkRawStrLit raw
    |> ExprKind.Lit
    |> mkExpr

  let mkPathExpr (symbols: Vec<Symbol>): Expr =
    ExprKind.Path(None, mkPathFromSymbols symbols)
    |> mkExpr

  let mkQualifiedPathExpr (qualified: Option<QSelf>) (symbols: Vec<Symbol>): Expr =
    ExprKind.Path(qualified, mkPathFromSymbols symbols)
    |> mkExpr

  let mkStructExpr path fields rest =
    { path = path
      fields = fields
      rest = rest }
    |> ExprKind.Struct
    |> mkExpr

  let mkArrayExpr (elements: seq<Expr>) =
    ExprKind.Array(Vec(elements))
    |> mkExpr

  let mkTupleExpr (elements: seq<Expr>) =
    ExprKind.Tup(Vec(elements))
    |> mkExpr

  let mkUnaryExpr op arg =
    ExprKind.Unary(op, arg)
    |> mkExpr

  let mkBinaryExpr op left right =
    ExprKind.Binary(op, left, right)
    |> mkExpr

  let mkAssignOpExpr op left right =
    ExprKind.AssignOp(op, left, right)
    |> mkExpr

  let mkCallExpr callee args =
    ExprKind.Call(callee, args)
    |> mkExpr

  let mkMethodCallExpr callee args =
    ExprKind.MethodCall(callee, args, DUMMY_SP)
    |> mkExpr

  let TODO_EXPR name =
    mkStrLit ("TODO_" + name)
    |> ExprKind.Lit |> mkExpr

[<AutoOpen>]
module Stmts =

  let mkStmt kind: Stmt =
    { id = DUMMY_NODE_ID
      kind = kind
      span = DUMMY_SP }

  let EMPTY_STMT = StmtKind.Empty |> mkStmt

[<AutoOpen>]
module BinOps =

  let mkBinOp kind =
    respan(DUMMY_SP, kind)

[<AutoOpen>]
module Items =

  let mkItem kind ident: Item =
    { attrs = Vec []
      id = DUMMY_NODE_ID
      span = DUMMY_SP
      vis = INHERITED_VIS
      ident = ident
      kind = kind
      tokens = None }

  let TODO_ITEM name: Item =
    Ident.from_str ("TODO_" + name)
    |> mkItem (ItemKind.ExternCrate None)
