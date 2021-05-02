module rec Fable.Transforms.Rust.AST.Helpers

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Types

[<AutoOpen>]
module Tokens =

  let mkToken kind: token.Token =
    { kind = kind
      span = DUMMY_SP }

  let mkTokenLit kind symbol: token.Lit =
    { kind = kind
      symbol = symbol
      suffix = None }

  let dummyDelimSpan: token.DelimSpan =
    { open_ = DUMMY_SP
      close = DUMMY_SP }

  let mkIdentToken symbol: token.Token =
    token.TokenKind.Ident(symbol, false)
    |> mkToken

  let mkIdentTokenRaw symbol: token.Token =
    token.TokenKind.Ident(symbol, true)
    |> mkToken

  let mkIntToken symbol: token.Token =
    symbol
    |> mkTokenLit token.LitKind.Integer
    |> token.TokenKind.Literal
    |> mkToken

  let mkStrToken symbol: token.Token =
    symbol
    |> mkTokenLit token.LitKind.Str
    |> token.TokenKind.Literal
    |> mkToken

  let mkStrTokenRaw raw symbol: token.Token =
    symbol
    |> mkTokenLit (token.LitKind.StrRaw raw)
    |> token.TokenKind.Literal
    |> mkToken

  let mkIdentTokenTreeToken symbol: token.TokenTree =
    symbol
    |> mkIdentToken
    |> token.TokenTree.Token

  let mkIdentTokenTreeTokenRaw symbol: token.TokenTree =
    symbol
    |> mkIdentTokenRaw
    |> token.TokenTree.Token

  let mkIntTokenTreeToken symbol: token.TokenTree =
    symbol
    |> mkIntToken
    |> token.TokenTree.Token

  let mkStrTokenTreeToken symbol: token.TokenTree =
    symbol
    |> mkStrToken
    |> token.TokenTree.Token

  let mkStrTokenTreeTokenRaw raw symbol: token.TokenTree =
    symbol
    |> mkStrTokenRaw raw
    |> token.TokenTree.Token

  let commaTokenTreeToken: token.TokenTree =
    token.TokenKind.Comma
    |> mkToken
    |> token.TokenTree.Token

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

let mkVis kind: Visibility =
  { kind = kind
    span = DUMMY_SP
    tokens = None }

let visibilityInherited = VisibilityKind.Inherited |> mkVis

let mkPathSegment ident: PathSegment =
  { ident = ident
    id = DUMMY_NODE_ID
    args = None }

let mkPathFromIdents (idents: Vec<Ident>): Path =
  { span = DUMMY_SP
    segments = idents.map(mkPathSegment)
    tokens = None }

let mkPath (symbols: Vec<Symbol>) =
  symbols.map(Ident.from_str)
  |> mkPathFromIdents

let mkExpr kind: Expr =
  { id = DUMMY_NODE_ID
    kind = kind
    span = DUMMY_SP
    attrs = Vec []
    tokens = None }

let mkStmt kind: Stmt =
  { id = DUMMY_NODE_ID
    kind = kind
    span = DUMMY_SP }

let mkStrLit symbol: Lit =
  { token = symbol |> mkTokenLit token.LitKind.Str
    kind = LitKind.Str(symbol, StrStyle.Cooked)
    span = DUMMY_SP }

let mkStrLitRaw raw symbol: Lit =
  { token = symbol |> mkTokenLit (token.LitKind.StrRaw raw)
    kind = LitKind.Str(symbol, StrStyle.Raw raw)
    span = DUMMY_SP }

let mkStrExpr symbol: Expr =
  symbol
  |> mkStrLit
  |> ExprKind.Lit
  |> mkExpr

let emptyStmt = StmtKind.Empty |> mkStmt
