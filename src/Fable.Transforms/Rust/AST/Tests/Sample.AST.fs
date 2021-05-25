module Sample.AST

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Types
open Fable.Transforms.Rust.AST.Helpers

let test_crate: Crate = {
  attrs = Vec []
  items = Vec [
    { // Item
      attrs = Vec []
      id = DUMMY_NODE_ID
      span = DUMMY_SP
      vis = INHERITED_VIS
      ident = Ident.from_str("main")
      kind =
        ItemKind.Fn(
          ( //FnKind
            Defaultness.Final,
            { // FnSig
              header = { // FnHeader
                unsafety = Unsafety.No
                asyncness = Asyncness.No
                constness = Constness.No
                ext = Extern.None
              }
              decl = { // FnDecl
                inputs = Vec []
                output = FnRetTy.Default(DUMMY_SP)
              }
              span = DUMMY_SP
            },
            { // Generics
              params_ = Vec []
              where_clause = { // WhereClause
                has_where_token = false
                predicates = Vec []
                span = DUMMY_SP
              }
              span = DUMMY_SP
            },
            Some(
              { // Block
                stmts = Vec [
                  { // Stmt
                    id = DUMMY_NODE_ID
                    kind =
                      StmtKind.Local(
                        { // Local
                          id = DUMMY_NODE_ID
                          pat = mkPatNot "a"
                          ty = None
                          init =
                            Some(
                              { // Expr
                                id = DUMMY_NODE_ID
                                kind =
                                  ExprKind.MacCall(
                                    { // MacCall
                                      path = mkPathFromSymbols(Vec ["vec"])
                                      args =
                                        MacArgs.Delimited(
                                          dummyDelimSpan,
                                          MacDelimiter.Bracket,
                                          token.TokenStream(
                                            Vec [
                                              (mkIntTokenTreeToken("1"), token.Spacing.Joint)
                                              (commaTokenTreeToken, token.Spacing.Alone)
                                              (mkIntTokenTreeToken("2"), token.Spacing.Joint)
                                              (commaTokenTreeToken, token.Spacing.Alone)
                                              (mkIntTokenTreeToken("3"), token.Spacing.Joint)
                                              (commaTokenTreeToken, token.Spacing.Alone)
                                              (mkIntTokenTreeToken("4"), token.Spacing.Joint)
                                              (commaTokenTreeToken, token.Spacing.Alone)
                                              (mkIntTokenTreeToken("5"), token.Spacing.Alone)
                                            ]
                                          )
                                        )
                                      prior_type_ascription = None
                                    }
                                  )
                                span = DUMMY_SP
                                attrs = Vec []
                                tokens = None
                              }
                            )
                          span = DUMMY_SP
                          attrs = Vec []
                          tokens = None
                        }
                      )
                    span = DUMMY_SP
                  }
                  { // Stmt
                    id = DUMMY_NODE_ID
                    kind =
                      StmtKind.MacCall(
                        { // MacCallStmt
                          mac = { // MacCall
                            path = mkPathFromSymbols(Vec ["println"])
                            args =
                              MacArgs.Delimited(
                                dummyDelimSpan,
                                MacDelimiter.Parenthesis,
                                token.TokenStream(
                                  Vec [
                                    (mkStrTokenTreeToken("{:?}"), token.Spacing.Joint)
                                    (commaTokenTreeToken, token.Spacing.Alone)
                                    (mkIdentTokenTreeToken("a"), token.Spacing.Alone)
                                  ]
                                )
                              )
                            prior_type_ascription = None
                          }
                          style = MacStmtStyle.Semicolon
                          attrs = Vec []
                          tokens = None
                        }
                      )
                    span = DUMMY_SP
                  }
                ]
                id = DUMMY_NODE_ID
                rules = BlockCheckMode.Default
                span = DUMMY_SP
                tokens = None
              }
            )
          )
        )
      tokens = None
    }
  ]
  span = DUMMY_SP
  proc_macros = Vec []
}