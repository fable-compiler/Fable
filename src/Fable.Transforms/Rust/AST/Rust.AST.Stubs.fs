// Source: https://github.com/rust-lang/rust/blob/master/compiler/rustc_ast/src/ast.rs

module rec Fable.Transforms.Rust.AST.Stubs

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Symbols
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Types

type Span with

    member self.hi() = 0u // TODO:
    member self.lo() = 0u // TODO:
    member self.to_(sp: Span) = sp // TODO:

type token.DelimSpan with

    member self.entire() = DUMMY_SP // TODO:

type IdentPrinter =
    static member new_
        (
            symbol: Symbol,
            is_raw: bool,
            convert_dollar_crate: Option<Span>
        )
        =
        symbol.as_str () // TODO:

    static member for_ast_ident(ident: Ident, is_raw: bool) =
        ident.name.as_str () // TODO:

type Ident with

    member self.to_string() : string = self.name.as_str ()
    member self.is_raw_guess() = false // TODO:
