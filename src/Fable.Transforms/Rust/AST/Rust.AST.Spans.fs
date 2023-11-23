// Source: https://github.com/rust-lang/rust/blob/master/compiler/rustc_span/src/

module rec Fable.Transforms.Rust.AST.Spans

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Symbols

type NodeId = u32
type BytePos = u32
type SyntaxContext = u32

let DUMMY_NODE_ID: NodeId = 0u

let DUMMY_SP: Span =
    {
        base_or_index = 0u
        len_or_tag = 0us
        ctxt_or_zero = 0us
    }

let respan<'T> (sp: Span, t: 'T) : Spanned<'T> =
    {
        node = t
        span = sp
    }

type SpanData =
    {
        lo: BytePos
        hi: BytePos
        /// Information about where the macro came from, if this piece of
        /// code was created by a macro expansion.
        ctxt: SyntaxContext
    }

type Span =
    {
        base_or_index: u32
        len_or_tag: u16
        ctxt_or_zero: u16
    }

type Spanned<'T> =
    {
        node: 'T
        span: Span
    }

type Ident =
    {
        name: Symbol
        span: Span
    }

type Ident with

    /// Constructs a new identifier from a symbol and a span.
    static member new_(name: Symbol, span: Span) : Ident =
        {
            name = name
            span = span
        }

    /// Constructs a new identifier with a dummy span.
    static member with_dummy_span(name: Symbol) : Ident =
        Ident.new_ (name, DUMMY_SP)

    static member invalid() : Ident = Ident.with_dummy_span (kw.Empty)

    /// Maps a string to an identifier with a dummy span.
    static member from_str(str: string) : Ident = Ident.with_dummy_span (str)

    /// Maps a string and a span to an identifier.
    static member from_str_and_span(str: string, span: Span) : Ident =
        Ident.new_ (str, span)
