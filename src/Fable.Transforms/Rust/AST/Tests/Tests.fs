module Tests

// open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Types
open Fable.Transforms.Rust.AST.Impl
open Fable.Transforms.Rust.AST.State
open type Macros

[<AutoOpen>]
module Helpers =

    let fun_to_string  (decl: FnDecl,
                        header: FnHeader,
                        name: Ident,
                        generics: Generics): string =
        State.new_().to_string(fun (s) ->
            s.head("")
            s.print_fn(decl, header, Some(name), generics)
            s.s.end_() // Close the head box.
            s.s.end_() // Close the outer box.
        )

    let variant_to_string(var: Variant): string =
        State.new_().to_string(fun (s) -> s.print_variant(var))

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member _.test_fun_to_string() =
        let abba_ident = Ident.from_str("abba")
        let decl: FnDecl = { inputs = Vec(); output = FnRetTy.Default(DUMMY_SP) }
        let generics = Generics.default_()
        assert_eq(
            fun_to_string(decl, FnHeader.default_(), abba_ident, generics),
            "fn abba()"
        )

    [<TestMethod>]
    member _.test_variant_to_string() =
        let ident = Ident.from_str("principal_skinner")

        let var_: Variant = {
            ident = ident
            vis = {
                span = DUMMY_SP
                kind = VisibilityKind.Inherited
                tokens = None
            }
            attrs = Vec()
            id = DUMMY_NODE_ID
            data = VariantData.Unit(DUMMY_NODE_ID)
            disr_expr = None
            span = DUMMY_SP
            is_placeholder = false
        }

        let varstr = variant_to_string(var_)
        assert_eq(varstr, "principal_skinner")

    [<TestMethod>]
    member _.test_crate_to_string() =
        let sm: SourceMap = SourceMap()
        let krate: Crate = Sample.AST.testCrate
        let filename: FileName = "filename.rs"
        let input: string = ""
        let ann: PpAnn = NoAnn() :> PpAnn
        let is_expanded: bool = false
        let edition: Edition = Edition.Edition2021

        let actual = print_crate(sm, krate, filename, input, ann, is_expanded, edition)
        let expected = "pub fn main() { let a = vec![1, 2, 3, 4, 5]; println!(\"{:?}\", a); }\n"
        assert_eq(actual, expected)

let run() =
    let tests = TestClass()
    tests.test_fun_to_string()
    tests.test_variant_to_string()
    tests.test_crate_to_string()
