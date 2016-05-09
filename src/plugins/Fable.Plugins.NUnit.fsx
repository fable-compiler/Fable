namespace Fable.Plugins

#r "../../build/fable/bin/Fable.exe"

open Fable
open Fable.AST
open Fable.FSharp2Fable
open Fable.Fable2Babel

module Util =
    let (|TestFixture|_|) (decl: Fable.Declaration) =
        match decl with
        | Fable.EntityDeclaration (ent, entDecls, entRange) ->
            match ent.TryGetDecorator "TestFixture" with
            | Some _ -> Some (ent, entDecls, entRange)
            | None -> None
        | _ -> None

    let methodDecorators = Map.ofList ["TestFixtureSetUp", "before";
                                       "SetUp", "beforeEach";
                                       "Test", "it";
                                       "TearDown", "afterEach";
                                       "TestFixtureTearDown", "after"]

    let (|Test|_|) (decl: Fable.Declaration) =
        match decl with
        | Fable.MemberDeclaration m ->
            match m.Kind, (m.Decorators |> List.tryFind (fun x -> Map.containsKey x.Name methodDecorators)) with
            | Fable.Method name, Some decorator -> Some (m, name, decorator)
            | _ -> None
        | _ -> None

    // Compile tests using Mocha.js BDD interface
    let transformTest com ctx (test: Fable.Member) name (decorator: Fable.Decorator) =
        if test.Arguments.Length > 0 then
            failwithf "Test parameters are not supported (testName = '%s')." name
        let testName =
            Babel.StringLiteral name :> Babel.Expression
        let testBody =
            Util.funcExpression com ctx test.Arguments test.Body :> Babel.Expression
        let testRange =
            match testBody.loc with
            | Some loc -> test.Range + loc | None -> test.Range
        let newMethodName = methodDecorators.Item(decorator.Name)
        // it('Test name', function() { /* Tests */ });
        Babel.ExpressionStatement(
            Babel.CallExpression(Babel.Identifier newMethodName,
                [U2.Case1 testName; U2.Case1 testBody], testRange), testRange)
        :> Babel.Statement

    let transformTestFixture (fixture: Fable.Entity) testRange testDecls =
        let testDesc =
            Babel.StringLiteral fixture.Name :> Babel.Expression
        let testBody =
            Babel.FunctionExpression([],
                Babel.BlockStatement (testDecls, ?loc=Some testRange), ?loc=Some testRange)
            :> Babel.Expression
        Babel.ExpressionStatement(
            Babel.CallExpression(Babel.Identifier "describe",
                [U2.Case1 testDesc; U2.Case1 testBody],
                testRange)) :> Babel.Statement

    let asserts com (i: Fable.ApplyInfo) =
        match i.methodName with
        | "areEqual" ->
            Fable.Util.ImportCall("assert", "*", Some "equal", false, i.args)
            |> Fable.Util.makeCall com i.range i.returnType |> Some
        | _ -> None
        
    let declareModMember range name _isPublic _modIdent expr =
        Util.varDeclaration (Some range) (Util.identFromName name) expr
        :> Babel.Statement |> U2.Case1
        
    let castStatements (decls: U2<Babel.Statement, Babel.ModuleDeclaration> list) =
        decls |> List.map (function
            | U2.Case1 statement -> statement
            | U2.Case2 _ -> failwith "Unexepected export in test fixture")

open Util

type NUnitPlugin() =
    interface IDeclarePlugin with
        member x.TryDeclareRoot com ctx file =
            if file.Root.TryGetDecorator "TestFixture" |> Option.isNone then None else
            Util.transformModDecls com ctx declareModMember None file.Declarations
            |> castStatements
            |> transformTestFixture file.Root file.Range
            |> U2.Case1
            |> List.singleton
            |> Some
        member x.TryDeclare com ctx decl =
            match decl with
            | Test (test, name, decorator) ->
                transformTest com ctx test name decorator
                |> List.singleton |> Some
            | TestFixture (fixture, testDecls, testRange) ->
                let ctx = { ctx with moduleFullName = fixture.FullName } 
                Util.transformModDecls com ctx declareModMember None testDecls
                |> castStatements
                |> transformTestFixture fixture testRange
                |> List.singleton |> Some
            | _ -> None

    interface IReplacePlugin with
        member x.TryReplace com info =
            match info.ownerFullName with
            | "NUnit.Framework.Assert" -> asserts com info
            | _ -> None
