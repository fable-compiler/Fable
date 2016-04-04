(*
  This code is a copy of Fable.Plugins.NUnit.fsx with the following changes

  - "TestFixture" attribute changed to "TestClass"
  - "Test" attribute changed to "TestMethod"
  - "NUnit.Framework.Assert" changed to "Microsoft.VisualStudio.TestTools.UnitTesting.Assert"

  Both frameworks are very similar, the exception being VisualStudio doesn't 
  (currently?) supports static class members as test methods.
  
  This notice is to remember that future improvements to that code should be 
  ported to this one.
*)

namespace Fable.Plugins

#r "../../build/fable/bin/Fable.exe"

open Fable.AST
open Fable.FSharp2Fable
open Fable.Fable2Babel

module Util =
    let (|TestClass|_|) (decl: Fable.Declaration) =
        match decl with
        | Fable.EntityDeclaration (ent, entDecls, entRange) ->
            match ent.TryGetDecorator "TestClass" with
            | Some _ -> Some (ent, entDecls, entRange)
            | None -> None
        | _ -> None

    let (|TestMethod|_|) (decl: Fable.Declaration) =
        match decl with
        | Fable.MemberDeclaration m ->
            match m.Kind, m.TryGetDecorator "TestMethod" with
            | Fable.Method name, Some _ -> Some (m, name)
            | _ -> None
        | _ -> None

    // Compile tests using Mocha.js BDD interface
    // TODO: Check method signature
    let transformTestMethod com ctx (testMethod: Fable.Member) name =
        let testName =
            Babel.StringLiteral name :> Babel.Expression
        let testBody =
            Util.funcExpression com ctx testMethod.Arguments testMethod.Body :> Babel.Expression
        let testRange =
            match testBody.loc with
            | Some loc -> testMethod.Range + loc | None -> testMethod.Range
        // it('Test name', function() { /* Tests */ });
        Babel.ExpressionStatement(
            Babel.CallExpression(Babel.Identifier "it",
                [U2.Case1 testName; U2.Case1 testBody], testRange), testRange)
        :> Babel.Statement

    let transformTestClass (testClass: Fable.Entity) testDecls testRange =
        let testDesc =
            Babel.StringLiteral testClass.Name :> Babel.Expression
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
            Fable.Util.ImportCall("assert", true, None, Some "equal", false, List.rev i.args)
            |> Fable.Util.makeCall com i.range i.returnType |> Some
        | _ -> None

open Util

type VisualStudioUnitTestsPlugin() =
    interface IDeclarePlugin with
        member x.TryDeclareRoot com ctx file =
            if file.Root.TryGetDecorator "TestClass" |> Option.isNone then None else
            let rootDecls = Util.transformModDecls com ctx None file.Declarations
            transformTestClass file.Root rootDecls file.Range
            |> U2.Case1
            |> List.singleton
            |> Some
        member x.TryDeclare com ctx decl =
            match decl with
            | TestMethod (test, name) ->
                transformTestMethod com ctx test name
                |> List.singleton |> Some
            | TestClass (fixture, testDecls, testRange) ->
                let testDecls =
                    let ctx = { ctx with moduleFullName = fixture.FullName } 
                    Util.transformModDecls com ctx None testDecls
                transformTestClass fixture testDecls testRange
                |> List.singleton |> Some
            | _ -> None

    interface IReplacePlugin with
        member x.TryReplace com info =
            match info.ownerFullName with
            | "Microsoft.VisualStudio.TestTools.UnitTesting.Assert" -> asserts com info
            | _ -> None
