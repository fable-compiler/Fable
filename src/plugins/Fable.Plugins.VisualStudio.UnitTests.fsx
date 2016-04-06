(*
  This code is a copy of Fable.Plugins.NUnit.fsx with the following changes

  - "TestFixture" attribute changed to "TestClass"
  - "Test" attribute changed to "TestMethod"
  - "NUnit.Framework.Assert" changed to "Microsoft.VisualStudio.TestTools.UnitTesting.Assert"
  - Names of methodDecorators changed to Visual Studio counterparts
  - In transformTestMethod, generate testBody without arguments

  Both frameworks are very similar, the exception being VisualStudio doesn't 
  (currently?) supports static class members as test methods.
  
  This notice is to remember that future improvements to that code should be 
  ported to this one.
*)

namespace Fable.Plugins

#r "../../build/fable/bin/Fable.exe"

open Fable
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

    let methodDecorators = Map.ofList ["ClassInitialize", "before";
                                       "TestInitialize", "beforeEach";
                                       "TestMethod", "it";
                                       "TestCleanup", "afterEach";
                                       "ClassCleanup", "after"]

    let (|TestMethod|_|) (decl: Fable.Declaration) =
        match decl with
        | Fable.MemberDeclaration m ->
            match m.Kind, (m.Decorators |> List.tryFind (fun x -> Map.containsKey x.Name methodDecorators)) with
            | Fable.Method name, Some decorator -> Some (m, name, decorator)
            | _ -> None
        | _ -> None

    // Compile tests using Mocha.js BDD interface
    // TODO: Check method signature
    let transformTestMethod com ctx (testMethod: Fable.Member) name (decorator: Fable.Decorator) =
        let testName =
            Babel.StringLiteral name :> Babel.Expression
        let testBody =
            // Don't pass arguments ("ClassInitialize" method has one)
            Util.funcExpression com ctx [] testMethod.Body :> Babel.Expression
        let testRange =
            match testBody.loc with
            | Some loc -> testMethod.Range + loc | None -> testMethod.Range
        let newMethodName = methodDecorators.Item(decorator.Name)
        // it('Test name', function() { /* Tests */ });
        Babel.ExpressionStatement(
            Babel.CallExpression(Babel.Identifier newMethodName,
                [U2.Case1 testName; U2.Case1 testBody], testRange), testRange)
        :> Babel.Statement

    let transformTestClass (testClass: Fable.Entity) testRange testDecls =
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
            Fable.Util.ImportCall("assert", "*", Some "equal", false, List.rev i.args)
            |> Fable.Util.makeCall com i.range i.returnType |> Some
        | _ -> None
        
    let declareModMember range name _isPublic _modIdent expr =
        Util.varDeclaration (Some range) (Util.identFromName name) expr
        :> Babel.Statement |> U2.Case1
        
    let castStatements (decls: U2<Babel.Statement, Babel.ModuleDeclaration> list) =
        decls |> List.map (function
            | U2.Case1 statement -> statement
            | U2.Case2 _ -> failwith "Unexepected export in test class")

open Util

type VisualStudioUnitTestsPlugin() =
    interface IDeclarePlugin with
        member x.TryDeclareRoot com ctx file =
            if file.Root.TryGetDecorator "TestClass" |> Option.isNone then None else
            Util.transformModDecls com ctx declareModMember None file.Declarations
            |> castStatements
            |> transformTestClass file.Root file.Range
            |> U2.Case1
            |> List.singleton
            |> Some
        member x.TryDeclare com ctx decl =
            match decl with
            | TestMethod (test, name, decorator) ->
                transformTestMethod com ctx test name decorator
                |> List.singleton |> Some
            | TestClass (testClass, testDecls, testRange) ->
                let ctx = { ctx with moduleFullName = testClass.FullName } 
                Util.transformModDecls com ctx declareModMember None testDecls
                |> castStatements
                |> transformTestClass testClass testRange
                |> List.singleton |> Some
            | _ -> None

    interface IReplacePlugin with
        member x.TryReplace com info =
            match info.ownerFullName with
            | "Microsoft.VisualStudio.TestTools.UnitTesting.Assert" -> asserts com info
            | _ -> None
