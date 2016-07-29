(*
  This code is a copy of Fable.Plugins.NUnit.fsx with the following changes

  - "TestFixture" attribute changed to "TestClass"
  - "Test" attribute changed to "TestMethod"
  - "NUnit.Framework.Assert" changed to "Microsoft.VisualStudio.TestTools.UnitTesting.Assert"
  - Names of methodDecorators changed to Visual Studio counterparts
  - In transformTestMethod, generate testBody without arguments. Also, doesn't check for 
    method signature (since it does not have support for test parameters, anyway)

  Both frameworks are very similar, the exception being VisualStudio doesn't 
  (currently?) supports static class members as test methods.
  
  This notice is to remember that future improvements to that code should be 
  ported to this one.
*)

namespace Fable.Plugins

#r "../../../build/fable/bin/Fable.Core.dll"
#r "../../../build/fable/bin/Fable.Compiler.dll"

open Fable
open Fable.AST
open Fable.Fable2Babel

module Util =
    let (|TestClass|_|) (decl: Fable.Declaration) =
        match decl with
        | Fable.EntityDeclaration (ent, _, entDecls, entRange) ->
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
            | Fable.Method, Some decorator -> Some (m, m.Name, decorator)
            | _ -> None
        | _ -> None
        
    let [<Literal>] runSyncWarning = "Async.RunSynchronously must wrap the whole test"

    // Compile tests using Mocha.js BDD interface
    let transformTestMethod (com: IBabelCompiler) ctx (testMethod: Fable.Member)
                            name (decorator: Fable.Decorator) =
        let buildAsyncTestBody range asyncBuilder =
            let doneFn = AST.Fable.Util.makeIdent "$done"
            let testBody =
                let doneFn = doneFn |> Fable.IdentValue |> Fable.Value
                let args = [asyncBuilder; doneFn; doneFn; doneFn]
                AST.Fable.Util.CoreLibCall("Async", Some "startWithContinuations", false, args)
                |> AST.Fable.Util.makeCall com range Fable.Unit
            [doneFn], testBody
        if testMethod.Arguments.Length > 0 then
            failwithf "Test parameters are not supported (testName = '%s')." name
        let testArgs, testBody =
            let (|RunSync|_|) = function
                | Fable.Sequential([Fable.Throw(Fable.Value(Fable.StringConst warning),_,_); arg],_)
                    when warning = runSyncWarning -> Some arg
                | _ -> None
            match testMethod.Body with
            | Fable.Apply(Fable.Value(Fable.Lambda(_,RunSync _)),[asyncBuilder],Fable.ApplyMeth,_,_)
            | RunSync asyncBuilder -> buildAsyncTestBody testMethod.Body.Range asyncBuilder
            | _ -> [], testMethod.Body
        let testBody =
            let args, body = com.TransformFunction ctx testArgs testBody
            Babel.ArrowFunctionExpression (args, body, ?loc=testBody.Range) :> Babel.Expression
        let testName =
            Babel.StringLiteral name :> Babel.Expression
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
        | "AreEqual" ->
            Fable.Util.ImportCall("assert", "*", Some "equal", false, i.args)
            |> Fable.Util.makeCall com i.range i.returnType |> Some
        | _ -> None
        
    let declareModMember range publicName privateName _isPublic _isMutable _modIdent expr =
        let privateName = defaultArg privateName publicName
        Util.varDeclaration (Some range) (Util.identFromName privateName) expr
        :> Babel.Statement |> U2.Case1 |> List.singleton
        
    let castStatements (decls: U2<Babel.Statement, Babel.ModuleDeclaration> list) =
        decls |> List.map (function
            | U2.Case1 statement -> statement
            | U2.Case2 _ -> failwith "Unexpected export in test class")

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
            match info.ownerFullName, info.methodName with
            | "Microsoft.VisualStudio.TestTools.UnitTesting.Assert", _ -> asserts com info
            | "Microsoft.FSharp.Control.FSharpAsync", "RunSynchronously" ->
                match info.returnType with
                | Fable.Unit ->
                    let warning = Fable.Throw(Fable.Value(Fable.StringConst Util.runSyncWarning), Fable.Unit, None)
                    AST.Fable.Util.makeSequential info.range [warning; info.args.Head] |> Some 
                | _ -> failwithf "Async.RunSynchronously in tests is only allowed with Async<unit> %O" info.range
            | _ -> None
