module Fable.Tests.Compiler.AnonRecordInInterface

open System
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

module private Source =
  let nl = Environment.NewLine
  let indent = (+) "  "
  let q = sprintf "\"%s\""
  let concat = String.concat nl
  let print source =
    printfn "%s" source
    source

  type Property =
    | ReadOnly of Name: string * Type: string
    | ReadWrite of Name: string * Type: string
    | Indexer of Name: string * IndexType: string * Type: string
    | ReadWriteIndexer of Name: string * IndexType: string * Type: string
  module Property =
    let format =
      function
      | ReadOnly (name, ty) ->
          sprintf "abstract %s: %s" name ty
          |> List.singleton
      | ReadWrite (name, ty) ->
          sprintf "abstract %s: %s with get,set" name ty
          |> List.singleton
      | Indexer (name, index, ty) ->
          [
            "[<EmitIndexer>]"
            sprintf "abstract %s:index: %s -> %s" name index ty
          ]
      | ReadWriteIndexer (name, index, ty) ->
          [
            "[<EmitIndexer>]"
            sprintf "abstract %s:index: %s -> %s with get,set" name index ty
          ]

  type Interface = {
    Name: string
    Properties: Property list
  }
  module Interface =
    let format i =
      [
        yield sprintf "type %s =" i.Name

        yield!
          i.Properties
          |> List.collect Property.format
          |> List.map indent
      ]

  type Case = {
    Name: string
    Fields: string list // no names, just types
  }
  module Case =
    let format c =
      match c.Fields with
      | [] -> sprintf "| %s" c.Name
      | fields ->
          fields
          |> String.concat " * "
          |> sprintf "| %s of %s" c.Name
      |> List.singleton
  type DiscriminatedUnion = {
    Name: string
    Erase: bool
    Cases: Case list
  }
  module DiscriminatedUnion =
    let format du =
      [
        if du.Erase then
          yield "[<Erase>]"
        yield sprintf "type %s =" du.Name

        yield!
          du.Cases
          |> List.collect Case.format
          |> List.map indent
      ]

  let assign (name: string) (ty: string) (value: string) =
    sprintf "let %s: %s = %s" name ty value

  /// name of variabe is `v`
  let assignAnonRecord (i: Interface) (anonRecordFields: (string * string) list) =
    assert(anonRecordFields |> List.isEmpty |> not)

    anonRecordFields
    |> List.map (fun (name, value) -> sprintf "%s = %s" name value)
    |> String.concat "; "
    |> sprintf "!!{| %s |}"
    |> assign "v" i.Name

  let interfaceAndAssignments (i: Interface) (assignments: string list) =
    [
      yield! i |> Interface.format
      yield! assignments
    ]
    |> concat

  let interfaceAndAnonRecordAssignment (i: Interface) (anonRecordFields: (string * string) list) =
    assignAnonRecord i anonRecordFields
    |> List.singleton
    |> interfaceAndAssignments i

open Source

let settings: Compiler.Settings =
  let s = Compiler.Settings.standard
  { s with
      Opens =
        [
          yield! s.Opens
          yield "Fable.Core"
          yield "Fable.Core.JsInterop"
        ]
  }
let compile source = Compiler.Cached.compile settings source
let private testCase msg test = testCase msg (test >> ignore)

module Error =

  let missingField = "Object doesn't contain field"
  let unexpectedType = "Expected type"
  let unexpectedTypeMulti = "Expected any type of"
  let unexpectedIndexerType = "Expected type"
  let unexpectedIndexerTypeMulti = "Expected any type of"

  type private Rx = System.Text.RegularExpressions.Regex
  /// Note: names cannot contain `'` (or `"`)
  type Regex private () =
    static let orNameRegex = Option.defaultValue "([^'\"]+)"
    static let orNamesRegex =
      Option.map (fun ns ->
        ns
        |> List.map (sprintf "'%s'")
        |> String.concat "; "
        |> sprintf "(\\[%s\\])"
      )
      >> Option.defaultValue "\\[\\s*(('[^'\"]+');?\\s*)+\\]"
    static member MissingField (?interfaceName: string, ?fieldName: string, ?expectedType: string) =
      let interfaceName = interfaceName |> orNameRegex
      let fieldName = fieldName |> orNameRegex
      let expectedType = expectedType |> orNameRegex
      $"Object doesn't contain field '{fieldName}' of type '{expectedType}' required by interface '{interfaceName}'"
      |> Rx
    static member UnexpectedType (?interfaceName: string, ?fieldName: string, ?expectedType: string, ?actualType: string) =
      let interfaceName = interfaceName |> orNameRegex
      let fieldName = fieldName |> orNameRegex
      let expectedType = expectedType |> orNameRegex
      let actualType = actualType |> orNameRegex
      $"Expected type '{expectedType}' for field '{fieldName}' in interface '{interfaceName}', but is '{actualType}'"
      |> Rx
    static member UnexpectedTypeMulti (?interfaceName: string, ?fieldName: string, ?expectedTypes: string list, ?actualType: string) =
      let interfaceName = interfaceName |> orNameRegex
      let fieldName = fieldName |> orNameRegex
      let expectedTypes = expectedTypes |> orNamesRegex
      let actualType = actualType |> orNameRegex
      $"Expected any type of {expectedTypes} for field '{fieldName}' in interface '{interfaceName}', but is '{actualType}'"
      |> Rx
    static member UnexpectedIndexerType (?interfaceName: string, ?indexerName: string, ?fieldName: string, ?expectedType: string, ?actualType: string) =
      let interfaceName = interfaceName |> orNameRegex
      let indexerName = indexerName |> orNameRegex
      let fieldName = fieldName |> orNameRegex
      let expectedType = expectedType |> orNameRegex
      let actualType = actualType |> orNameRegex
      $"Expected type '{expectedType}' for field '{fieldName}' because of Indexer '{indexerName}' in interface '{interfaceName}', but is '{actualType}'"
      |> Rx
    static member UnexpectedIndexerTypeMulti (?interfaceName: string, ?indexerName: string, ?fieldName: string, ?expectedTypes: string list, ?actualType: string) =
      let interfaceName = interfaceName |> orNameRegex
      let indexerName = indexerName |> orNameRegex
      let fieldName = fieldName |> orNameRegex
      let expectedTypes = expectedTypes |> orNamesRegex
      let actualType = actualType |> orNameRegex
      $"Expected any type of {expectedTypes} for field '{fieldName}' because of Indexer '{indexerName}' in interface '{interfaceName}', but is '{actualType}'"
      |> Rx

module private Ty =
  let string = "System.String"
  let int = "System.Int32"
  let float = "System.Double"

// Currently 'errors` are compiler warnings
let tests =
  testList "Cast Anonymous Record to interface (with `!!`)" [
    testList "Tests for Compiler Messages Tests" [
      let i = { Name = "Field1"; Properties = [ ReadOnly ("Value", "string") ] }
      testCase "missing field" <| fun _ ->
        interfaceAndAnonRecordAssignment i [("Foo", q "foo")]
        |> compile
        |> Assert.Is.Single.errorOrWarning
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField ())
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (fieldName = "Value"))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (expectedType = Ty.string))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Value", expectedType = Ty.string))
      testCase "unexpected type" <| fun _ ->
        interfaceAndAnonRecordAssignment i [("Value", string 42)]
        |> compile
        |> Assert.Is.Single.errorOrWarning
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType ())
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (fieldName = "Value"))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (expectedType = Ty.string))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (actualType = Ty.int))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Value", expectedType = Ty.string, actualType = Ty.int))

      let i = { Name = "Field2"; Properties = [ ReadOnly ("Value", "U2<string,int>") ] }
      testCase "unexpected type Multi" <| fun _ ->
        interfaceAndAnonRecordAssignment i [("Value", string 3.14)]
        |> compile
        |> Assert.Is.Single.errorOrWarning
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti ())
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (interfaceName = i.Name))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (fieldName = "Value"))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (expectedTypes = [Ty.string; Ty.int]))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (actualType = Ty.float))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (interfaceName = i.Name, fieldName = "Value", expectedTypes = [Ty.string; Ty.int], actualType = Ty.float))

      let i = { Name = "Indexer1"; Properties = [ Indexer ("Item", "string", "string") ] }
      testCase "unexpected indexer type" <| fun _ ->
        interfaceAndAnonRecordAssignment i [("Value", string 42)]
        |> compile
        |> Assert.Is.Single.errorOrWarning
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType ())
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (indexerName = "Item"))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (fieldName = "Value"))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (expectedType = Ty.string))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (actualType = Ty.int))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, indexerName = "Item", fieldName = "Value", expectedType = Ty.string, actualType = Ty.int))

      let i = { Name = "Indexer2"; Properties = [ Indexer ("Item", "string", "U2<string,int>") ] }
      testCase "unexpected indexer type Multi" <| fun _ ->
        interfaceAndAnonRecordAssignment i [("Value", string 3.14)]
        |> compile
        |> Assert.Is.Single.errorOrWarning
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti ())
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (interfaceName = i.Name))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (indexerName = "Item"))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (fieldName = "Value"))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (expectedTypes = [Ty.string; Ty.int]))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (actualType = Ty.float))
        |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (interfaceName = i.Name, indexerName = "Item", fieldName = "Value", expectedTypes = [Ty.string; Ty.int], actualType = Ty.float))
    ]

    testList "Interface with fields" [
      testList "Interface with single readonly field" [
        let i = { Name = "Field1"; Properties = [ ReadOnly ("Value", "string") ] }
        testCase "no error for existing field with correct type" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for existing field with wrong type" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Value"))
        testCase "error for missing field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Answer", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Value"))
        testCase "error for missing field because of lower case typo" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("value", q "foo")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Value"))
      ]
      testList "interface with single readwrite field" [
        let i = { Name = "Field1"; Properties = [ ReadWrite ("Value", "string") ] }
        testCase "no error for existing field with correct type" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for existing field with wrong type" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Value"))
        testCase "error for missing field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Answer", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Value"))
        testCase "error for missing field because of lower case typo" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("value", q "foo")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Value"))

      ]
      testList "interface with two fields" [
        let i = { Name = "Field2"; Properties = [ ReadOnly ("Value1", "string"); ReadOnly ("Value2", "int") ] }

        testCase "no error fir existing fields with correct types" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value1", q "foo"); ("Value2", string 42)]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for existing field with wrong type" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value1", q "foo"); ("Value2", q "bar")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Value2"))
        testCase "error for missing field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value2", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Value1"))
        testCase "two errors for two missing field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("SomeValue", string 42)]
          |> compile
          |> Assert.Are.errorsOrWarnings 2
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Value1"))
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Value2"))
        testCase "two errors for missing field and wrong type" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value1", string 42)]
          |> compile
          |> Assert.Are.errorsOrWarnings 2
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Value1"))
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Value2"))
      ]
      testList "interface with U2 field" [
        // Ux receive special treatment: containing types (here string & int) are extracted and both types are accepted
        let ty = "U2<string,int>"
        let i = { Name = "Field3"; Properties = [ ReadOnly ("Value", ty) ] }

        testCase "no error for existing field with string" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for existing field with int" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for existing field with float" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 3.14)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (interfaceName = i.Name, fieldName = "Value"))
        //testCase "no error for existing field with U2<string,int>" <| fun _ ->
        //  interfaceAndAssignments i [
        //    assign "a" ty "U2.Case2 42"
        //    assignAnonRecord i [("Value", "a")]
        //  ]
        //  |> compile
        //  |> Assert.Is.strictSuccess
        //testCase "Ideally: error because of missmatching type U3" <| fun _ ->
        //  // Erased Union is reduced to type `Any` -> cannot distinguish between correct U2<string,int> and other Erased Unions (or even just `obj`)
        //  interfaceAndAssignments i [
        //    assign "a" "U3<string,int,float>" "U3.Case3 3.14"
        //    assignAnonRecord i [("Value", "a")]
        //  ]
        //  |> compile
        //  |> Assert.Is.strictSuccess
        testCase "Ideally: error because of missmatching type obj" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "obj()")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "Probably: no error because of double-bangs" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "!!3.14")]
          |> compile
          |> Assert.Is.errorOrWarning
      ]
      testList "interface with U3<int, string, float>" [
        let ty = "U3<string,int, float>"
        let i = { Name = "Field3"; Properties = [ ReadOnly ("Value", ty) ] }

        testCase "no error for existing field with string" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for existing field with int" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for existing field with float" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 3.14)]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for existing field with char" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "\'c\'")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (interfaceName = i.Name, fieldName = "Value"))
      ]
      testList "interface with Union field" [
        let du = { Name = "Union1"; Erase = false; Cases = [ { Name = "SomeInt"; Fields = ["int"] } ]}
        let i = { Name = "Field4"; Properties = [ ReadOnly ("Value", du.Name) ] }

        testCase "no error for existing field with correct type" <| fun _ ->
          [
            yield! du |> DiscriminatedUnion.format
            yield! i |> Interface.format

            yield assignAnonRecord i [("Value", "SomeInt 42")]
          ]
          |> concat
          |> compile
          |> Assert.Is.strictSuccess

        testCase "error for existing field with wrong type" <| fun _ ->
          [
            yield! du |> DiscriminatedUnion.format
            yield! i |> Interface.format

            yield assignAnonRecord i [("Value", string 42)]
          ]
          |> concat
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Value"))
      ]
      //testList "interface with Erased Union field" [
      //  let du = { Name = "ErasedUnion1"; Erase = true; Cases = [ { Name = "SomeInt"; Fields = ["int"] } ]}
      //  let i = { Name = "Field5"; Properties = [ ReadOnly ("Value", du.Name) ] }

      //  testCase "no error for existing field with correct type" <| fun _ ->
      //    [
      //      yield! du |> DiscriminatedUnion.format
      //      yield! i |> Interface.format

      //      yield assignAnonRecord i [("Value", "SomeInt 42")]
      //    ]
      //    |> concat
      //    |> compile
      //    |> Assert.Is.strictSuccess

      //  testCase "Ideally: error for existing field with wrong int type" <| fun _ ->
      //    // Erased Union gets reduced to `Any` -> `Any` accepts everything
      //    [
      //      yield! du |> DiscriminatedUnion.format
      //      yield! i |> Interface.format

      //      yield assignAnonRecord i [("Value", string 42)]
      //    ]
      //    |> concat
      //    |> compile
      //    // |> Assert.Is.Single.errorOrWarning
      //    // |> Assert.Exists.errorOrWarningWith (Error.incorrectType "Tmp.ErasedUnion1" "Value")
      //    |> Assert.Is.strictSuccess

      //  testCase "Ideally: error for existing field with wrong obj type" <| fun _ ->
      //    [
      //      yield! du |> DiscriminatedUnion.format
      //      yield! i |> Interface.format

      //      yield assignAnonRecord i [("Value", "obj ()")]
      //    ]
      //    |> concat
      //    |> compile
      //    |> Assert.Is.strictSuccess
      //]

      testList "interface with field with option type" [
        let i = { Name = "Field6"; Properties = [ ReadOnly ("Value", "string option"); ReadOnly ("Required", "string") ] }

        testCase "no error for field with string option" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "Some \"foo\""); ("Required", q "bar")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for field with string" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo"); ("Required", q "bar")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for missing optional field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Required", q "bar")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for field with int option" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "Some 42"); ("Required", q "bar")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Value"))
        testCase "error for field with int" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42); ("Required", q "bar")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Value"))
        testCase "error for missing required field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Required"))
      ]
      testList "interface with field with option U2 type" [
        let i = { Name = "Field7"; Properties = [ ReadOnly ("Value", "U2<string, int> option"); ReadOnly ("Required", "string") ] }

        testCase "no error for field with string option" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "Some \"foo\""); ("Required", q "bar")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for field with string" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo"); ("Required", q "bar")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for field with int option" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "Some \"foo\""); ("Required", q "bar")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for field with int" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo"); ("Required", q "bar")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for missing optional field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Required", q "bar")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for field with float option" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "Some 3.14"); ("Required", q "bar")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (interfaceName = i.Name, fieldName = "Value"))
        testCase "error for field with float" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 3.14); ("Required", q "bar")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (interfaceName = i.Name, fieldName = "Value"))
        testCase "error for missing required field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Required"))
      ]
      testList "interface with field with aliased U2 type" [
        let al = "type Value = U2<bool, string>"
        let i = { Name = "Field8"; Properties = [ ReadOnly ("Value", "Value") ] }
        let tys = [
          al
          yield! i |> Interface.format
        ]
        let aliasAndInterfaceAndAnonRecordAssignment value =
          [
            yield! tys
            assignAnonRecord i [("Value", value)]
          ]

        testCase "no error for field with string" <| fun _ ->
          aliasAndInterfaceAndAnonRecordAssignment "\"foo\""
          |> concat
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for field with bool" <| fun _ ->
          aliasAndInterfaceAndAnonRecordAssignment "true"
          |> concat
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for field with int" <| fun _ ->
          aliasAndInterfaceAndAnonRecordAssignment "42"
          |> concat
          |> compile
          |> Assert.Is.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (interfaceName = i.Name, fieldName = "Value"))
        // testCase "no error for field with Value" <| fun _ ->
        //   [
        //     yield! tys
        //     assign "value" "Value" "U2.Case1 true"
        //     assignAnonRecord i [("Value", "value")]
        //   ]
        //   |> concat
        //   |> compile
        //   |> Assert.Is.strictSuccess
        // testCase "no error for field with U2<bool, string>" <| fun _ ->
        //   [
        //     yield! tys
        //     assign "value" "U2<bool, string>" "U2.Case1 true"
        //     assignAnonRecord i [("Value", "value")]
        //   ]
        //   |> concat
        //   |> compile
        //   |> Assert.Is.strictSuccess
        // testCase "no error for field with U2<int, string>" <| fun _ ->
        //   [
        //     yield! tys
        //     assign "value" "U2<int, string>" "U2.Case1 42"
        //     assignAnonRecord i [("Value", "value")]
        //   ]
        //   |> concat
        //   |> compile
        //   |> Assert.Is.strictSuccess
        testCase "no error for field with string & Aliased Value" <| fun _ ->
          [
            "type Root = U2<bool, string>"
            "type Value = Root"
            yield! i |> Interface.format
            assignAnonRecord i [("Value", "\"foo\"")]
          ]
          |> concat
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for field with int & Aliased Value" <| fun _ ->
          [
            "type Root = U2<bool, string>"
            "type Value = Root"
            yield! i |> Interface.format
            assignAnonRecord i [("Value", "42")]
          ]
          |> concat
          |> compile
          |> Assert.Is.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (interfaceName = i.Name, fieldName = "Value"))

        testCase "no error for option field with string & aliased Value" <| fun _ ->
          [
            al
            yield!
              { Name = "Field8"; Properties = [ ReadOnly ("Value", "Value option") ] }
              |> Interface.format
            assignAnonRecord i [("Value", "\"foo\"")]
          ]
          |> concat
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for option field with int & aliased Value" <| fun _ ->
          [
            al
            yield!
              { Name = "Field8"; Properties = [ ReadOnly ("Value", "Value option") ] }
              |> Interface.format
            assignAnonRecord i [("Value", "42")]
          ]
          |> concat
          |> compile
          |> Assert.Is.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedTypeMulti (interfaceName = i.Name, fieldName = "Value"))
      ]
    ]

    testList "Interface with EmitIndexer" [
      testList "interface with single indexer of type string" [
        let i = { Name = "Indexer1"; Properties = [ Indexer ("Item", "string", "string") ] }

        testCase "no error for single string field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for single int field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, indexerName = "Item", fieldName = "Value"))
        testCase "no errror for multiple string fields" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("V1", q "a"); ("V2", q "b"); ("V3", q "c"); ("V4", q "d"); ("V5", q "e")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for two int fields" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("V1", q "a"); ("V2", string 42); ("V3", q "c"); ("V4", string 42); ("V5", q "e")]
          |> compile
          |> Assert.Are.errorsOrWarnings 2
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, indexerName = "Item", fieldName = "V2"))
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, indexerName = "Item", fieldName = "V4"))
        testCase "error for field with function" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("F", "fun (x: string) -> x.ToLower()")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, indexerName = "Item", fieldName = "F"))
        testCase "error for field with partial applied function" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("F", "sprintf \"Hello %s\"")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, indexerName = "Item", fieldName = "F"))
      ]
      testList "interface with single indexer of type U2<string, int>" [
        let ty = "U2<string, int>"
        let i = { Name = "Indexer2"; Properties = [ Indexer ("Item", "string", ty) ] }

        testCase "no error for single string field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for single int field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for single float field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 3.14)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (interfaceName = i.Name, indexerName = "Item", fieldName = "Value"))
        //testCase "no error for single U2<string,int> field" <| fun _ ->
        //  [
        //    yield! i |> Interface.format
        //    yield assign "a" ty "U2.Case2 42"

        //    yield assignAnonRecord i [("Value", "a")]
        //  ]
        //  |> concat
        //  |> compile
        //  |> Assert.Is.strictSuccess
        //testCase "Ideally: error for missmatching type U3" <| fun _ ->
        //  [
        //    yield! i |> Interface.format
        //    yield assign "a" "U3<string,int,float>" "U3.Case3 3.14"

        //    yield assignAnonRecord i [("Value", "a")]
        //  ]
        //  |> concat
        //  |> compile
        //  |> Assert.Is.strictSuccess
      ]
      testList "interface with two indexers U<string,int> and int" [
        // in TS: number indexer must be subset of string indexer
        let ty = "U2<string, int>"
        let i = { Name = "Indexer3"; Properties = [ Indexer ("Item", "string", ty); Indexer ("Item", "int", "int") ] }

        testCase "no error for single string field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for single int field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for single float field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 3.14)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (interfaceName = i.Name, indexerName = "Item", fieldName = "Value"))
      ]
      testList "indexer with other name than `Item`" [
        let i = { Name = "Indexer4"; Properties = [ Indexer ("MyIndexer", "string", "string") ] }

        testCase "no error for single string field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for single int field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, indexerName = "MyIndexer", fieldName = "Value"))
      ]
      testList "indexer with string indexer and get,set" [
        let i = { Name = "Indexer5"; Properties = [ ReadWriteIndexer ("Item", "string", "string") ] }

        testCase "no error for single string field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for single int field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, indexerName = "Item", fieldName = "Value"))
      ]

      testList "indexer with string option" [
        let i = { Name = "Indexer6"; Properties = [ Indexer ("Item", "string", "string option") ] }

        testCase "no error for field with string option" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "Some \"foo\"")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for field with string" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for field with int option" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "Some 42")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, fieldName = "Value"))
        testCase "error for field with int" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, fieldName = "Value"))
      ]
      testList "indexer with U2<string,int> option" [
        let ty = "U2<string, int> option"   // same as `U2<string option, string option>`
        let i = { Name = "Indexer7"; Properties = [ Indexer ("Item", "string", ty) ] }

        testCase "no error for field with string option" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "Some \"foo\"")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for field with string" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for field with int option" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "Some \"foo\"")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for field with int" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for field with float option" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", "Some 3.14")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (interfaceName = i.Name, fieldName = "Value"))
        testCase "error for field with float" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 3.14)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (interfaceName = i.Name, fieldName = "Value"))
      ]
    ]

    testList "Interface fields and indexer" [
      testList "Indexer and single field with string type" [
        let i = { Name = "FieldAndIndexer1"; Properties = [
            ReadOnly ("Name", "string")
            Indexer ("Item", "string", "string")
          ]}

        testCase "No error for Name field and no `Item` field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", q "John")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for Name field and additional string fields" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", q "John"); ("Value1", q "foo"); ("Value2", q "bar"); ("Value3", q "baz")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for missing name field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", q "foo")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Name"))
        testCase "error for Name field with wrong type" <| fun _ ->
          // NOTE: only ONE error, NOT two (one for interface field, one for indexer)
          interfaceAndAnonRecordAssignment i [("Name", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Name"))
        testCase "two errors for missing name field AND additional int field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Value", string 42)]
          |> compile
          |> Assert.Are.errorsOrWarnings 2
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerType (interfaceName = i.Name, fieldName = "Value"))
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.MissingField (interfaceName = i.Name, fieldName = "Name"))
      ]
      testList "Indexer U2<string,int> and string field Name" [
        let ty = "U2<string,int>"
        let i = { Name = "FieldAndIndexer2"; Properties = [
            ReadOnly ("Name", "string")
            Indexer ("Item", "string", ty)
          ]}

        testCase "no error for Name field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", q "John")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for int Name field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Name"))
        testCase "error for float Name field" <| fun _ ->
          //Note: just ONE error
          interfaceAndAnonRecordAssignment i [("Name", string 3.14)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Name"))
        testCase "no error for string Name field and additional string fields" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", q "John"); ("Value1", q "foo"); ("Value2", q "bar"); ("Value3", q "baz")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for string Name field and additional int fields" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", q "John"); ("Value1", string 1); ("Value2", string 2); ("Value3", string 3)]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "no error for string Name field and additional string & int fields" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", q "John"); ("Value1", q "foo"); ("Value2", q "bar"); ("Value3", string 3); ("Value4", string 4); ("Value5", q "baz")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for additional float field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", q "John"); ("Value", string 3.14)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (interfaceName = i.Name, indexerName = "Item", fieldName = "Value"))
        testCase "error for one float field among several valid fields" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", q "John"); ("Value1", q "foo"); ("Value2", q "bar"); ("Value3", string 3.14); ("Value4", string 4); ("Value5", q "baz")]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedIndexerTypeMulti (interfaceName = i.Name, indexerName = "Item", fieldName = "Value3"))
      ]
      testList "any Indexer and string field" [
        let i = { Name = "FieldAndIndexer3"; Properties = [
            ReadOnly ("Name", "string")
            Indexer ("Item", "string", "obj")
          ]}

        testCase "no error for string Name field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", q "John")]
          |> compile
          |> Assert.Is.strictSuccess
        testCase "error for int Name field" <| fun _ ->
          interfaceAndAnonRecordAssignment i [("Name", string 42)]
          |> compile
          |> Assert.Is.Single.errorOrWarning
          |> Assert.Exists.errorOrWarningMatching (Error.Regex.UnexpectedType (interfaceName = i.Name, fieldName = "Name"))
        testCase "no error for string Name field and several additional fields of various types" <| fun _ ->
          interfaceAndAnonRecordAssignment i [
            ("Name", q "John")
            ("Value1", q "foo")
            ("Value2", string 42)
            ("Value3", "obj ()")
            ("Value4", string 3.14)
            ("Value5", "[1;2;3;4]")
            ("Value6", "(1, 3.14)")
            ("Value7", "{| Value = 42 |}")
          ]
          |> compile
          |> Assert.Is.strictSuccess
      ]
    ]
  ]
