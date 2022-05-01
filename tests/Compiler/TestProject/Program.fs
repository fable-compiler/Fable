open System
open Fable.Core
open Fable.Core.JsInterop

type FieldAndIndexer3 =
  abstract Name: string
  [<EmitIndexer>]
  abstract Item:index: string -> obj
let v: FieldAndIndexer3 = !!{| Name = "John"; Value1 = "foo"; Value2 = 42; Value3 = obj (); Value4 = 3.14; Value5 = [1;2;3;4]; Value6 = (1, 3.14); Value7 = {| Value = 42 |} |}