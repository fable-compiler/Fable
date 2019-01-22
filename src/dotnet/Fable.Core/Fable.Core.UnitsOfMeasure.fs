namespace Fable.Core

open System

// UNITS OF MEASURE FOR NON-NUMERIC PRIMITIVE TYPES
// See https://fable.io/repl/#?code=PTAEFUDsEsBcGdQHsBmoCyBTAhvArgE6agpIGiRKQC0keAtpgdAMaiwCeADpogEYdQAUWjMA1qAAq8bAS7YANtHgAoEKADKmYgAtYsLvABcIWAHc4sJgDoWSesABi2PgswBhe12huCweLDYsHjwwACMAAwA7ABMAByRAGxhAJwxUQkRMQCs8SoqSDyQmhwBmPT5ANoAPFi4hJgAgpCUgVYAJo18fEQAbtBB0FQAfAC67NzEfEhICtU1dfhEY6AA5PTDoAC8oNOzKgs4S00tSG2Ynd19A7BDkCucPKB40JCwiQAs87VHDSvrmx2LzenwOP3qRGarSCFy6PUw-UGI3Gj2IAHEXu1vos-uMAdtQBjoO0wTjIadzpd4YjbsiJk8AsxIABzbG-ZZ4jYExmvZmk9knaEdOHXJH3FGTKTQRgaeSQNkQzD-Lk7STSzCy7CQfmKqFnGFU0W08X04gAERhasYCuOysBoAtVitmB1xz1lJFCJudweksdmGdAHkUCh4JhYDbcWsVQ7Lerg6Hw-lURAYLBg2SA+5tipQHnQOosLAdEh2qB4NBmZAgg1EO1MCwFLJiNhTaAiE3jfAdNAuLtw2ZtNr8wWwHg04hUKBGIrkGhi8QZIxnpB6wQFBxeabc-n1AA5ABCK7XG63qMQrwgSHQoFkzIYmDeiG7SDwCjLijM2FK06CLB0O55gEgxsIw9B8EwoAAJLwFAcAZgKAAUAD6oBGLsMxzKs2DDAANKAqHoXsWF8MMACUBKIWRgHlm0rDTuUEHkDBcHpigmYoWhoCvBG2F4QRXE8dUqykRROxUTRwG3KBjGQSxaYIYqnHoTxnzCTh+GEdxIJfCJ5GUdRI5SfRYFMdBsEKexSFacC7y6RpAnobZal6WJoASUZdEyeBckWfBVlKVpKAKEgQTqfxQUhWFrkGZJXkMT5zF+WxHFafWLDSoo4WaVx6WZSR+niYZ+bGd5Znyf5qVcUSWJ8Tl6E1cJomxZ5IEJeVyWKccym0UyrJ1Y5vW8k1hXucVQHxaZvmsV1DQ9c6mrygNWkLXKI1uR5JWTbJSUzQF3Vaf6zrZYNR3qutLVbW1U27ZZVXoWdjAJmGvEOYdcZPSGL0XUV+QAMSUF+BDFAARB8MQg1U1QAEqYAAji8RAAIp4Io0AoNAsIsCwvDwGMKj0KWb7EHtmY5jRbiwNpSiQMQXDML0MIESwuARqAAB6kjuPh7PYDzfCgGYOiPu5nPZmQHOthL7N8BR6GIaV7XTXdApcbzoAAFQcwL1CbOOcBkZsiFU+hvNiTRf3o6AACEjiNAeAAyQjIe4gboAAClBTvQyONF5ohf2gCDIPsGrAt-eNoB-ZgChhn7K7TAAHtU2ugJssAW4+7To-kI4gGAzKPkw9H61TU4zscoAswEJDjiwxoU+G1OvMQ1dU4hidq9gcupzsyFt9UpMClzuG86PpGgIn+SE+0xNSLwVNbLnoCHIqvpPNnBA7qvtoSk8GNuMvlPN7T05iGaoiT+hPIstUm-2kPiq2Kzk87sfrw08Q9BiI4PjEIn19YB9WqAfJUBJH7HGfjXKeb8m5WBrohTe7sgg6EAcA++FFEKgL3NgRgaDhqgPIk5NMOYRz0x4igUGjhwAOwdqAd2jRJAAAl0IAFJAzAHYSHJBKCSB-xwYwI+Td6AcAvuQHY38xFB2ACI4A8hiyQzzMfERv83AEm-qo4gINQHWFgInWAijRygGUaIy+Ejz6X2yEYyQPY6xIF4JAVYVM7D0G8IfHc8CqYiKkSov+O51CeOnBwTRQSpHqBscoUA7R7HwEcc4rwfiVBAA&html=DwQgJg9gxgLgngBwKYAIAWMC2AbAfAKGAxwOAHpi98g&css=Q

[<MeasureAnnotatedAbbreviation>] type bool<[<Measure>] 'm> = bool
[<MeasureAnnotatedAbbreviation>] type uint64<[<Measure>] 'm> = uint64
[<MeasureAnnotatedAbbreviation>] type Guid<[<Measure>] 'm> = Guid
[<MeasureAnnotatedAbbreviation>] type string<[<Measure>] 'm> = string
[<MeasureAnnotatedAbbreviation>] type TimeSpan<[<Measure>] 'm> = TimeSpan
[<MeasureAnnotatedAbbreviation>] type DateTime<[<Measure>] 'm> = DateTime
[<MeasureAnnotatedAbbreviation>] type DateTimeOffset<[<Measure>] 'm> = DateTimeOffset

type UnitOfMeasureTC =
    // Method signatures declare a type relationship between
    // units of measure of the same underlying type
    // NB underlying types in UoM arguments should always match
    static member IsUnitOfMeasure(_ : bool<'a>, _ : bool<'b>) = ()
    static member IsUnitOfMeasure(_ : int<'a>, _ : int<'b>) = ()
    static member IsUnitOfMeasure(_ : int64<'a>, _ : int64<'b>) = ()
    static member IsUnitOfMeasure(_ : uint64<'a>, _ : uint64<'b>) = ()
    static member IsUnitOfMeasure(_ : float<'a>, _ : float<'b>) = ()
    static member IsUnitOfMeasure(_ : decimal<'a>, _ : decimal<'b>) = ()
    static member IsUnitOfMeasure(_ : Guid<'a>, _ : Guid<'b>) = ()
    static member IsUnitOfMeasure(_ : string<'a>, _ : string<'b>) = ()
    static member IsUnitOfMeasure(_ : TimeSpan<'a>, _ : TimeSpan<'b>) = ()
    static member IsUnitOfMeasure(_ : DateTime<'a>, _ : DateTime<'b>) = ()
    static member IsUnitOfMeasure(_ : DateTimeOffset<'a>, _ : DateTimeOffset<'b>) = ()

#nowarn "42"

[<RequireQualifiedAccess>]
module UnitOfMeasure =
    let inline private _cast< ^TC, ^a, ^b when (^TC or ^a or ^b) : (static member IsUnitOfMeasure : ^a * ^b -> unit)> (t : ^a) =
    #if !FABLE_COMPILER
      (# "" t : ^b #)
    #else
      unbox< ^b > t
    #endif

    /// generic unit of measure cast function
    let inline cast (x : ^a) : ^b = _cast<UnitOfMeasureTC,^a,^b> x
