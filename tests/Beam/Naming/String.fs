/// Used to compile to `string.erl`, shadowing OTP's `string` stdlib module.
module Fable.Tests.Naming.String

let reverse (s: string) = s |> Seq.rev |> Seq.toArray |> System.String

let repeat (n: int) (s: string) = String.replicate n s
