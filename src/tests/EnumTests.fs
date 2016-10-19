[<NUnit.Framework.TestFixture>]
module Fable.Tests.Enum

open System
open FSharp.Core.LanguagePrimitives
open NUnit.Framework
open Fable.Tests.Util

type Fruits =
| Apple = 1
| Banana = 2
| Coconut = 3

[<Test>]
let ``Enum operator = works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple = Fruits.Apple |> equal true
    a = Fruits.Apple |> equal true
    Fruits.Apple = a |> equal true
    a = a |> equal true

    Fruits.Apple = Fruits.Banana |> equal false
    a = Fruits.Banana |> equal false
    Fruits.Banana = a |> equal false
    a = b |> equal false

[<Test>]
let ``Enum operator <> works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple <> Fruits.Apple |> equal false
    a <> Fruits.Apple |> equal false
    Fruits.Apple <> a |> equal false
    a <> a |> equal false

    Fruits.Apple <> Fruits.Banana |> equal true
    a <> Fruits.Banana |> equal true
    Fruits.Banana <> a |> equal true
    a <> b |> equal true

[<Test>]
let ``Enum operator < works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple < Fruits.Apple |> equal false
    a < Fruits.Apple |> equal false
    Fruits.Apple < a |> equal false
    a < a |> equal false

    Fruits.Apple < Fruits.Banana |> equal true
    a < Fruits.Banana |> equal true
    Fruits.Banana < a |> equal false
    a < b |> equal true

[<Test>]
let ``Enum operator <= works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple <= Fruits.Apple |> equal true
    a <= Fruits.Apple |> equal true
    Fruits.Apple <= a |> equal true
    a <= a |> equal true

    Fruits.Apple <= Fruits.Banana |> equal true
    a <= Fruits.Banana |> equal true
    Fruits.Banana <= a |> equal false
    a <= b |> equal true

[<Test>]
let ``Enum operator > works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple > Fruits.Apple |> equal false
    a > Fruits.Apple |> equal false
    Fruits.Apple > a |> equal false
    a > a |> equal false

    Fruits.Apple > Fruits.Banana |> equal false
    a > Fruits.Banana |> equal false
    Fruits.Banana > a |> equal true
    a > b |> equal false

[<Test>]
let ``Enum operator >= works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple >= Fruits.Apple |> equal true
    a >= Fruits.Apple |> equal true
    Fruits.Apple >= a |> equal true
    a >= a |> equal true

    Fruits.Apple >= Fruits.Banana |> equal false
    a >= Fruits.Banana |> equal false
    Fruits.Banana >= a |> equal true
    a >= b |> equal false

[<Test>]
let ``EnumOfValue works``() =
    EnumOfValue 1 |> equal Fruits.Apple
    EnumOfValue 2 |> equal Fruits.Banana
    EnumOfValue 3 |> equal Fruits.Coconut

open LanguagePrimitives

type Instruction =
| Jmp = 0x40uy // 0100 0000
| Jsr = 0x60uy // 0110 0000

type Register =
| R0 = 0x00uy // 0000 0000

let InstructionMask    = 0b11110000uy
let RegisterMask       = 0b00000111uy

type Operand =
| NoOp
| Reg of Register
| Addr of uint16

type InstructionRegister = {
    mutable Data: byte array
    mutable SourceOperand: Operand
    mutable TargetOperand: Operand
}

type Registers = {
    R: uint16 array
    InstructionRegister: InstructionRegister
}

type Cpu = {
    Registers: Registers
}

[<Test>]
let ``Pattern matching can be nested within a switch statement``() = // See #483
    let Execute cpu =
        let _r = cpu.Registers.R
        let _ir = cpu.Registers.InstructionRegister

        let firstOpCode = _ir.Data.[0]
        let instruction = firstOpCode &&& InstructionMask |> EnumOfValue
        let register = firstOpCode &&& RegisterMask

        match instruction with
        | Instruction.Jmp ->
            match _ir.TargetOperand with
            | Addr address -> _r.[7] <- address
            | _ -> ()

        | Instruction.Jsr ->
            match _ir.TargetOperand with
            | Addr address ->
                _r.[6] <- _r.[6] - 2us
                _r.[int register] <- _r.[7]
                _r.[7] <- address
            | _ -> ()

        | _ -> ()
    // Not sure how to test this, just check it compiles
    ()
