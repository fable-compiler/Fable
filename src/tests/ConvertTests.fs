[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Convert
open System
open NUnit.Framework
open Fable.Tests.Util

[<Test>]
let ``System.Convert.ToSByte works``() =
    let x = 1y
    sbyte(1y) |> equal x
    sbyte(1uy) |> equal x
    sbyte(257s) |> equal x
    sbyte(257) |> equal x
    sbyte(257L) |> equal x
    sbyte(257u) |> equal x
    sbyte(257us) |> equal x
    sbyte(257ul) |> equal x
    sbyte(257uL) |> equal x
    Convert.ToSByte(1y) |> equal x
    Convert.ToSByte(1uy) |> equal x
    Convert.ToSByte(1s) |> equal x
    Convert.ToSByte(1) |> equal x
    Convert.ToSByte(1L) |> equal x
    Convert.ToSByte(1u) |> equal x
    Convert.ToSByte(1us) |> equal x
    Convert.ToSByte(1ul) |> equal x
    Convert.ToSByte(1uL) |> equal x

[<Test>]
let ``System.Convert.ToInt16 works``() =
    let x = 1s
    int16(1y) |> equal x
    int16(1uy) |> equal x
    int16(1s) |> equal x
    int16(65537) |> equal x
    int16(65537L) |> equal x
    int16(65537u) |> equal x
    int16(1us) |> equal x
    int16(65537ul) |> equal x
    int16(65537uL) |> equal x
    Convert.ToInt16(1y) |> equal x
    Convert.ToInt16(1uy) |> equal x
    Convert.ToInt16(1s) |> equal x
    Convert.ToInt16(1) |> equal x
    Convert.ToInt16(1L) |> equal x
    Convert.ToInt16(1u) |> equal x
    Convert.ToInt16(1us) |> equal x
    Convert.ToInt16(1ul) |> equal x
    Convert.ToInt16(1uL) |> equal x

[<Test>]
let ``System.Convert.ToInt32 works``() =
    let x = 1
    int(1y) |> equal x
    int(1uy) |> equal x
    int(1s) |> equal x
    int(1) |> equal x
    int(1L) |> equal x
    int(1u) |> equal x
    int(1us) |> equal x
    int(1ul) |> equal x
    int(1uL) |> equal x
    Convert.ToInt32(1y) |> equal x
    Convert.ToInt32(1uy) |> equal x
    Convert.ToInt32(1s) |> equal x
    Convert.ToInt32(1) |> equal x
    Convert.ToInt32(1L) |> equal x
    Convert.ToInt32(1u) |> equal x
    Convert.ToInt32(1us) |> equal x
    Convert.ToInt32(1ul) |> equal x
    Convert.ToInt32(1uL) |> equal x

[<Test>]
let ``System.Convert.ToInt64 works``() =
    let x = 1L
    int64(1y) |> equal x
    int64(1uy) |> equal x
    int64(1s) |> equal x
    int64(1) |> equal x
    int64(1L) |> equal x
    int64(1u) |> equal x
    int64(1us) |> equal x
    int64(1ul) |> equal x
    int64(1uL) |> equal x
    Convert.ToInt64(1y) |> equal x
    Convert.ToInt64(1uy) |> equal x
    Convert.ToInt64(1s) |> equal x
    Convert.ToInt64(1) |> equal x
    Convert.ToInt64(1L) |> equal x
    Convert.ToInt64(1u) |> equal x
    Convert.ToInt64(1us) |> equal x
    Convert.ToInt64(1ul) |> equal x
    Convert.ToInt64(1uL) |> equal x

[<Test>]
let ``System.Convert.ToByte works``() =
    let x = 1uy
    byte(1y) |> equal x
    byte(1uy) |> equal x
    byte(257s) |> equal x
    byte(257) |> equal x
    byte(257L) |> equal x
    byte(257u) |> equal x
    byte(257us) |> equal x
    byte(257ul) |> equal x
    byte(257uL) |> equal x
    Convert.ToByte(1y) |> equal x
    Convert.ToByte(1uy) |> equal x
    Convert.ToByte(1s) |> equal x
    Convert.ToByte(1) |> equal x
    Convert.ToByte(1L) |> equal x
    Convert.ToByte(1u) |> equal x
    Convert.ToByte(1us) |> equal x
    Convert.ToByte(1ul) |> equal x
    Convert.ToByte(1uL) |> equal x

[<Test>]
let ``System.Convert.ToUInt16 works``() =
    let x = 1us
    uint16(1y) |> equal x
    uint16(1uy) |> equal x
    uint16(1s) |> equal x
    uint16(65537) |> equal x
    uint16(65537L) |> equal x
    uint16(65537u) |> equal x
    uint16(1us) |> equal x
    uint16(65537ul) |> equal x
    uint16(65537uL) |> equal x
    Convert.ToUInt16(1y) |> equal x
    Convert.ToUInt16(1uy) |> equal x
    Convert.ToUInt16(1s) |> equal x
    Convert.ToUInt16(1) |> equal x
    Convert.ToUInt16(1L) |> equal x
    Convert.ToUInt16(1u) |> equal x
    Convert.ToUInt16(1us) |> equal x
    Convert.ToUInt16(1ul) |> equal x
    Convert.ToUInt16(1uL) |> equal x

[<Test>]
let ``System.Convert.ToUInt32 works``() =
    let x = 1u
    uint32(1y) |> equal x
    uint32(1uy) |> equal x
    uint32(1s) |> equal x
    uint32(1) |> equal x
    uint32(1L) |> equal x
    uint32(1u) |> equal x
    uint32(1us) |> equal x
    uint32(1ul) |> equal x
    uint32(1uL) |> equal x
    Convert.ToUInt32(1y) |> equal x
    Convert.ToUInt32(1uy) |> equal x
    Convert.ToUInt32(1s) |> equal x
    Convert.ToUInt32(1) |> equal x
    Convert.ToUInt32(1L) |> equal x
    Convert.ToUInt32(1u) |> equal x
    Convert.ToUInt32(1us) |> equal x
    Convert.ToUInt32(1ul) |> equal x
    Convert.ToUInt32(1uL) |> equal x

[<Test>]
let ``System.Convert.ToUInt64 works``() =
    let x = 1uL
    uint64(1y) |> equal x
    uint64(1uy) |> equal x
    uint64(1s) |> equal x
    uint64(1) |> equal x
    uint64(1L) |> equal x
    uint64(1u) |> equal x
    uint64(1us) |> equal x
    uint64(1ul) |> equal x
    uint64(1uL) |> equal x
    Convert.ToUInt64(1y) |> equal x
    Convert.ToUInt64(1uy) |> equal x
    Convert.ToUInt64(1s) |> equal x
    Convert.ToUInt64(1) |> equal x
    Convert.ToUInt64(1L) |> equal x
    Convert.ToUInt64(1u) |> equal x
    Convert.ToUInt64(1us) |> equal x
    Convert.ToUInt64(1ul) |> equal x
    Convert.ToUInt64(1uL) |> equal x

[<Test>]
let ``System.Convert.ToSingle works``() =
    let x = 1.f
    float32(1y) |> equal x
    float32(1uy) |> equal x
    float32(1s) |> equal x
    float32(1) |> equal x
    float32(1L) |> equal x
    float32(1u) |> equal x
    float32(1us) |> equal x
    float32(1ul) |> equal x
    float32(1uL) |> equal x
    Convert.ToSingle(1y) |> equal x
    Convert.ToSingle(1uy) |> equal x
    Convert.ToSingle(1s) |> equal x
    Convert.ToSingle(1) |> equal x
    Convert.ToSingle(1L) |> equal x
    Convert.ToSingle(1u) |> equal x
    Convert.ToSingle(1us) |> equal x
    Convert.ToSingle(1ul) |> equal x
    Convert.ToSingle(1uL) |> equal x

[<Test>]
let ``System.Convert.ToDouble works``() =
    let x = 1.
    float(1y) |> equal x
    float(1uy) |> equal x
    float(1s) |> equal x
    float(1) |> equal x
    float(1L) |> equal x
    float(1u) |> equal x
    float(1us) |> equal x
    float(1ul) |> equal x
    float(1uL) |> equal x
    Convert.ToDouble(1y) |> equal x
    Convert.ToDouble(1uy) |> equal x
    Convert.ToDouble(1s) |> equal x
    Convert.ToDouble(1) |> equal x
    Convert.ToDouble(1L) |> equal x
    Convert.ToDouble(1u) |> equal x
    Convert.ToDouble(1us) |> equal x
    Convert.ToDouble(1ul) |> equal x
    Convert.ToDouble(1uL) |> equal x
