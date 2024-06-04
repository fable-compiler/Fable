// source: https://github.com/fsharp/fsharp
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace BigInt

#if FX_NO_BIGINT
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

/// Abstract internal type
[<NoEquality; NoComparison>]
type internal BigNat

module internal BigNatModule =

    val zero: BigNat
    val one: BigNat
    val two: BigNat

    val add: BigNat -> BigNat -> BigNat
    val sub: BigNat -> BigNat -> BigNat
    val mul: BigNat -> BigNat -> BigNat
    val divmod: BigNat -> BigNat -> BigNat * BigNat
    val div: BigNat -> BigNat -> BigNat
    val rem: BigNat -> BigNat -> BigNat
    val bitAnd: BigNat -> BigNat -> BigNat
    val bitOr: BigNat -> BigNat -> BigNat
    val bitXor: BigNat -> BigNat -> BigNat
    val hcf: BigNat -> BigNat -> BigNat

    val min: BigNat -> BigNat -> BigNat
    val max: BigNat -> BigNat -> BigNat
    val scale: int32 -> BigNat -> BigNat
    val powi: BigNat -> int32 -> BigNat
    val pow: BigNat -> BigNat -> BigNat

    val IsZero: BigNat -> bool
    val isZero: BigNat -> bool
    val isOne: BigNat -> bool
    val equal: BigNat -> BigNat -> bool
    val compare: BigNat -> BigNat -> int32
    val lt: BigNat -> BigNat -> bool
    val gt: BigNat -> BigNat -> bool
    val lte: BigNat -> BigNat -> bool
    val gte: BigNat -> BigNat -> bool

    val hash: BigNat -> int32
    val toFloat: BigNat -> float
    val ofInt32: int32 -> BigNat
    val ofInt64: int64 -> BigNat
    val toString: BigNat -> string
    val ofString: string -> BigNat

    val toUInt32: BigNat -> uint32
    val toUInt64: BigNat -> uint64

    val factorial: BigNat -> BigNat
    // val randomBits : int32 -> BigNat
    val bits: BigNat -> int32
    val isSmall: BigNat -> bool (* will fit in int32 (but not nec all int32) *)
    val getSmall: BigNat -> int32 (* get the value, if it satisfies isSmall *)

#endif
