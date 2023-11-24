module Random

open System

module private Native =
    open Fable.Core
    open Fable.Core.JsInterop

    let random () = JS.Math.random ()

    let randomNext (min: int, max: int) : int =
        emitJsStatement
            ()
            """
        if (max < min) {
            throw new Error("minValue must be less than maxValue");
        }
        return Math.floor(Math.random() * (max - min)) + min
        """

    let randomBytes (buffer: byte array) : unit =
        emitJsStatement
            ()
            """
        if (buffer == null) {
            throw new Error("Buffer cannot be null");
        }
        for (let i = 0; i < buffer.length; i += 6) {
            // Pick random 48-bit number. Fill buffer in 2 24-bit chunks to avoid bitwise truncation.
            let r = Math.floor(Math.random() * 281474976710656); // Low 24 bits = chunk 1.
            const rhi = Math.floor(r / 16777216); // High 24 bits shifted via division = chunk 2.
            for (let j = 0; j < 6 && i + j < buffer.length; j++) {
                if (j === 3) { r = rhi; }
                buffer[i + j] = r & 255;
                r >>>= 8;
            }
        }"""

type IRandom =
    // Avoid overloads
    abstract Next0: unit -> int
    abstract Next1: maxValue: int -> int
    abstract Next2: minValue: int * maxValue: int -> int
    abstract NextDouble: unit -> float
    abstract NextBytes: buffer: byte array -> unit

// Lightweight version of System.Random that just defers to native random for when we don't need
// seeded values. This avoids bringing dependencies like the Long module, see #2688
type NonSeeded() =
    interface IRandom with
        member _.Next0() = Native.randomNext (0, Int32.MaxValue)
        member _.Next1(maxValue) = Native.randomNext (0, maxValue)

        member _.Next2(minValue, maxValue) =
            Native.randomNext (minValue, maxValue)

        member _.NextDouble() = Native.random ()
        member _.NextBytes(buffer) = Native.randomBytes (buffer)

// Port of System.Random, see https://github.com/fable-compiler/Fable/issues/2688#issuecomment-1003752599
type Seeded(seed: int) =
    let MBIG = Int32.MaxValue
    let MSEED = 161803398
    let MZ = 0

    let mutable inext = 0
    let mutable inextp = 0
    let mutable seedArray = Array.zeroCreate 56

    do
        let mutable ii = 0
        let mutable mj = 0
        let mutable mk = 0

        let subtraction =
            if seed = Int32.MinValue then
                Int32.MaxValue
            else
                abs seed

        mj <- MSEED - subtraction

        seedArray.[55] <- mj

        mk <- 1

        for i = 1 to 54 do
            ii <- (21 * i) % 55
            seedArray.[ii] <- mk
            mk <- mj - mk

            if mk < 0 then
                mk <- mk + MBIG

            mj <- seedArray.[ii]

        for k = 1 to 4 do
            for i = 1 to 55 do
                seedArray.[i] <- seedArray.[i] - seedArray.[1 + (i + 30) % 55]

                if seedArray.[i] < 0 then
                    seedArray.[i] <- seedArray.[i] + MBIG

        inext <- 0
        inextp <- 21

    member private _.InternalSample() =
        let mutable retVal = 0
        let mutable locINext = inext
        let mutable locINextp = inextp

        locINext <- locINext + 1

        if locINext >= 56 then
            locINext <- 1

        locINextp <- locINextp + 1

        if locINextp >= 56 then
            locINextp <- 1

        retVal <- seedArray.[locINext] - seedArray.[locINextp]

        if retVal = MBIG then
            retVal <- retVal - 1

        if retVal < 0 then
            retVal <- retVal + MBIG

        seedArray.[locINext] <- retVal

        inext <- locINext
        inextp <- locINextp

        retVal

    member this.Sample() =
        float (this.InternalSample()) * (1.0 / float MBIG)

    member this.GetSampleForLargeRange() =
        let mutable result = float (this.InternalSample())

        let negative = this.InternalSample() % 2 = 0

        if negative then
            result <- -result

        let mutable d = result
        d <- d + float (Int32.MaxValue - 1)
        d <- d / (2.0 * float (uint (Int32.MaxValue - 1)))
        d

    interface IRandom with
        member this.Next0() = this.InternalSample()

        member this.Next1(maxValue: int) =
            if maxValue < 0 then
                raise
                <| ArgumentOutOfRangeException("maxValue must be positive")

            int (this.Sample() * float maxValue)

        member this.Next2(minValue: int, maxValue: int) =
            if minValue > maxValue then
                raise
                <| ArgumentOutOfRangeException(
                    "minValue must be less than maxValue"
                )

            let range = int64 (maxValue - minValue)

            if range <= int64 Int32.MaxValue then
                int (int (this.Sample() * float range) + minValue)
            else
                int (
                    int64 (this.GetSampleForLargeRange() * float range)
                    + int64 minValue
                )

        member this.NextDouble() = this.Sample()

        member this.NextBytes(buffer: byte array) =
            if isNull buffer then
                raise <| ArgumentNullException("buffer")

            for i = 0 to buffer.Length - 1 do
                buffer.[i] <-
                    byte (
                        (int (this.InternalSample())) % (int Byte.MaxValue + 1)
                    )

let nonSeeded () = NonSeeded()
let seeded seed = Seeded(seed)
