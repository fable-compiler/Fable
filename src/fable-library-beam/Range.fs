module FSharp.Core.OperatorIntrinsics

let makeRangeStepFunction<'T when 'T: comparison> (step: 'T) (stop: 'T) (zero: 'T) (add: 'T -> 'T -> 'T) =
    let stepComparedWithZero = compare step zero

    if stepComparedWithZero = 0 then
        invalidArg "step" "The step of a range cannot be zero."

    let stepGreaterThanZero = stepComparedWithZero > 0

    fun x ->
        let comparedWithLast = compare x stop

        if
            (stepGreaterThanZero && comparedWithLast <= 0)
            || (not stepGreaterThanZero && comparedWithLast >= 0)
        then
            Some(x, add x step)
        else
            None

let integralRangeStep<'T when 'T: comparison> (start: 'T) (step: 'T) (stop: 'T) (zero: 'T) (add: 'T -> 'T -> 'T) =
    let stepFn = makeRangeStepFunction step stop zero add
    Seq.delay (fun () -> Seq.unfold stepFn start)

let rangeDecimal start step stop =
    integralRangeStep start step stop 0m (+)

let rangeDouble start step stop =
    integralRangeStep start step stop 0.0 (+)

[<Fable.Core.Emit("($0 =:= nan)")>]
let private isNaN (x: float) : bool = Fable.Core.Util.nativeOnly

[<Fable.Core.Emit("fable_utils:round_single($0)")>]
let private roundSingle (x: float) : float = Fable.Core.Util.nativeOnly

// Float overflow raises badarith on Beam instead of giving infinity.
let inline private orOnOverflow (fallback: float) ([<InlineIfLambda>] f: unit -> float) : float =
    try
        f ()
    with _ ->
        fallback

let private floatingRangeStep (round: float -> float) (start: float) (step: float) (stop: float) =
    if start = stop then
        Seq.singleton start
    else
        let up = start < stop

        if (up && step < 0.0) || (not up && step > 0.0) then
            Seq.empty
        else
            let limit =
                if step > 0.0 then
                    System.Double.PositiveInfinity
                else
                    System.Double.NegativeInfinity

            let before (a: float) (b: float) =
                if up then
                    a < b
                else
                    a > b

            let stepFn (x: float voption) =
                match x with
                | ValueNone -> None
                | ValueSome x ->
                    let next = orOnOverflow limit (fun () -> round (x + step))

                    if next <> x && ((before x next && before next stop) || next = stop) then
                        Some(x, ValueSome next)
                    else
                        Some(x, ValueNone)

            Seq.unfold stepFn (ValueSome start)

let private floatingRange (round: float -> float) (start: float) (step: float) (stop: float) =
    Seq.delay (fun () ->
        if isNaN start then
            invalidArg "n" "The start of a range cannot be NaN."

        if isNaN step then
            invalidArg "step" "The step of a range cannot be NaN."

        if isNaN stop then
            invalidArg "m" "The end of a range cannot be NaN."

        if not (step < 0.0 || step > 0.0) then
            invalidArg "step" "The step of a range cannot be zero."

        let numSteps =
            orOnOverflow System.Double.PositiveInfinity (fun () -> round (round (stop - start) / step))

        if
            -2147483648.0 <= numSteps
            && numSteps <= 2147483647.0
            && System.Math.Floor numSteps = numSteps
        then
            let stepFn (i: float) =
                if i <= numSteps then
                    Some(round (start + round (round i * step)), i + 1.0)
                else
                    None

            Seq.unfold stepFn 0.0
        else
            floatingRangeStep round start step stop
    )

let rangeFloat (start: float) (step: float) (stop: float) : seq<float> = floatingRange id start step stop

let rangeSingle (start: float32) (step: float32) (stop: float32) : seq<float32> =
    floatingRange roundSingle (float start) (float step) (float stop)
    |> Seq.map float32

let rangeInt32 start step stop = integralRangeStep start step stop 0 (+)

let rangeUInt32 start step stop =
    integralRangeStep start step stop 0u (+)

let rangeInt64 start step stop =
    integralRangeStep start step stop 0L (+)

let rangeUInt64 start step stop =
    integralRangeStep start step stop 0UL (+)

let rangeBigInt start step stop =
    integralRangeStep start step stop 0I (+)

let rangeChar (start: char) (stop: char) =
    let intStop = int stop

    let stepFn i =
        if i <= intStop then
            Some(char i, i + 1)
        else
            None

    Seq.delay (fun () -> Seq.unfold stepFn (int start))
