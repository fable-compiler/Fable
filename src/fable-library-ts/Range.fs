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

let inline private integerRange (start: ^T) (step: ^T) (stop: ^T) : seq< ^T > =
    let zero: ^T = LanguagePrimitives.GenericZero

    if step = zero then
        invalidArg "step" "The step of a range cannot be zero."

    let up = step > zero

    // No operation here leaves the range of ^T: stop - x is only computed when x has the sign of the step, x + step when it has not.
    let hasNext (x: ^T) =
        if up then
            if x >= zero then
                stop - x >= step
            else
                x + step <= stop
        else if x <= zero then
            stop - x <= step
        else
            x + step >= stop

    let stepFn (x: ^T voption) =
        match x with
        | ValueNone -> None
        | ValueSome x ->
            if hasNext x then
                Some(x, ValueSome(x + step))
            else
                Some(x, ValueNone)

    if (up && start <= stop) || (not up && start >= stop) then
        Seq.unfold stepFn (ValueSome start)
    else
        Seq.empty

let rangeBigInt start step stop =
    integralRangeStep start step stop 0I (+)

let rangeDecimal start step stop =
    integralRangeStep start step stop 0m (+)

let private floatingRangeStep (round: float -> float) (start: float) (step: float) (stop: float) =
    if start <> start then
        invalidArg "n" "The start of a range cannot be NaN."

    if step <> step then
        invalidArg "step" "The step of a range cannot be NaN."

    if stop <> stop then
        invalidArg "m" "The end of a range cannot be NaN."

    if start = stop then
        Seq.singleton start
    else
        let up = start < stop

        if (up && step < 0.0) || (not up && step > 0.0) then
            Seq.empty
        else
            let before (a: float) (b: float) =
                if up then
                    a < b
                else
                    a > b

            let stepFn (x: float voption) =
                match x with
                | ValueNone -> None
                | ValueSome x ->
                    let next = round (x + step)

                    if next <> x && ((before x next && before next stop) || next = stop) then
                        Some(x, ValueSome next)
                    else
                        Some(x, ValueNone)

            Seq.unfold stepFn (ValueSome start)

let private floatingRange (round: float -> float) (start: float) (step: float) (stop: float) =
    Seq.delay (fun () ->
        if step = 0.0 then
            invalidArg "step" "The step of a range cannot be zero."

        let numSteps = round (round (stop - start) / step)

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

let rangeDouble start step stop =
    integralRangeStep start step stop 0.0 (+)

let rangeFloat (start: float) (step: float) (stop: float) : seq<float> = floatingRange id start step stop

let rangeSingle (start: float32) (step: float32) (stop: float32) : seq<float32> =
    floatingRange (fun x -> float (float32 x)) (float start) (float step) (float stop)
    |> unbox

let rangeInt64 (start: int64) (step: int64) (stop: int64) : seq<int64> = integerRange start step stop

let rangeUInt64 (start: uint64) (step: uint64) (stop: uint64) : seq<uint64> = integerRange start step stop

let rangeChar (start: char) (stop: char) =
    let intStop = int stop

    let stepFn i =
        if i <= intStop then
            Some(char i, i + 1)
        else
            None

    Seq.delay (fun () -> Seq.unfold stepFn (int start))
