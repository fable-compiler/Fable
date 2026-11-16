module Range_

[<CompiledName("rangeNumeric")>]
let inline rangeNumeric (start: 'T) (step: 'T) (stop: 'T) =
    let zero = LanguagePrimitives.GenericZero
    let stepComparedWithZero = compare step zero

    if stepComparedWithZero = 0 then
        failwith "The step of a range cannot be zero"

    let stepFn x =
        let comparedWithLast = compare x stop

        if
            (stepComparedWithZero > 0 && comparedWithLast <= 0)
            || (stepComparedWithZero < 0 && comparedWithLast >= 0)
        then
            Some(x, x + step)
        else
            None

    Seq.unfold stepFn start

let inline private floatingRangeStep (start: 'T) (step: 'T) (stop: 'T) : seq<'T> =
    let zero: 'T = LanguagePrimitives.GenericZero

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

        if (up && step < zero) || (not up && step > zero) then
            Seq.empty
        else
            let stepFn (x: 'T voption) =
                match x with
                | ValueNone -> None
                | ValueSome x ->
                    let next = x + step

                    let isBefore =
                        if up then
                            x < next && next < stop
                        else
                            x > next && next > stop

                    if next <> x && (isBefore || next = stop) then
                        Some(x, ValueSome next)
                    else
                        Some(x, ValueNone)

            Seq.unfold stepFn (ValueSome start)

let inline private floatingRange ([<InlineIfLambda>] ofInt64: int64 -> 'T) (start: 'T) (step: 'T) (stop: 'T) : seq<'T> =
    Seq.delay (fun () ->
        let zero: 'T = LanguagePrimitives.GenericZero

        if step = zero then
            invalidArg "step" "The step of a range cannot be zero."

        let numSteps = float ((stop - start) / step)

        if
            -2147483648.0 <= numSteps
            && numSteps <= 2147483647.0
            && System.Math.Floor numSteps = numSteps
        then
            let stepFn (i: int64) =
                if float i <= numSteps then
                    Some(start + ofInt64 i * step, i + 1L)
                else
                    None

            Seq.unfold stepFn 0L
        else
            floatingRangeStep start step stop
    )

let rangeFloat (start: float) (step: float) (stop: float) : seq<float> = floatingRange float start step stop

let rangeSingle (start: float32) (step: float32) (stop: float32) : seq<float32> = floatingRange float32 start step stop

let rangeChar (start: char) (stop: char) =
    let intStop = uint stop

    let stepFn i =
        if i <= intStop then
            Some(char i, i + 1u)
        else
            None

    Seq.unfold stepFn (uint start)
