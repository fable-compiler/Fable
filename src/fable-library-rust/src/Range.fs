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

let rangeChar (start: char) (stop: char) =
    let intStop = uint stop

    let stepFn c =
        if c <= intStop then
            Some(char c, c + 1u)
        else
            None

    Seq.unfold stepFn (uint start)
