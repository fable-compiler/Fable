module FSharp.Core.OperatorIntrinsics

let makeRangeStepFunction<'T>
    (step: 'T)
    (stop: 'T)
    (zero: 'T)
    (add: 'T -> 'T -> 'T)
    (compare: 'T -> 'T -> int)
    =
    let stepComparedWithZero = compare step zero

    if stepComparedWithZero = 0 then
        failwith "The step of a range cannot be zero"

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

let integralRangeStep<'T>
    (start: 'T)
    (step: 'T)
    (stop: 'T)
    (zero: 'T)
    (add: 'T -> 'T -> 'T)
    (compare: 'T -> 'T -> int)
    =
    let stepFn = makeRangeStepFunction step stop zero add compare
    Seq.delay (fun () -> Seq.unfold stepFn start)

let rangeDouble start step stop =
    integralRangeStep start step stop 0.0 (+) compare

let rangeInt start step stop =
    integralRangeStep start step stop 0 (+) compare
//let rangeBigInt start step stop = integralRangeStep start step stop 0I (+) compare
//let rangeDecimal start step stop = integralRangeStep start step stop 0m (+) compare

let rangeChar (start: char) (stop: char) =
    let intStop = int stop

    let stepFn c =
        if c <= intStop then
            Some(char c, c + 1)
        else
            None

    Seq.delay (fun () -> Seq.unfold stepFn (int start))
