module Main


[<EntryPoint>]
let main args = // args
    //add2Eq4()
    let a = "hello world"
    ArithmeticTests.testAddition()
    ArithmeticTests.testSubtraction()
    ArithmeticTests.testMultiply()
    ArithmeticTests.testDivide()
    StringTests.testStringConcatWorks()
    RunTests.testGenericMap()
    RunTests.testGenericMapWithClosure()
    // RunTests.testCurriedApply()
    // RunTests.testGenericMap2()
    0
