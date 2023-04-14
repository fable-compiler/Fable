module ArithmeticTests
open Util

let testAddition () =
    let res = 2 + 2
    assertTrue(res = 4)
    1

let testSubtraction () =
    let res = 4 - 1
    assertTrue(res = 3)
    1

let testMultiply () =
    let res = 2 * 3
    assertTrue(res = 6)
    1

let testDivide () =
    let res = 10 / 2
    assertTrue(res = 5)
    1