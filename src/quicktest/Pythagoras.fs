module QuickTest

let add(a, b, cont) =
    cont(a + b)

let square(x, cont) =
    cont(x * x)

type Math () =
    let a = 10

let sqrt(x, cont) =
    cont(sqrt(x))

let pythagoras(a, b, cont) =
    square(a, (fun aa ->
        printfn "test"
        square(b, (fun bb ->
            printfn "test"
            add(aa, bb, (fun aabb ->
                printfn "test"
                sqrt(aabb, (fun result ->
                    printfn "test"
                    cont(result)
                ))
            ))
        ))
    ))

