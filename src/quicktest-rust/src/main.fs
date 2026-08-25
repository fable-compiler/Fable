open System

type RefC(v: int) =
    member _.V = v

let twoSwitchRef (x: RefC option) (b: bool) =
    match x with
    | Some c when b -> c.V + 1
    | Some c -> c.V
    | None -> -1

type RefU =
    | RA of RefC
    | RB of RefC

let orPatRef (u: RefU) =
    match u with
    | RA c
    | RB c -> c.V

type ObjU =
    | OA of obj
    | OB of obj

let orPatObj (u: ObjU) : obj =
    match u with
    | OA o
    | OB o -> o

[<EntryPoint>]
let main argv =
    Console.WriteLine(twoSwitchRef (Some(RefC 5)) true)
    Console.WriteLine(twoSwitchRef (Some(RefC 5)) false)
    Console.WriteLine(twoSwitchRef None false)
    Console.WriteLine(orPatRef (RA(RefC 5)))
    Console.WriteLine(orPatRef (RB(RefC 7)))
    Console.WriteLine((orPatObj (OA(box 9))) :? int)
    Console.WriteLine((orPatObj (OB(box 11))) :? string)
    0
