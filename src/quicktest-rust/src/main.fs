open System
open System.Collections.Generic

let genericHash<'T when 'T: equality> (x: 'T) : int =
    let cmp = EqualityComparer<'T>.Default
    cmp.GetHashCode(x)

[<EntryPoint>]
let main argv =
    Console.WriteLine("Hello {0}!", "World")

    let o1 = [| 1; 2 |]
    let o2 = [| 1; 2 |]
    let o3 = [| 2; 1 |]
    let h1 = genericHash o1
    let h2 = genericHash o2
    let h3 = genericHash o3

    Console.WriteLine(" h1 = {0}\n h2 = {1}\n h3 = {2}", h1, h2, h3)

    0
