open System.Collections.Generic

let fastFib n =
  let sqrt5 = sqrt 5.0
  let a = (1.0 + sqrt5) / 2.0
  let b = (1.0 - sqrt5) / 2.0
  let power = float (n+1)
  let result = ((a ** power) - (b ** power)) / sqrt5
  int (round result)

let rec slowFib n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | _ -> slowFib (n - 1) + slowFib (n - 2)

let memoizedFibonnaci =
    let dict = new Dictionary<int, int>()
    fun n ->
      if dict.ContainsKey n then dict.[n]
      else
        let result = slowFib n
        dict.Add(n, result)
        result

[1..30]
|> List.map memoizedFibonnaci
|> List.iter (printfn "%d")