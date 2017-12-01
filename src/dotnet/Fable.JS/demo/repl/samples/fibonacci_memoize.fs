let fastFib n =
  let sqrt5 = sqrt 5.0
  let a = (1.0 + sqrt5) / 2.0
  let b = (1.0 - sqrt5) / 2.0
  let power = float n
  let result = ((a ** power) - (b ** power)) / sqrt5
  int (round result)

let rec slowFib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> slowFib (n - 1) + slowFib (n - 2)

let memoFib =
    let dict = new System.Collections.Generic.Dictionary<int, int>()
    fun n ->
      if dict.ContainsKey n then dict.[n]
      else
        let result = slowFib n
        dict.Add(n, result)
        result

let seq_Fib n =
    let rec fib_seq = seq {
        yield! [0; 1]
        yield! fib_seq
            |> Seq.pairwise
            |> Seq.map (fun (prev, next) -> prev + next)
    }
    fib_seq |> Seq.item n

let measure f x =
    let dtStart = System.DateTime.UtcNow
    let result = f x
    let elapsed = System.DateTime.UtcNow - dtStart
    result, elapsed.TotalSeconds

let cw fname f n =
    let result, elapsed = measure f n
    printfn "%s(%d) = %d, elapsed: %.3f s" fname n result elapsed

let n = 35;
cw "fastFib" fastFib n
cw "slowFib" slowFib n
cw "memoFib" memoFib n
cw "seq_Fib" seq_Fib n
