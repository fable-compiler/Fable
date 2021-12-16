module Fable.Tests.MiscTestsHelper

let counter =
    let mutable i = 0
    fun () ->
        i <- i + 1
        i

type Type = {
    a : int
    b : int
    c : int
    d : int
    e : int
}
  with
    static member New(n) = {
        a = n
        b = n * 2
        c = n * 3
        d = n * 4
        e = counter()  // <== should only be called twice
      }

    member        this.Method  (v:bool) = { this with a = this.a * if v then 2 else 3 }
    member inline this.MethodI (v:bool) = { this with a = this.a * if v then 2 else 3 }
    member        this.Method  ()       = { this with a = this.a * 10 }
    member inline this.MethodI ()       = { this with a = this.a * 10 }
