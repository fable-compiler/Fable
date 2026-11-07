module App

type Derived =
    inherit Library.Base
    val M: int
    new () = { inherit Library.Base(1); M = 2 }
