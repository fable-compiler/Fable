module Library

let registered = ResizeArray<obj>()

type Base =
    val N: int
    new (n: int) as self = { N = n } then registered.Add(box self)
