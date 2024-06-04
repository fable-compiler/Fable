namespace System

type SystemException() =
    inherit Exception()

type TimeoutException() =
    inherit SystemException()
