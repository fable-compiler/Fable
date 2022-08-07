namespace System

type Exception(msg: string) =
    member _.Message = msg

// type SystemException() =
//     inherit Exception()

// type TimeoutException() =
//     inherit SystemException()
