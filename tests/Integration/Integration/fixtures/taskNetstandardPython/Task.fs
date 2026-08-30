module TaskRegression

open System.Threading.Tasks

let run shouldThrow : Task<Result<int, exn>> =
    task {
        let! value = Task.FromResult 42

        try
            if shouldThrow then
                failwith "boom"

            return Ok value
        with error ->
            return Error error
    }
