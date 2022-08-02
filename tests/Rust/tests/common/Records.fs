module Common.Records

type ImportTestRecord = {
    a: int
}

module ImportTestRecord = 
    let create a = { a = a }