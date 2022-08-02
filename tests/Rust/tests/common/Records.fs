module Common.Records

//This deliberately has the same name as in RecordsTests to ensure a collision
type MyRecord = {
    a: int
}

module MyRecord = 
    let create a = { a = a }