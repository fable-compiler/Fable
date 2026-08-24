module Library

type IHelpers =
    abstract Wrap: string -> string

type IGreeting =
    abstract Greet: IHelpers -> string

/// Regression shape: the object expression's member takes another type of this library
let inline greeting (name: string) =
    { new IGreeting with
        member _.Greet(helpers) = helpers.Wrap name
    }

let inline double (value: int) = value * 2

let plain (value: int) = value + 1
