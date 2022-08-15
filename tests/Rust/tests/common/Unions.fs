module Common.Unions

type MyUnion =
    | A of int
    | B of string

module MyUnion =
    let createA i =
        A i