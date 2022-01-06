module Util

let equals (x: 'T) (y: 'T): bool =
    x = y

let compare (x: 'T) (y: 'T): int =
    if x > y then 1
    else
        if x < y then -1
        else 0
