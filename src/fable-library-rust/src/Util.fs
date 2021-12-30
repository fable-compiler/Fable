module Util

let equals<'T when 'T: equality> (x: 'T) (y: 'T) =
    x = y

let compare<'T when 'T: comparison> (x: 'T) (y: 'T) =
    if x = y then 0
    else if x < y then -1 else 1
