/// Same file name as Naming/Second/Types.fs. Both used to compile to `types.erl`: the one
/// compiled last overwrote the other on disk, so half of these declarations vanished from the
/// output entirely.
module Fable.Tests.Naming.First.Types

type Shape =
    | Circle of float
    | Square of float

let area =
    function
    | Circle r -> 3.0 * r * r
    | Square s -> s * s
